+++
title = "Owned values and Futures in Rust"
date = 2023-09-10
+++
Let's write a simple `tokio`-powered program that will download the contents of an HTTP response body using `reqwest` and print it to stdout. We'll take the URL to download on the command line using `clap`. This might look something like the following:

```rust
use anyhow::Result;
use clap::Parser;

#[derive(clap::Parser)]
struct Opt {
    url: String,
}

#[tokio::main]
async fn main() -> Result<()> {
    let Opt { url } = Opt::parse();
    let body = reqwest::get(url).await?.text().await?;
    println!("{body}");
    Ok(())
}
```

All good, but let's (arguably) improve our program by extracting the logic for the download-and-print to a helper function:

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let Opt { url } = Opt::parse();
    download_and_print(&url).await?;
    Ok(())
}

async fn download_and_print(url: &str) -> Result<()> {
    let body = reqwest::get(url).await?.text().await?;
    println!("{body}");
    Ok(())
}
```

I've followed general best practices here and taken the `url` as a string slice instead of an owned string. Now, it's really easy to extend this program to support multiple URLs:

```rust
#[derive(clap::Parser)]
struct Opt {
    urls: Vec<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let Opt { urls } = Opt::parse();
    for url in urls {
        download_and_print(&url).await?;
    }
    Ok(())
}
```

But now, let's kick it up a notch and introduce some parallelism. We're going to use a `JoinSet` to allow us to spawn off a separate task per URL provided and wait on all of them returning. If anything fails along the way, we'll exit the entire program and abort ongoing activities.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let Opt { urls } = Opt::parse();
    let mut set = tokio::task::JoinSet::new();

    for url in urls {
        set.spawn(download_and_print(&url));
    }

    while let Some(result) = set.join_next().await {
        match result {
            Ok(Ok(())) => (),
            Ok(Err(e)) => {
                set.abort_all();
                return Err(e);
            }
            Err(e) => {
                set.abort_all();
                return Err(e.into());
            }
        }
    }
    Ok(())
}
```

While the parallelism going on here is OK, the spawning of the new tasks themselves fails:

```
error[E0597]: `url` does not live long enough
  --> src/main.rs:15:38
   |
14 |     for url in urls {
   |         --- binding `url` declared here
15 |         set.spawn(download_and_print(&url));
   |                   -------------------^^^^-
   |                   |                  |
   |                   |                  borrowed value does not live long enough
   |                   argument requires that `url` is borrowed for `'static`
16 |     }
   |     - `url` dropped here while still borrowed
```

This is a common failure mode in async (and, for that matter, multithreaded) Rust. The issue is that the `String` we want to pass is owned by the `main` task, and we're trying to pass a reference to it with no guarantee that the `main` task will outlive the child task. You might argue that the `main` task will _always_ outlive all other tasks, but (1) there's no static proof of that within the code, and (2) it's entirely possible to slightly refactor this program so that the spawning occurs in a subtask instead.

The question is: how do you fix this compile time error? We'll explore a few options.

## Take a String

Arguably the simplest solution is to change the type of the `download_and_print` function so that it takes an owned `String` instead of a reference:

```rust
async fn download_and_print(url: String) -> Result<()> {
    let body = reqwest::get(url).await?.text().await?;
    println!("{body}");
    Ok(())
}
```

Now, at the call site, we're no longer borrowing a reference to the main task's `String`. Instead, we pass in the entire owned value, transferring ownership to the newly spawned task:

```rust
for url in urls {
    // Note the lack of & here!
    set.spawn(download_and_print(url));
}
```

On the one hand, this feels dirty. We're violating best practices and taking an owned `String` where one isn't needed. However, this may be considered a small price to pay for the code simply working. However, if the `download_and_print` function will be used in other parts of the code base where passing a reference will work fine, forcing an owned `String` will cause an unnecessary allocation for those use cases, and we may want to look for a better solution.

## Adjust the callsite with async move

Another possibility is to leave our `download_and_print` function as-is taking a reference, and modify our call site as follows:

```rust
for url in urls {
    set.spawn(async move { download_and_print(&url).await });
}
```

By introducing an `async move` block, what we've done is created a new `Future` that will be passed to `set.spawn`. That new `Future` itself owns the `String`, not the main task. Therefore, borrowing a reference to `url` and passing it to `download_and_print` works just fine.

This is a great solution when you're using a library function that you cannot modify, or when most of your code doesn't run into this lifetime issue. But it can be a bit tedious to have to rewrite code in this way.

## impl AsRef

Our final approach today will be to modify the function to accept a more general `url` type:

```rust
async fn download_and_print(url: impl AsRef<str>) -> Result<()> {
    let body = reqwest::get(url.as_ref()).await?.text().await?;
    println!("{body}");
    Ok(())
}
```

This type means "I'll accept anything that can be converted into a `&str`." This will work for an owned `String` as well as a string slice, leaving the decision entirely to the caller. If we leave our call site as passing in a reference, we'll still get the lifetime error above. But if instead we pass in `url` directly, our program once again works.

This is the approach I'd probably recommend in general. It takes a bit of practice to get used to these `impl AsRef` parameters, but the payoff is worth it in my opinion.

## Improvements

The code above is _not_ perfect. I'm sure others will find other limitations, but two things that jump out at me are:

1. Instead of using `reqwest::get`, we should be creating a single `reqwest::Client` and sharing it throughout the application.
2. For a large number of incoming URLs, we wouldn't want to spawn a separate task per URL, but instead have a fixed number of workers and have them all pop work items from a shared queue. This would help with avoiding rate limiting from servers and from overwhelming our application. But the number of URLs we'd have to be requesting would need to be pretty high to run into either of these issues in practice.

Fortunately, both of these are relatively easy to implement thanks to the simplicity of the `JoinSet` API:

```rust
use anyhow::Result;
use async_channel::Receiver;
use clap::Parser;

#[derive(clap::Parser)]
struct Opt {
    urls: Vec<String>,
    #[clap(long, default_value_t = 8)]
    workers: usize,
}

#[tokio::main]
async fn main() -> Result<()> {
    let Opt { urls, workers } = Opt::parse();
    let mut set = tokio::task::JoinSet::new();
    let client = reqwest::Client::new();
    let (send, recv) = async_channel::bounded(workers * 2);

    set.spawn(async move {
        for url in urls {
            send.send(url).await?;
        }
        Ok(())
    });

    for _ in 0..workers {
        set.spawn(worker(client.clone(), recv.clone()));
    }

    while let Some(result) = set.join_next().await {
        match result {
            Ok(Ok(())) => (),
            Ok(Err(e)) => {
                set.abort_all();
                return Err(e);
            }
            Err(e) => {
                set.abort_all();
                return Err(e.into());
            }
        }
    }
    Ok(())
}

async fn worker(client: reqwest::Client, recv: Receiver<String>) -> Result<()> {
    while let Ok(url) = recv.recv().await {
        download_and_print(&client, &url).await?;
    }
    Ok(())
}

async fn download_and_print(client: &reqwest::Client, url: impl AsRef<str>) -> Result<()> {
    let body = client.get(url.as_ref()).send().await?.text().await?;
    println!("{body}");
    Ok(())
}
```
