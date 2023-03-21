## Intro to Rust

* Michael Snoyman
* VP of Software Development, FP Complete
* Func Prog Sweden 2023

---

## My quick background

* Haskell developer for 15 years
* Rust developer for 6
* Co-author of Begin Rust
* Wrote the Rust Crash Course
* Regularly train groups on beginner and intermediate Rust
    * And Haskell :)
* Running multiple production apps in Rust: backend, blockchain, CLI, analytics, and more    

---

## What is Rust?

* Systems programming language
* No garbage collector! Manual memory management
* Competitor/replacement for C/C++
* Focus on high performance

---

## Isn't this an FP meetup?

But Rust is much more

* Strong types
* Memory safety/no undefined behavior
* Expression-oriented
* Strong influences from FP languages
* Zero-cost abstractions
* "Fearless concurrency"
* Great package management (cargo)
* Macros to avoid boilerplate

Point being:

* Rust excels in systems programming
* But also works great as an application language

---

## Hello World!

No talk would be complete without it!

```rust
fn main() {
    println!("Hello, world!");
}
```

* Generated automatically by `cargo new projectname`
* What's the `!` for? Any guesses?

<div class="fragment">

Rust uses macros throughout for better safety and convenience

<pre><code class="lang-rust hljs">fn main() {
    println!("This is invalid syntax {");
    println!("This variable doesn't exist {hello}");

    let hello = Some(5);
    println!("This variable exists, but this still doesn't work: {hello}");

    println!("But this works: {hello:?}");
    println!("This too: {:?}", hello);
}</code></pre>

</div>

---

## Cargo.toml

* We're going to use some extra dependencies in these slides
* Want to follow along? Put this in your `Cargo.toml`
* Brand new to Rust? Install at https://www.rust-lang.org/learn/get-started

```toml
[package]
name = "introtorust"
version = "0.1.0"
edition = "2021"

[dependencies]
anyhow = "1"
axum = "0.6"
clap = { version = "4", features = ["env", "derive"] }
csv = "1"
fs-err = "2"
rand = "0.8"
reqwest = { version = "0.11", features = ["json"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
serde_yaml = "0.9"
tokio = { version = "1", features = ["full"] }

[dev-dependencies]
quickcheck = "1"

```

---

## Basic flavor of the language

* Let's start with some simple Rust examples
* Once we get a flavor for the language, we'll dive into real stuff!

```rust
use std::fmt::Display;

struct Staff {
    name: String,
    job: Job,
}

enum Job {
    Principal,
    Teacher(Subject),
}

enum Subject {
    History,
    Physics,
}

fn main() {
    let staff = Staff {
        name: "Alice".to_string(),
        job: Job::Teacher(Subject::Physics),
    };

    staff.greet();
}

impl Staff {
    fn greet(&self) {
        println!("Hello {}, your job is {}", self.name, self.job);

        // Alternative with pattern matching/destructuring
        let Staff { name, job } = self;
        println!("Hello {name}, your job is {job}");
    }
}

impl Display for Job {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Job::Principal => write!(f, "principal"),
            Job::Teacher(subject) => write!(f, "{subject} teacher"),
        }
    }
}

impl Display for Subject {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Subject::History => "history",
                Subject::Physics => "physics",
            }
        )
    }
}
```

---

## Iterators

* Lends itself to function-style coding
* Highly efficient generated code
* Here's a silly example demonstrating it

```rust
fn main() {
    (0..100)
        .filter(|x| x % 3 == 0)
        .map(|x| x + 1)
        .take_while(|x| *x < 50)
        .for_each(|x| println!("{x}"))
}
```

---

## Error handling

* No runtime exceptions, errors are explicit
* Leverages ADTs (`Result`)
* `must_use` prevents you from ignoring an error
* Great libraries like `anyhow` and `thiserror` make it convenient

```rust
use std::io::{BufRead, BufReader};

use anyhow::{Context, Result};

fn main() -> Result<()> {
    let file = fs_err::File::open("Cargo.toml")?;
    let buffered_file = BufReader::new(file);
    for (idx, line_res) in buffered_file.lines().enumerate() {
        // Reading the line may have failed, handle that failure
        let line = line_res.context("Reading a single line failed")?;
        println!("Line #{idx}: {line}");
    }
    Ok(()) // OK wrapping is a bit annoying, oh well :)
}
```

---

## Prime factors

Let's do some math and write a test suite.

```rust
fn read_line() -> String {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input
}

fn main() {
    println!("Enter an integer between 1 and {}", u16::MAX);
    let input = read_line();
    let input = input.trim().parse().unwrap();
    let factors = prime_factors(input);
    for factor in factors {
        println!("Prime factor: {factor}");
    }
}

fn prime_factors(mut input: u16) -> Vec<u16> {
    if input == 0 {
        panic!("Input cannot be 0!");
    }
    let mut factors = vec![];
    let mut primes = vec![2];
    let mut curr_prime = 2;

    while input != 1 {
        if input % curr_prime == 0 {
            factors.push(curr_prime);
            input /= curr_prime;
        } else {
            curr_prime = next_prime(curr_prime, &primes);
            primes.push(curr_prime);
        }
    }

    factors
}

fn next_prime(curr_prime: u16, primes: &[u16]) -> u16 {
    let is_prime = |candidate| {
        for prime in primes {
            if candidate % prime == 0 {
                return false;
            } else if *prime * *prime > candidate {
                return true;
            }
        }
        true
    };

    for candidate in curr_prime + 1.. {
        if is_prime(candidate) {
            return candidate;
        }
    }

    panic!("Oops! We ran out of primes.");
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::quickcheck;

    #[test]
    fn empty_for_one() {
        assert_eq!(prime_factors(1), vec![]);
    }

    #[test]
    fn singleton_for_primes() {
        assert_eq!(prime_factors(2), vec![2]);
        assert_eq!(prime_factors(5), vec![5]);
        assert_eq!(prime_factors(7), vec![7]);
        assert_eq!(prime_factors(73), vec![73]);
    }

    #[test]
    fn spot_checks() {
        assert_eq!(prime_factors(4), vec![2, 2]);
        assert_eq!(prime_factors(8), vec![2, 2, 2]);
        assert_eq!(prime_factors(24), vec![2, 2, 2, 3]);
        assert_eq!(prime_factors(100), vec![2, 2, 5, 5]);
        assert_eq!(prime_factors(60), vec![2, 2, 3, 5]);
    }

    #[test]
    fn next_prime_examples() {
        assert_eq!(next_prime(3, &[2, 3]), 5);
    }

    quickcheck! {
        fn product_is_original(orig: u16) -> bool {
            orig == 0 || prime_factors(orig).into_iter().product::<u16>() == orig
        }
    }
}
```

---

## serde

* **ser**ialize and **de**serialize data
* Wonderful derive system with attributes
* Support for _many_ different formats out of the box
* Leveraged extensively through the ecosystem

```rust
use anyhow::Result;
use rand::Rng;

#[derive(serde::Serialize, serde::Deserialize)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn random() -> Self {
        let mut rng = rand::thread_rng();
        Point {
            x: rng.gen_range(-500..=500),
            y: rng.gen_range(-500..=500),
        }
    }
}

fn main() -> Result<()> {
    // Write 1000 random points to a CSV file
    let mut csv = csv::Writer::from_path("points.csv")?;
    for _ in 0..1000 {
        csv.serialize(Point::random())?;
    }
    csv.flush()?; // not needed, happens automatically on drop
    std::mem::drop(csv); // make sure the file is closed before opening it again

    // Now read the data into a Vec
    let points = csv::Reader::from_path("points.csv")?
        .into_deserialize()
        .collect::<Result<Vec<Point>, _>>()?;

    // And write the data to JSON and YAML formats
    let mut json = fs_err::File::create("points.json")?;
    serde_json::to_writer_pretty(&mut json, &points)?;

    let mut yaml = fs_err::File::create("points.yaml")?;
    serde_yaml::to_writer(&mut yaml, &points)?;

    Ok(())
}
```

---

## Command line tooling

* Wonderful library: clap
* Heavy usage of derive macros and attributes
* Leverages ADTs and traits

```rust
use std::{fmt::Display, str::FromStr};

use clap::Parser;

/// A simple program demonstrating how clap works
#[derive(clap::Parser)]
#[clap(author = "Michael Snoyman")]
struct Opt {
    /// The name of the chat bot
    #[clap(long, global = true, env = "CHAT_BOT_NAME", default_value = "Eliza")]
    chat_bot_name: String,

    #[clap(subcommand)]
    cmd: Subcommand,
}

#[derive(clap::Parser)]
enum Subcommand {
    SayHello {
        name: String,
        #[clap(long, default_value = "afternoon")]
        time: TimeOfDay,
    },
    SayGoodbye,
}

#[derive(Clone, Copy)]
enum TimeOfDay {
    Morning,
    Afternoon,
    Evening,
}

// Could use a helper crate like strum
impl FromStr for TimeOfDay {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "morning" => Ok(TimeOfDay::Morning),
            "afternoon" => Ok(TimeOfDay::Afternoon),
            "evening" => Ok(TimeOfDay::Evening),
            _ => Err(anyhow::anyhow!("Unknown time of day: {s}")),
        }
    }
}
impl Display for TimeOfDay {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TimeOfDay::Morning => "morning",
                TimeOfDay::Afternoon => "afternoon",
                TimeOfDay::Evening => "evening",
            }
        )
    }
}

fn main() {
    let Opt { chat_bot_name, cmd } = Opt::parse();

    match cmd {
        Subcommand::SayHello { name, time } => {
            println!("Good {time}, {name}. My name is {chat_bot_name}.");
        }
        Subcommand::SayGoodbye => println!("It's been a pleasure! I've been {chat_bot_name}"),
    }
}
```

---

## Web client

* Leverages tokio and the async/await ecosystem
* Direct support for JSON via serde
* Provides a blocking API, would make more sense for this simple demo

```rust
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    let client = reqwest::ClientBuilder::new()
        .user_agent("Intro to Rust!")
        .build()?;
    let response: JsonResponse = client
        .get("https://httpbin.org/json")
        .send()
        .await?
        .json()
        .await?;
    println!("{response:#?}");
    Ok(())
}

#[derive(serde::Deserialize, Debug)]
struct JsonResponse {
    slideshow: Slideshow,
}

#[derive(serde::Deserialize, Debug)]
struct Slideshow {
    author: String,
    date: String,
    title: String,
}

```

---

## Web server

* Many different server libraries
* I'm a fan of axum, built on hyper (same as reqwest)
* Ties in really nicely with the tokio ecosystem

```rust
use std::net::SocketAddr;

use anyhow::Result;
use axum::{extract::State, routing::get, Json, Router};
use clap::Parser;
use rand::Rng;
use reqwest::Client;

async fn hello_world() -> &'static str {
    "Hello World!"
}

#[derive(serde::Serialize)]
struct RandomNumber {
    number: u64,
}

async fn random_number() -> Json<RandomNumber> {
    Json(RandomNumber {
        number: rand::thread_rng().gen(),
    })
}

async fn relay(State(client): State<Client>) -> Json<serde_json::Value> {
    let res = async {
        client
            .get("https://httpbin.org/json")
            .send()
            .await?
            .json()
            .await
    }
    .await;
    Json(match res {
        Ok(value) => value,
        Err(e) => serde_json::to_value(ErrorOccurred {
            error: e.to_string(),
        })
        .unwrap(),
    })
}

#[derive(serde::Serialize)]
struct ErrorOccurred {
    error: String,
}

#[derive(clap::Parser)]
struct Opt {
    #[clap(long, default_value = "0.0.0.0:3000")]
    bind: SocketAddr,
}

#[tokio::main]
async fn main() -> Result<()> {
    let Opt { bind } = Opt::parse();
    let client = reqwest::ClientBuilder::new()
        .user_agent("axum + reqwest")
        .build()?;
    let app = Router::new()
        .route("/hello-world", get(hello_world))
        .route("/random-number", get(random_number))
        .route("/relay", get(relay))
        .with_state(client);
    axum::Server::bind(&bind)
        .serve(app.into_make_service())
        .await?;
    Ok(())
}
```

---

## FP comparison

(Especially versus Haskell)

* FP-style
    * Traits
    * ADTs
    * Immutable
    * Strong typing
    * Iterators
    * Expression oriented
* Unlike FP
    * No garbage collection
    * Avoids more advanced techniques like Higher Kinded Types
    * Prefers specific notation (like `?` for errors) versus general notation (like `do`-notation)

---

## Other Rust use cases

* Rust is great for application-level development
* Lack of runtime and high performance give other opportunities
    * Embedded
    * Realtime
    * Write an entire operating system
    * Blockchain smart contracts (Cosmos, Solana, Near)
    * Web frontends (via WASM)
    * Serverless/AWS Lambda/Cloudflare Workers

---

## Personal takeaways

* FP is a fundamentally great paradigm
* Rust takes the best of FP
* Wonderful tooling, great set of libraries
* Manual memory management is a burden most apps don't need
    * But it gets much easier over time
    * And the other benefits of the language balance it out
* I highly recommend learning Rust and considering it for future projects

---

## Questions?

* If we have time, happy to demo some code
* Thanks for listening!
