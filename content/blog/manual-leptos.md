+++
title = "Manual Leptos"
description = "Leptos is a Rust frontend web framework focusing on fine-grained reactivity. In this post, we take a look at a more manual approach to using Leptos."
date = 2024-06-25
+++
I've spent most of my career on the server side. In the past few years, the projects I've been running have included significant TypeScript+React codebases, which has given me a crash course in the framework. About six months ago, I decided to look into Rust frontend frameworks, and played around with Leptos. I ended up writing a simple utility program with Leptos. Overall, the process was pretty nice, and the performance of the app was noteworthy. (Meaning: other team members commented on how responsive the app was.) However, I never felt like I fully grokked Leptos. In particular, I felt like I was always glancing over my shoulder to make sure I'd properly made things reactive (more details on this below).

Last week, we had some FP Complete discussions around frontend frameworks, and experimented a bit with a Leptos codebase on an engineering call. The topics inspired me to look at Leptos with a fresh set of eyes. And I think I realized where a lot of my pain came from: the similarity in Leptos's syntax to React confuses me!

In this post, I'm going to walk some simple activities in Leptos, showing where it's easy to make your site non-reactive by mistake, and in the process explore a more manual approach to Leptos. I'm not convinced yet that this is a better approach, and I may be missing out on some downsides\*. But at the very least, I felt more comfortable about my understanding of Leptos after trying this out.

\* One downside is explicitly called out in the Leptos book: by bypassing the `view!` macro (one of the later things we try in this post), you take a performance penalty when using SSR. See the [performance note](https://book.leptos.dev/view/builder.html#performance-note) for details.

Prequisites for this post: basic Rust knowledge, basic understanding of frontend development, especially the DOM. And if you know some React, the post will make more sense. I've also included the full history of code samples as separate commits in [my manual-leptos GitHub repo](https://github.com/snoyberg/manual-leptos).

## Hello Leptos!

We're going to start off with a standard Leptos project following the [getting started instructions](https://book.leptos.dev/getting_started/index.html). Basically:

* [Install Rust](https://www.rust-lang.org/tools/install)
* Install `trunk`: `cargo install trunk`
* Create a new project: `cargo new manual-leptos && cd manual-leptos`
* Add Leptos as a dependency with nightly functionality: `cargo add leptos --features=csr,nightly`
* Set your compiler to nightly: `rustup toolchain install nightly && rustup override set nightly`
    * If you're wondering why we're using nightly, it's for function-call syntax of signals. See the getting started page linked above for details.
* Create an `index.html` file with the following content. It should be in the same directory as `Cargo.toml`:

  ```html
  <!DOCTYPE html>
  <html>
    <head></head>
    <body></body>
  </html>
  ```
* Replace your `src/main.rs` file with the following content:

  ```rust
  use leptos::*;

  fn main() {
      mount_to_body(|| view! { <p>"Hello, world!"</p> })
  }
  ```
* Run `trunk serve` and open your browser to <http://localhost:8080>

Congratulations! You've followed the basic tutorial! Now it's time to do some of our own stuff.

## Our first signal

The core building block of Leptos is *Signals*. Signals are essentially mutable variables that allow you to subscribe to updates. One of the earliest things we learn in Leptos is how to use `create_signal`. Let's see this in action:

```rust
use leptos::*;

fn main() {
    mount_to_body(|| view! { <App /> })
}

#[component]
fn App() -> impl IntoView {
    let (name, set_name) = create_signal("Alice".to_owned());
    let change_name = move |_| set_name("Bob".to_owned());
    view! {
        <SayHi name={name()} />
        <button on:click=change_name>Say hi to Bob!</button>
    }
}

#[component]
fn SayHi(name: String) -> impl IntoView {
    view! { <p>Hello, <b>{name}</b></p> }
}
```

We've created a new signal to hold the name of the person we want to greet. Initially we start with Alice, and provide a button to update that signal to Bob. The `change_name` closure is used as the `on:click` handler for a button. And we introduce a `SayHi` component to say the name. Go ahead and run this application.

__EXERCISE__ Something isn't working correctly in this code. Can you identify what the buggy behavior is? And try to figure out _why_ the bug occurred. We'll explain it in the next section.

## Reactivity

If you come from a React background, the code above probably looked pretty reasonable. We have a component called `SayHi` which takes a parameter (or, perhaps better called _property_), and we render our view based on that. However, if you ran the application and clicked on the button, you may have noticed that nothing happened. Why?

The explanation is simple. In Leptos, _we only call our render functions once_. `SayHi` is a function that gets called when the page is loaded. And when it renders, it puts in the initial `name` value, which is `Alice`. And even if the signal updates later, we're not _subscribed_ to that update.

Fixing this is fortunately fairly straightforward. Instead of `SayHi` taking a `String`, it needs to take a _reactive `String`_. There's [more information in the Leptos book](https://book.leptos.dev/view/03_components.html#reactive-and-static-props) on this. But we can address this by converting our `String` prop into a `Signal<String>`. We can also use some macro magic from Leptos to make it fairly pleasant to look at:

```diff
     let (name, set_name) = create_signal("Alice".to_owned());
     let change_name = move |_| set_name("Bob".to_owned());
     view! {
-        <SayHi name={name()} />
+        <SayHi name={name} />
         <button on:click=change_name>Say hi to Bob!</button>
     }
 }

 #[component]
-fn SayHi(name: String) -> impl IntoView {
+fn SayHi(#[prop(into)] name: Signal<String>) -> impl IntoView {
     view! { <p>Hello, <b>{name}</b></p> }
 }
```

With these two changes, our application works as expected! When we embed `name` inside the `view!` macro in the `SayHi` component, we're embedding a signal, not a value. Leptos automatically subscribes to any changes in that signal, and will update just the relevant DOM node when the signal is updated. In the callsite, we no longer pass in `name()`, but rather `name`. This is the heart of reactivity in Leptos: when we want values to be updated, we pass around signals, not values. Previously, we were getting the current value of the signal when doing initial render and never updating it. Now we pass in the mutable signal itself.

We can simplify this a bit more by leveraging punning:

```rust
-        <SayHi name={name} />
+        <SayHi name />
```

## Optional names

Our application right now starts off saying hi to Alice. But why this bias towards Alice? Maybe we wanted to say hi to Bob first! That's fairly easy to model in `App`: we use an `Option<String>` instead of a `String`:

```rust
#[component]
fn App() -> impl IntoView {
    let (name, set_name) = create_signal(None);
    view! {
        <SayHi name />
        <button on:click=move |_| set_name(Some("Alice".to_owned()))>Say hi to Alice!</button>
        <button on:click=move |_| set_name(Some("Bob".to_owned()))>Say hi to Bob!</button>
    }
}
```

The changes to `SayHi` to handle this `Option` are pretty simple too:

```diff
 #[component]
-fn SayHi(#[prop(into)] name: Signal<String>) -> impl IntoView {
+fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
     view! { <p>Hello, <b>{name}</b></p> }
 }
```

__EXERCISE__ What's the content of the web page when you first load it?

Unfortunately, the initial display is a bit lacking. The `name` signal has an `Option<String>`. Once we choose either Alice or Bob, everything displays as expected. However, initially, we see `Hello, `, because the `None` value renders to nothing. That's not what we want! Instead, we want to put up a message saying that no name has been selected. The following code looks correct, but it isn't:

```rust
#[component]
fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
    match name() {
        Some(name) => view! { <p>Hello, <b>{name}</b></p> },
        None => view! { <p>No name selected</p> },
    }
}
```

__EXERCISE__ Without running the code, can you guess what the incorrect behavior is? Super bonus points if you can figure out a solution.

### Side note: great error messages!

Before moving onto the solution, I want to hopefully calm some concerns. When I first read about fine-grained reactivity in Leptos, I was sure I would mess it up on a regular basis. Fortunately, the runtime diagnostics are _really great_. For example, when running the code above, I get the following error message:

```
you access a signal or memo (defined at src/main.rs:9:28) outside a reactive tracking context. This might mean your app is not responding to changes in signal values in the way you expect.

Here’s how to fix it:

1. If this is inside a `view!` macro, make sure you are passing a function, not a value.
  ❌ NO  <p>{x.get() * 2}</p>
  ✅ YES <p>{move || x.get() * 2}</p>

2. If it’s in the body of a component, try wrapping this access in a closure: 
  ❌ NO  let y = x.get() * 2
  ✅ YES let y = move || x.get() * 2.

3. If you’re *trying* to access the value without tracking, use `.get_untracked()` or `.with_untracked()` instead.
```

Hopefully that gives you a good idea of what's broken!

## I need some closure

The problem, again, is that we're running the `name` signal _at render time_, and not subscribing to updates from it. Instead, we need to _reactively_ determine whether we're in the `None` or `Some` case. So far, we've seen reactivity always come from subscribing to a `Signal`. Fortunately, we have another option: closures. Every function in Leptos supports reactivity. Our problem is that our `SayHi` component simply returns a fully static view. Instead, we want it to return a closure! Fortunately, closures also implement the `IntoView` trait, so fixing our example is as easy as:

```rust
 #[component]
 fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
-    match name() {
+    move || match name() {
         Some(name) => view! { <p>Hello, <b>{name}</b></p> },
         None => view! { <p>No name selected</p> },
     }
```

And with that, we have a properly reactive application! Adding closures to force reactivity is one of the most common activities in Leptos.

### More advanced topic: tighter reactivity

Feel free to skip this section, it covers a more advanced topic.

This code works fine, and is probably close to what I'd use in a production application. However, arguably it's inefficient. Each time the name changes, it forces a full recreation of all the DOM nodes. In reality, we should only need to update the one text node with the name when the signal changes for one person to the other. However, our signal fires each time the name changes, causing new DOM nodes to be created.

Is this a problem? Probably not in this case, but in larger examples, it could hurt performance. It may also harm user interactions by removing DOM input nodes they were interacting with. A standard way of approaching this is to use a *memoized signal*, which only tells subscribers to update if the actual value has changed. For example, we can create a memoized signal to let us know if the name is set:

```rust
#[component]
fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
    let is_set = create_memo(move |_| name().is_some());
    move || {
        if is_set() {
            view! { <p>Hello, <b>{name}</b></p> }
        } else {
            view! { <p>No name selected</p> }
        }
    }
}
```

Now only the text node itself updates! Exactly what we wanted!

There's a more common approach to this: [using the `Show` component](https://book.leptos.dev/view/06_control_flow.html#show).

```rust
#[component]
fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
    view! {
        <Show
            when=move || name().is_some()
            fallback=|| view! { <p>No name selected</p> }
        >
            <p>Hello, <b>{name}</b></p>
        </Show>
    }
}
```

Both versions of the application only update the text node. I'll leave it to each reader to determine whether they prefer the first or second approach. But since we're exploring manual approaches in this post, we won't be using helper components like `Show` going forward.

And one final comment in this section. Here's an example of the `SayHi` component that demonstrates the over-rerendering issue:

```rust
#[component]
fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
    let (update_count, set_update_count) = create_signal(0);
    move || {
        set_update_count.update(|old| *old += 1);
        if name().is_some() {
            view! { <p>Hello, <b>{name}</b>. Updates: {update_count}</p> }
        } else {
            view! { <p>No name selected. Updates: {update_count}</p> }
        }
    }
}
```

If you're coming from a React background, you may be a bit surprised to notice that modifying the update count in the same component that uses it does _not_ create an infinite loop! That's one of the reasons I love Leptos's fine-grained reactivity so much more than the vDOM/rerender approach.

## Do we need components?

We've been using the `#[component]` macro since the beginning of this post. Do we need it? Let's find out!

```diff
-#[component]
+#[allow(non_snake_case)]
 fn App() -> impl IntoView {
```

This works! It turns out that components are simply functions, awkwardly named in PascalCase instead of snake_case. We've added an `allow` to avoid warnings about the non-snake-case, but otherwise everything works exactly as before.

However, the same cannot be said of our `SayHi` component. Since we're using the `#[prop(into)]` attribute, removing `#[component]` causes the compilation to fail:

```
error: cannot find attribute `prop` in this scope
  --> src/main.rs:17:12
   |
17 | fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
   |            ^^^^
```

Getting rid of that `prop` attribute is easy enough:

```diff
-#[component]
-fn SayHi(#[prop(into)] name: Signal<Option<String>>) -> impl IntoView {
+#[allow(non_snake_case)]
+fn SayHi(name: impl Into<Signal<Option<String>>>) -> impl IntoView {
+    let name = name.into();
     let is_set = create_memo(move |_| name().is_some());
```

But we still get a compilation failure:

```
error[E0282]: type annotations needed
  --> src/main.rs:11:10
   |
11 |         <SayHi name />
   |          ^^^^^ cannot infer type
```

I'll be honest, I don't fully understand which type needs to be inferred here. But the problem is easy enough to explain: we can't pass properties to a component using the HTML-tag syntax without the `#[component]` macro. We can instead treat `SayHi` as a function directly like this:

```diff
-        <SayHi name />
+        {SayHi(name)}
```

And once we realize that, we can replace all of our components with normal, snake-case-named functions:

```rust
use leptos::*;

fn main() {
    mount_to_body(app)
}

fn app() -> impl IntoView {
    let (name, set_name) = create_signal(None);
    view! {
        {say_hi(name)}
        <button on:click=move |_| set_name(Some("Alice".to_owned()))>Say hi to Alice!</button>
        <button on:click=move |_| set_name(Some("Bob".to_owned()))>Say hi to Bob!</button>
    }
}

fn say_hi(name: impl Into<Signal<Option<String>>>) -> impl IntoView {
    let name = name.into();
    let is_set = create_memo(move |_| name().is_some());
    move || {
        if is_set() {
            view! { <p>Hello, <b>{name}</b></p> }
        } else {
            view! { <p>No name selected</p> }
        }
    }
}
```

## Look ma, no macros!

For more details, check out the [Leptos book on builder syntax](https://book.leptos.dev/view/builder.html).

We're now left with only one macro used in our application: `view!`. And we don't need that one either. The `leptos::html` module contains functions for creating all kinds of HTML nodes. I haven't used it extensively myself, but the small bit I _have_ used has been pleasant. Here's a rewrite to use the builder syntax:

```rust
use leptos::*;

fn main() {
    mount_to_body(app)
}

fn app() -> impl IntoView {
    let (name, set_name) = create_signal(None);
    let make_button = move |name: &str| {
        let name = name.to_owned();
        html::button()
            .child(format!("Say hi to {name}!"))
            .on(ev::click, move |_| set_name(Some(name.clone())))
            .into_view()
    };
    [
        say_hi(name).into_view(),
        make_button("Alice"),
        make_button("Bob"),
    ]
}

fn say_hi(name: impl Into<Signal<Option<String>>>) -> impl IntoView {
    let name = name.into();
    let is_set = create_memo(move |_| name().is_some());
    move || {
        if is_set() {
            html::p().child("Hello, ").child(html::b().child(name))
        } else {
            html::p().child("No name selected")
        }
    }
}
```

## Conclusion

I personally find it much easier to understand how reactivity is working in the non-macro version of the code. The explicitness of returning closures makes it much easier to see how reactivity is flowing into the system. By contrast, when I've written code previously using things like `Suspense` and `ErrorBoundary`, I always felt like I was just hoping it would work correctly.

Will I move in this direction in general? I don't know. I'm not actually opposed to the macros themselves, and I think the view macro is really great for simple cases. However, even needing to reach out to `Show` for the implementation of the `SayHi` component felt wrong. I think what I'll end up experimenting with is using this more manual approach for overall page setup, and then use the regular view macro for individual pieces of content.

I'm planning on continuing with this way of playing with Leptos. I may share a few more blog posts in the future on these experiments. (And if people _do_ want to see more, please let me know.) My next pieces of investigation will be playing with an alternative to routing and using [leptos-query](https://docs.rs/leptos_query) for making network requests. In fact, playing with leptos-query is what pushed me into playing with this "manual Leptos" approach, since porting my existing app to leptos-query was more painful than expected.

For routing, I'm thinking of stealing the idea of well-typed routes from Yesod. One annoyance I have in general with routing is how you have to write duplicate parameter checking code inside route handlers. This isn't Leptos-specific, I have similar issues with axum's default routing. Hopefully I'll have more to share soon.
