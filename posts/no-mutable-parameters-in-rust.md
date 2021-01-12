While reviewing the second-to-last chapter in the [Begin Rust](https://www.beginrust.com/) book, a slightly more advanced topic came up. This is a topic that has intrigued me for a while, in particular because it demonstrates some fundamental differences in how Rust and Haskell approach mutability. The topic is too advanced for the book, but I wanted to provide an external source to reference for people who are curious. So here it is!

Let's build this up bit by bit. Does the following program compile?

```rust
fn main() {
    let x = 5;
    x += 1;
    println!("x == {}", x);
}
```

Answer: no! `x` is an immutable variable, and therefore you can't use `+= 1` on it. Fixing this is easy: add `mut`:

```rust
fn main() {
    let mut x = 5;
    x += 1;
    println!("x == {}", x);
}
```

But since adding 1 and printing is such an important operation in my application (yes, that's sarcasm), I've decided to extract it to its own function. Tell me, does _this_ code compile?

```rust
fn add_and_print(x: i32) {
    x += 1;
    println!("x == {}", x);
}
```

No, and for the same reason as the first example: `x` is immutable. Fixing that is easy enough here too:

```rust
fn add_and_print(mut x: i32) {
    x += 1;
    println!("x == {}", x);
}
```

Now we have `x` as the only parameter to `add_and_print`. Its type is `i32`, and it is mutable. OK, cool. Finally we're in a position to use this function from `main`. Tell me, will this program compile and run? And as some really great foreshadowing, will the program generate any warnings?

```rust
fn main() {
    let mut x = 5;
    add_and_print(x);
}

fn add_and_print(mut x: i32) {
    x += 1;
    println!("x == {}", x);
}
```

Answer: it compiles, runs, and generates the output `x == 6`. However, it _does_ have a warning:

```
warning: variable does not need to be mutable
 --> src/main.rs:2:9
  |
2 |     let mut x = 5;
  |         ----^
  |         |
  |         help: remove this `mut`
  |
```

Initially, at least for me, this was really surprising. `add_and_print` needs to receive a mutable `i32` as its first parameter. We provide it with a mutable `i32`. And then the compiler says that the `mut` in `main` is unnecessary. What gives?!?

There's a mistake in my explanation above. The function `add_and_print`, perhaps very confusingly, does _not_ take a mutable `i32` as a parameter. "But it says `mut`!!!" you argue. That's true. However, that detail is _internal to the function_, not part of its type signature. That sounds confusing, so let me explain.

There's a pattern that appears all over Rust which I call the [rule of three](https://www.snoyman.com/blog/2018/11/rust-crash-course-05-rule-of-three). This covers the fact that in many cases, we end up having three "versions" of things:

* An immutable borrow
* A mutable borrow
* A move

That can apply to function parameters, for example:

```rust
fn immutable_borrow(x: &String)
fn mutable_borrow(x: &mut String)
fn move(x: String)
```

Notice that the difference is entirely _after_ the colon. These three things are different types, and the types are what make up the signature of the function.

However, the stuff before the colon, in this case the `x`, does _not_ affect the signature of the function. What name you choose for the variable that captures the parameter is irrelevant to the signature of the function. Once you pass in a value of the right type to the function, the function gets to decide what to call it.

But this rule applies to more than just the variable name. It applies to the variable _mutability_ as well. You see, mutability in Rust is a feature of the _variable_, not the value. When I say `let mut x = 5`, I'm saying "I'm creating a variable called `x`, it's pointing a value `5`, and I'm allowed to use `x` to mutate that value." If I drop the `mut`, I'm no longer allowed to mutate that value via the variable `x`.

A gut reaction you might have is, if you can't mutate the variable, the only thing you can do is read it. This is where my Haskeller spidey-senses lead me to at least. But it's not true in Rust. There's one other thing you're allowed to do: move the value into another scope. `add_and_print` accepts the value by move, and even if `x` is an immutable variable, I'm still allowed to move the value that it references.

Once I've moved the value, it's entirely up to `add_and_print` to decide what to do with it. And it's allowed to call it mutable, even though the original variable was immutable. That's because the _value itself_ is passed in, not the variable. And the value is perfectly comfortable being mutated.

So a warning-free version of the program is:

```rust
fn main() {
    let x = 5;
    add_and_print(x);
}

fn add_and_print(mut x: i32) {
    x += 1;
    println!("x == {}", x);
}
```

In fact, moving-into-mutable is something that can happen even without a function call. For example, you can "upgrade" from an immutable variable to a mutable one inside a single function:

```rust
fn main() {
    let x = 5;
    let mut y = x;
    y += 1;
    println!("y == {}", y);
}
```

"But wait," you complain, "you can't just 'upgrade' an immutable reference to a mutable reference!" Or maybe a dozen other similar complaints about my explanation. What I'm slightly glossing over here is that, when it comes to references, the mutability _is_ baked into the value. That's because with something like `x: &i32`, `x` doesn't have a number, it has a _reference_ to a number. And references themselves are types that do in fact enforce mutable vs immutable rules. Therefore, you can't simply upgrade an immutable reference to a mutable one. This code is broken:

```rust
fn main() {
    let mut x: i32 = 5;
    let y: &i32 = &x;
    let z: &mut i32 = y; // b0rken!

    *z += 1;

    println!("x == {}", x);
}
```

So, pulling it back together:

* You can have values, immutable references to values, and mutable references to values
* Values are neither immutable or mutable
* Instead, variables are immutable or mutable
* When you move a value into a new variable (via a `let` or a function call), you can change the mutability of the variable
* The mutability and names of variables in function signatures do not affect the signature of the function
* The mutability of a reference is built into the type itself, so you can't "upgrade" an immutable reference to a mutable one
