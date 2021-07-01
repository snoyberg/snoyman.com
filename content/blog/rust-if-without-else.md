+++
title = "if without else in Rust"
description = "Answering a user question from Begin Rust: why must some if expressions evaluate to unit?"
path = "/blog/2021/07/rust-if-without-else"
date = 2021-07-01
+++
Miriam and my book, [Begin Rust](https://www.beginrust.com), is available both as a standalone book or as a [course on Educative](https://www.educative.io/courses/ultimate-guide-to-rust-programming). One of our users on Educative asked a question about the "Hello If" section of the book, and we decided to provide a response on the blog (in addition to on the [Begin Rust mailing list](https://groups.google.com/g/begin-rust).

* * *

In the book, we say:

> However, there is one restriction in the kind of if expressions we’ve seen so far. They must evaluate to unit ()

The user asked whether this was related to the fact that the main function is returning unit, which in fact is implied by the error message the compiler gives. However, that's _not_ the case. In the context of that section, "the kind of if expressions" we're referring to are if expressions without else blocks. Let's demonstrate. Consider this program:

```rust
fn return_number(is_even: bool) -> u32 {
    if is_even {
        42
    } else {
        43
    }
}

fn main() {
    println!("{}", return_number(true));
    println!("{}", return_number(false));
}
```

We have an `if`/`else` expression which evaluates to a `u32`. That becomes the return value of the function `return_number`. Everything is fine.

However, let's tweak this ever so slightly and remove the else:

```rust
fn return_number(is_even: bool) -> u32 {
    if is_even {
        42
    }
}
```

This no longer compiles, and instead gives the error message:

```
error[E0317]: `if` may be missing an `else` clause
```

We can try to "fix" this by ignoring the result of evaluating the if expression, like so:

```rust
fn return_number(is_even: bool) -> u32 {
    if is_even {
        42
    }; // semicolon ignores the result!
    43
}
```

Even so, the compiler is not happy, and gives the same error message above.

Takeaways:

* If you have an if without an else, the body must _always_ evaluate to unit, no exceptions.
* If you have an if and an else, their blocks must evaluate to the same type.
* If you return the result of an if/else expression from a function, the return type of the function must match the type of the expressions in the if and else blocks.
