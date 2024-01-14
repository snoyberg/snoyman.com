+++
title = "My Best and Worst Deadlock in Rust"
description = "I wrote a deadlock a few months back in some Rust code, and we finally solved it recently. It was both my best and worst deadlock ever."
path = "/blog/2024/01/best-worst-deadlock-rust"
date = 2024-01-15
+++
We're going to build up a deadlock together. If you're unfamiliar with Rust and/or its multithreaded concepts, you'll probably learn a lot from this. If you _are_ familiar with Rust's multithreading capabilities, my guess is you'll be as surprised by this deadlock as I was. And if you spot the deadlock immediately, you get a figurative hat-tip from me.

As to the title, this deadlock was the worst I ever experienced because of how subtle it was. It was the best because of the tooling told me exactly where the problem was. You'll see both points come out below.

## Access control

If you've read much of my writing, you'll know I almost always introduce a data structure that looks like this:

```rust
struct Person {
    name: String,
    age: u32,
}
```

So we'll do something very similar here! I'm going to simulate some kind of an access control program that allows multiple threads to use some shared, mutable state representing a person. And we'll make two sets of accesses to this state:

* A read-only thread that checks if the user has access
* A writer thread that will simulate a birthday and make the person 1 year older

Our access control is really simple: we grant access to people 18 years or older. One way to write this program looks like this:

```rust
use std::sync::Arc;

use parking_lot::RwLock;

#[derive(Clone)]
struct Person {
    inner: Arc<RwLock<PersonInner>>,
}

struct PersonInner {
    name: String,
    age: u32,
}

impl Person {
    fn can_access(&self) -> bool {
        const MIN_AGE: u32 = 18;

        self.inner.read().age >= MIN_AGE
    }

    /// Returns the new age
    fn birthday(&self) -> u32 {
        let mut guard = self.inner.write();
        guard.age += 1;
        guard.age
    }
}

fn main() {
    let alice = Person {
        inner: Arc::new(RwLock::new(PersonInner {
            name: "Alice".to_owned(),
            age: 15,
        })),
    };

    let alice_clone = alice.clone();
    std::thread::spawn(move || loop {
        println!("Does the person have access? {}", alice_clone.can_access());
        std::thread::sleep(std::time::Duration::from_secs(1));
    });

    for _ in 0..10 {
        std::thread::sleep(std::time::Duration::from_secs(1));
        let new_age = alice.birthday();

        println!("Happy birthday! Person is now {new_age} years old.");
    }
}
```

We're using the wonderful [parking-lot crate](https://docs.rs/parking_lot) for this example. Since we have one thread which will exclusively read, an [`RwLock`](https://docs.rs/parking_lot/latest/parking_lot/type.RwLock.html) seems like the right data structure to use. It will allow us to take multiple concurrent read locks or one exclusive write lock at a time. For those familiar with it, this is very similar to the general Rust borrow rules, which allow for multiple read-only (or shared) references or a single mutable (or exclusive) reference.

Anyway, we follow a common pattern with our `Person` data type. It has a single `inner` field, which contains an `Arc` and `RwLock` wrapping around our inner data structure, which contains the actual name and age. Now we can cheaply clone the `Person`, keep a single shared piece of data in memory for multiple threads, and either read or mutate the values inside.

Next up, to provide nicely encapsulated access, we provide a series of methods on `Person` that handle the logic of getting read or write locks. In particular, the `can_access` method takes a read lock, gets the current age, and compares it to the constant value 18. The `birthday` method takes a write lock and increments the age, returning the new value.

If you run this on your computer, you'll see something like the following output:

```
Does the person have access? false
Happy birthday! Person is now 16 years old.
Does the person have access? false
Happy birthday! Person is now 17 years old.
Does the person have access? false
Does the person have access? false
Happy birthday! Person is now 18 years old.
Does the person have access? true
Happy birthday! Person is now 19 years old.
Does the person have access? true
Happy birthday! Person is now 20 years old.
Does the person have access? true
Happy birthday! Person is now 21 years old.
Does the person have access? true
Happy birthday! Person is now 22 years old.
Does the person have access? true
Happy birthday! Person is now 23 years old.
Does the person have access? true
Happy birthday! Person is now 24 years old.
Does the person have access? true
Happy birthday! Person is now 25 years old.
```

The output may look slightly different due to timing differences, but you get the idea. The person, whoever that happens to be, suddenly has access starting at age 18.

**NOTE TO READER** I'm not going to keep asking this, but I encourage you to look at each code sample and ask: is this the one that introduces the deadlock? I'll give you the answers towards the end of the post.

## What's in a name?

It's pretty annoying having now idea _who_ has access. Alice has a name! We should use it. Let's implement a helper method for getting the person's name:

```rust
fn get_name(&self) -> &String {
    &self.inner.read().name
}
```

While this looks nice, it doesn't actually compile:

```
error[E0515]: cannot return value referencing temporary value
  --> src/main.rs:30:9
   |
30 |         &self.inner.read().name
   |         ^-----------------^^^^^
   |         ||
   |         |temporary value created here
   |         returns a value referencing data owned by the current function
```

You see, the way an `RwLock`'s `read` method works is that it returns a `RwLockReadGuard`. This implements all the borrow rules we want to see at runtime via value creation and dropping. Said more directly: when you call `read`, it does something like the following:

1. Waits until it's allowed to take a read guard. For example, if there's an existing write guard active, it will block until that write guard finishes.
2. Increments a counter _somewhere_ indicating that there's a new active read guard.
3. Constructs the `RwLockReadGuard` value.
4. When that value gets dropped, its `Drop` impl will decrement that counter.

And this is basically how many interior mutability primitives in Rust work, whether it's an `RwLock`, `Mutex`, or `RefCell`.

The problem with our implementation of `get_name` is that it tries to take a lock and then borrow a value through the lock. However, when we exit the `get_name` method is that it's still holding a reference to the `RwLockReadGuard` which we're trying to drop. So how do we implement this method? There are a few possibilities:

* Return the `RwLockReadGuard<PersonInner>`. This is no longer a `get_name` method, but now a general purpose "get a read lock" method. It's also unsatisfying because it requires exposing the innards of our inner data structure.
* Clone the inner `String`, which is unnecessary allocation.
* Wrap the `name` field with an `Arc` and clone the `Arc`, which is [probably cheaper than cloning the `String`](https://blocklisted.github.io/blog/arc_str_vs_string_is_it_really_faster/).

There are really interesting API design points implied by all this, and it would be fun to explore them another time. However, right now, I've got a tight deadline from my boss on the really important feature of _print out the person's name_, so I better throw together something really quick and direct. And the easiest thing to do is to just lock the `RwLock` directly wherever we want a name.

We'll make a small tweak to our spawned thread's closure:

```rust
std::thread::spawn(move || loop {
    let guard = alice_clone.inner.read();
    println!(
        "Does the {} have access? {}",
        guard.name,
        alice_clone.can_access()
    );
    std::thread::sleep(std::time::Duration::from_secs(1));
});
```

## Delays

> The definition of insanity is doing the same thing over and over and expecting different results

\- *Somebody*, but almost certainly __not__ Albert Einstein

By the above definition of insanity, many have pointed out that multithreaded programming is asking the programmer to become insane. You need to expect different results for different runs of a program. That's because the interleaving of actions between two different threads is non-deterministic. Random delays, scheduling differences, and much more can cause a program to behave correctly on one run and completely incorrectly on another. Which is what makes deadlocks so infuriatingly difficult to diagnose and fix.

So let's simulate some of those random delays in our program by pretending that we need to download some super cute loading image while checking access. I've done so with a `println` call and an extra sleep to simulate the network request time:

```rust
    std::thread::spawn(move || loop {
        let guard = alice_clone.inner.read();
        println!("Downloading a cute loading image, please wait...");
        std::thread::sleep(std::time::Duration::from_secs(1));
        println!(
            "Does the {} have access? {}",
            guard.name,
            alice_clone.can_access()
        );
        std::thread::sleep(std::time::Duration::from_secs(1));
    });
```

And when I run my program, lo and behold, output stops after printing `Downloading a cute loading image, please wait...`. Maybe the output will be a bit different on your computer, maybe not. That's the nature of the non-deterministic beast. But this appears to be a deadlock.

## The best deadlock experience ever

It turns out that the `parking-lot` crate provides an experimental feature: deadlock detection. When we were facing the real-life deadlock in our production systems, [Sibi](https://x.com/psibi) found this feature and added it to our executable. And boom! The next time our program deadlocked, we immediately got a backtrace pointing us to the exact function where the deadlock occurred. Since it was a release build, we didn't get line numbers, since those had been stripped out. But since I'm doing a debug build for this blog post, we're going to get something even better here.

Let's add in the following code to the top of our `main` function:

```rust
    std::thread::spawn(move || loop {
        std::thread::sleep(std::time::Duration::from_secs(2));
        for deadlock in parking_lot::deadlock::check_deadlock() {
            for deadlock in deadlock {
                println!(
                    "Found a deadlock! {}:\n{:?}",
                    deadlock.thread_id(),
                    deadlock.backtrace()
                );
            }
        }
    });
```

Every 2 seconds, this background thread will check if `parking-lot` has detected any deadlocks and print out the thread they occurred in and the full backtrace. (Why 2 seconds? Totally arbitrary. You could use any sleep amount you want.) When I add this to my program, I get some very helpful output. I'll slightly trim the output to not bother with a bunch of uninteresting backtraces outside of the `main` function:

```
Found a deadlock! 140559740036800:
   0: parking_lot_core::parking_lot::deadlock_impl::on_unpark
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot_core-0.9.9/src/parking_lot.rs:1211:32
   1: parking_lot_core::parking_lot::deadlock::on_unpark
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot_core-0.9.9/src/parking_lot.rs:1144:9
   2: parking_lot_core::parking_lot::park::{{closure}}
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot_core-0.9.9/src/parking_lot.rs:637:17
   3: parking_lot_core::parking_lot::with_thread_data
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot_core-0.9.9/src/parking_lot.rs:207:5
      parking_lot_core::parking_lot::park
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot_core-0.9.9/src/parking_lot.rs:600:5
   4: parking_lot::raw_rwlock::RawRwLock::lock_common
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot-0.12.1/src/raw_rwlock.rs:1115:17
   5: parking_lot::raw_rwlock::RawRwLock::lock_shared_slow
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot-0.12.1/src/raw_rwlock.rs:719:9
   6: <parking_lot::raw_rwlock::RawRwLock as lock_api::rwlock::RawRwLock>::lock_shared
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/parking_lot-0.12.1/src/raw_rwlock.rs:109:26
   7: lock_api::rwlock::RwLock<R,T>::read
             at /home/michael/.cargo/registry/src/index.crates.io-6f17d22bba15001f/lock_api-0.4.11/src/rwlock.rs:459:9
   8: access_control::Person::can_access
             at src/main.rs:19:9
   9: access_control::main::{{closure}}
             at src/main.rs:59:13
  10: std::sys_common::backtrace::__rust_begin_short_backtrace
             at /rustc/79e9716c980570bfd1f666e3b16ac583f0168962/library/std/src/sys_common/backtrace.rs:154:18
```

Wow, this gave us a direct pointer to where in our codebase the problem occurs. The deadlock happens in the `can_access` method, which is called from our `println!` macro call in `main`.

In a program of this size, getting a direct link to the relevant code isn't terribly helpful. There were only a few lines that could have possibly caused the deadlock. However, in our production codebase, we have thousands of lines of code in the program itself that could have possibly been related. And it turns out the program itself wasn't even the culprit, it was one of the support libraries we wrote!

Being able to get such direct information on a deadlock is a complete gamechanger for debugging problems of this variety. Absolutely huge props and thanks to the `parking-lot` team for providing this.

## But what's the problem?

OK, now it's time for the worst. We still need to identify what's causing the deadlock. Let's start off with the actual deadlock location: the `can_access` method:

```rust
fn can_access(&self) -> bool {
    const MIN_AGE: u32 = 18;

    self.inner.read().age >= MIN_AGE
}
```

Is this code, on its own, buggy? Try as I might, I can't possibly find a bug in this code. And there isn't one. This is completely legitimate usage of a read lock. In fact, it's a great demonstration of best practices: we take the lock for as little time as needed, ensuring we free the lock and avoiding contention.

So let's go up the call stack and look at the body of our subthread infinite loop:

```rust
let guard = alice_clone.inner.read();
println!("Downloading a cute loading image, please wait...");
std::thread::sleep(std::time::Duration::from_secs(1));
println!(
    "Does the {} have access? {}",
    guard.name,
    alice_clone.can_access()
);
std::thread::sleep(std::time::Duration::from_secs(1));
```

This code is already pretty suspicious. The first thing that pops out to me when reading this code is the sleeps. We're doing something _very_ inappropriate: holding onto a read lock while sleeping. This is a sure-fire way to cause contention for locks. It would be far superior to only take the locks for a limited period of time. Because lexical scoping leads to drops, and drops lead to freeing locks, one possible implementation would look like this:

```rust
println!("Downloading a cute loading image, please wait...");
std::thread::sleep(std::time::Duration::from_secs(1));
{
    let guard = alice_clone.inner.read();
    println!(
        "Does the {} have access? {}",
        guard.name,
        alice_clone.can_access()
    );
}
std::thread::sleep(std::time::Duration::from_secs(1));
```

This version of the code _is_ an improvement. We've eliminated a legitimate performance issue of over-locking a value. And if you run it, you might see output like the following:

```
Downloading a cute loading image, please wait...
Happy birthday! Person is now 16 years old.
Does the Alice have access? false
Happy birthday! Person is now 17 years old.
Downloading a cute loading image, please wait...
Happy birthday! Person is now 18 years old.
Does the Alice have access? true
Happy birthday! Person is now 19 years old.
Downloading a cute loading image, please wait...
Happy birthday! Person is now 20 years old.
Does the Alice have access? true
Downloading a cute loading image, please wait...
Happy birthday! Person is now 21 years old.
Happy birthday! Person is now 22 years old.
Does the Alice have access? true
Downloading a cute loading image, please wait...
Happy birthday! Person is now 23 years old.
Does the Alice have access? true
Happy birthday! Person is now 24 years old.
Happy birthday! Person is now 25 years old.
Downloading a cute loading image, please wait...
```

However, you may also see another deadlock message! So our change is a performance improvement, and makes it more likely for our program to complete without hitting the deadlock. But the deadlock is still present. But where???

## Why I thought this _isn't_ a deadlock

It's worth pausing one quick moment before explaining where the deadlock is. (And figurative hat-tip if you already know.) Our program has three threads of execution:

1. The deadlock detection thread. We know this isn't the cause of the deadlock, because we added that thread _after_ we saw the deadlock. (Though "deadlock detection thread leads to deadlock" would be an appropriately mind-breaking statement to make.)
2. The access check thread, which only does read locks.
3. The main thread, where we do the birthday updates. We'll call it the birthday thread instead. This thread takes write locks.

And my assumption going into our debugging adventure is that _this is perfectly fine_. The birthday thread will keep blocking waiting for a write lock. It will block as long as the access check thread is holding a read lock. OK, that's part of a deadlock: thread B is waiting on thread A. And the check access thread will wait for the birthday thread to release its write lock before it can grab a read lock. That's another component of a deadlock. But it seems like _each thread can always complete its locking_ without waiting on the other thread.

If you don't know what the deadlock is yet, and want to try to figure it out for yourself, go check out the [`RwLock` docs from the standard library](https://doc.rust-lang.org/stable/std/sync/struct.RwLock.html). But we'll continue the analysis here.

## How many read locks?

At this point in our real-life debugging, Sibi observed something: our code was less efficient than it should be. Focus on this bit of code:

```rust
let guard = alice_clone.inner.read();
println!(
    "Does the {} have access? {}",
    guard.name,
    alice_clone.can_access()
);
```

If we inline the definition of `can_access`, the problem becomes more obvious:

```rust
let guard = alice_clone.inner.read();
println!("Does the {} have access? {}", guard.name, {
    const MIN_AGE: u32 = 18;

    alice_clone.inner.read().age >= MIN_AGE
});
```

The inefficiency is that we're taking two read locks instead of one! We already read-lock `inner` to get the name, and then we call `alice_clone.can_access()` which makes its own lock. This is good from a code reuse standpoint. But it's not good from a resource standpoint. During our debugging session, I agreed that this warranted further investigation, but we continued looking for the deadlock.

Turns out, I was completely wrong. This wasn't just an inefficiency. **This is the deadlock.** But how? It turns out, I'd missed a _very_ important piece of the documentation for `RwLock`.

> This lock uses a task-fair locking policy which avoids both reader and writer starvation. This means that readers trying to acquire the lock will block even if the lock is unlocked when there are writers waiting to acquire the lock. Because of this, attempts to recursively acquire a read lock within a single thread may result in a deadlock.

Or, to copy from `std`'s docs, we have a demonstration of how to generate a potential deadlock with seemingly innocuous code:

```
// Thread 1             |  // Thread 2
let _rg = lock.read();  |
                        |  // will block
                        |  let _wg = lock.write();
// may deadlock         |
let _rg = lock.read();  |
```

This is _exactly_ what our code above was doing: the access check thread took a first read lock to get the name, then took a second read lock inside the `can_access` method to check the age. By introducing a sleep in between these two actions, we increased the likelihood of the deadlock occurring by giving a wider timespan when the `write` lock from the birthday thread could come in between those two locks. But the sleep was not the bug. The bug was taking two read locks in the first place!

Let's first try to understand why `RwLock` behaves like this, and then put together some fixes.

## Fairness and starvation

Imagine that, instead of a single access check thread, we had a million of them. Each of them is written so that it grabs a read lock, holds onto it for about 200 milliseconds, and then releases it. With a million such threads, there's a fairly high chance that the birthday thread will never be able to get a write lock. There will _always_ be at least one read lock active.

This problem is *starvation*: one of the workers in a system is never able to get a lock, and therefore it's starved from doing any work. This can be more than just a performance issue, it can completely undermine the expected behavior of a system. In our case, Alice would remain 15 for the entire lifetime of the program and never be able to access the system.

The solution to starvation is *fairness*, where you make sure all workers get a chance to do some work. With a simpler data structure like a `Mutex`, this is relatively easy to think about: everyone who wants a lock stands in line and takes the lock one at a time.

However, `RwLock`s are more complex. They have both read and write locks, so there's not really just one line to stand in. A naive implementation--meaning what I would have implemented before reading the docs from `std` and `parking-lot`--would look like this:

* `read` blocks until all write locks are released
* `write` blocks until all read and write locks are released

However, the actual implementation with fairness accounted for looks something like this:

* `read` blocks if there's an active write lock, _or_ if another thread is waiting for a write lock
* `write` blocks until all read and write locks are released

And now we can see the deadlock directly:

1. Access check thread takes a read lock (for reading the name)
2. Birthday thread tries to take a write lock, but it can't because there's already a read lock. It stands in line waiting its turn.
3. Access check thread tries to take a read lock (for checking the age). It sees that there's a write lock waiting in line, and to avoid starving it, stands in line behind the birthday thread
4. The access check thread is blocked until the birthday thread releases its lock. The birthday thread is blocked until the access check thread releases its first lock. Neither thread can make progress. Deadlock!

This, to me, is the worst deadlock I've encountered. Every single step of this process is logical. The standard library and `parking-lot` both made the correct decisions about implementation. And it still led to confusing behavior at runtime. Yes, the answer is "you should have read the docs," which I've now done. Consider this blog post an attempt to make sure that everyone else reads the docs too.

OK, so how do we resolve this problem? Let's check out two approaches.

## Easiest: read_recursive

The `parking-lot` crate provides a [`read_recursive`](https://docs.rs/lock_api/0.4.9/lock_api/struct.RwLock.html#method.read_recursive) method. Unlike the normal `read` method, this method will not check if there's a waiting write lock. It will simply grab a read lock. By using `read_recursive` in our `can_access` method, we don't have a deadlock anymore. And in this program, we also don't have a risk of starvation, because the `read_recursive` call is always gated _after_ our thread already got a read lock.

However, this isn't a good general purpose solution. It's essentially undermining all the fairness work that's gone into `RwLock`. Instead, even though it requires a bit more code change, there's a more idiomatic solution.

## Just take one lock

This is the best approach we can take. We only need to take one read lock inside our access check thread. One way to make this work is to move the `can_access` method from `Person` to `PersonInner`, and then call `can_access` on the `guard`, like so:

```rust
impl PersonInner {
    fn can_access(&self) -> bool {
        const MIN_AGE: u32 = 18;

        self.age >= MIN_AGE
    }
}

// ...


let guard = alice_clone.inner.read();
println!("Downloading a cute loading image, please wait...");
std::thread::sleep(std::time::Duration::from_secs(1));
println!(
    "Does the {} have access? {}",
    guard.name,
    guard.can_access()
);
std::thread::sleep(std::time::Duration::from_secs(1));
```

This fully resolves the deadlock issue. There are still questions about exposing the innards of our data structure. We could come up with a more complex API that keeps some level of encapsulation, e.g.:

```rust
use std::sync::Arc;

use parking_lot::{RwLock, RwLockReadGuard};

#[derive(Clone)]
struct Person {
    inner: Arc<RwLock<PersonInner>>,
}

struct PersonInner {
    name: String,
    age: u32,
}

struct PersonReadGuard<'a> {
    guard: RwLockReadGuard<'a, PersonInner>,
}

impl Person {
    fn read(&self) -> PersonReadGuard {
        PersonReadGuard {
            guard: self.inner.read(),
        }
    }

    /// Returns the new age
    fn birthday(&self) -> u32 {
        let mut guard = self.inner.write();
        guard.age += 1;
        guard.age
    }
}

impl PersonReadGuard<'_> {
    fn can_access(&self) -> bool {
        const MIN_AGE: u32 = 18;

        self.guard.age >= MIN_AGE
    }

    fn get_name(&self) -> &String {
        &self.guard.name
    }
}

fn main() {
    std::thread::spawn(move || loop {
        std::thread::sleep(std::time::Duration::from_secs(2));
        for deadlock in parking_lot::deadlock::check_deadlock() {
            for deadlock in deadlock {
                println!(
                    "Found a deadlock! {}:\n{:?}",
                    deadlock.thread_id(),
                    deadlock.backtrace()
                );
            }
        }
    });

    let alice = Person {
        inner: Arc::new(RwLock::new(PersonInner {
            name: "Alice".to_owned(),
            age: 15,
        })),
    };

    let alice_clone = alice.clone();
    std::thread::spawn(move || loop {
        let guard = alice_clone.read();
        println!("Downloading a cute loading image, please wait...");
        std::thread::sleep(std::time::Duration::from_secs(1));
        println!(
            "Does the {} have access? {}",
            guard.get_name(),
            guard.can_access()
        );
        std::thread::sleep(std::time::Duration::from_secs(1));
    });

    for _ in 0..10 {
        std::thread::sleep(std::time::Duration::from_secs(1));
        let new_age = alice.birthday();

        println!("Happy birthday! Person is now {new_age} years old.");
    }
}
```

Is this kind of overhead warranted? Definitely not for this case. But such an approach might make sense for larger programs.

## So when did we introduce the bug?

Just to fully answer the question I led with: we introduced the deadlock in the section title "What's in a name". In the real life production code, the bug came into existance in almost exactly the same way I described above. We had an existing helper method that took a read lock, then ended up introducing another method that took a read lock on its own and, while that lock was held, called into the existing helper method.

It's very easy to introduce a bug like that. (Or at least that's what I'm telling myself to feel like less of an idiot.) Besides the deadlock problem, it also introduces other race conditions. For example, if I had taken-and-released the read lock in the parent function before calling the helper function, I'd have a different kind of race condition: I'd be pulling data from the same `RwLock` in a non-atomic manner. Consider if, for example, Alice's name changes to "Alice the Adult" when she turns 18. In the program above, it's entirely possible to imagine a scenario where we say that "Alice the Adult" doesn't have access.

All of this to say: any time you're dealing with locking, you need to be careful to avoid potential data races. Rust makes it _so much nicer_ than many other languages to avoid race conditions through things like `RwLockReadGuard`, the `Send` and `Sync` traits, mutable borrow checking, and other techniques. But it's still not a panacea.
