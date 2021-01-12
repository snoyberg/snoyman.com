Every once in a while, a friend will share a comment from social media discussing in broad terms [Stackage](https://github.com/commercialhaskell/stackage#readme) and [Rust](https://www.rust-lang.org/). As the original founder of the Stackage project, it's of course fun to see other languages discussing Stackage. And as a Rustacean myself, it's great to see it in the context of Rust.

This topic has popped up enough times now, and I've thought about it off-and-on for long enough now, that I thought a quick blog post on the topic would make sense.

Before diving in, I want to make something very clear. I know exactly what drove me to create Stackage for Haskell (and I'll describe it below). And I've come to some conclusions about Stackage for Rust, which I'll also describe below. But I want to acknowledge that while I do write Rust, and I have Rust code running in production, it's nowhere near the level of Haskell code I have in production. I am well aware of the fact that my opinions on Rust may not match up with others in the Rust community.

I would be more than happy to engage with any Rustaceans who are interested in discussing this topic more, especially if you think I'm wrong in what I say below. Not only do I love the Rust language, but I find these kinds of package coordination topics fascinating, and would love to interact with others on them.

## What is Stackage?

Stackage's two word slogan is "Stable Hackage." Hackage is the de facto standard repository for open source Haskell libraries and tools. It would easily be described as Haskell's version of Crates. Stackage is a project which produces versioned snapshots of packages, together with underlying compiler versions, which are guaranteed to meet a basic buildability standard. That essentially comes down to:

* On a Linux system, using a Docker container we've configured with a number of helper system libraries, the package compiles
* Unless told otherwise, the docs build successful, the tests build and run successfully, and the benchmarks build successfully

There are plenty of holes in this. Packages may not build on Windows. There may be semantic bugs that aren't picked up by the test suite. It's far from saying "we guarantee this package is perfect." It's saying "we've defined an objective standard of quality and are testing for it." I'm a strong believer that having clearly defined specifications like that is a Very Good Thing.

There are plenty of other details about how Stackage operates. We produce nightly snapshots that always try to grab the latest versions of packages, unless specifically overridden. We produce Long Term Support (LTS) versions that avoid new major versions of packages during a series. We manually add and remove packages from the "skipped tests" based on whether the tests will run correctly in our environment. But most of those kinds of details can be ignored for our discussion today. To summarize, if you want to understand this discussion, think of it this way:

> Stackage defines snapshots where open source libraries and tools are tested to build together and pass (most of) their test suites

## Why I created Stackage

It's vital to understand this part to understand my position on a Stackage for Rust. Stackage has an interesting history going through a few previous projects, in particular Haskell Platform and Yesod Platform. Ignoring those for the moment, and pretending that Stackage emerged fully formed as it is today, let's look at the situation before Stackage.

The Haskell community has always embraced breakage more than other communities. We rely heavily on strong types to flag major changes. The idea of a build time failure with a clear compiler error message is much less intimidating from a silent behavior change or a type error that only appears at runtime. In our pursuit of elegance, we will typically break APIs left, right, and center. I, more than most, have been historically guilty of that attitude. (I'm now of a very different opinion though.)

This attitude goes up the tree to GHC (the de facto standard Haskell compiler), which regularly breaks large swaths of the open source code base with new versions. This kind of activity typically leads to a large cascade effect of releases of userland packages, which ultimately leads to a lot of churn in the ecosystem.

At the time I created Stackage, my primary focus was the Yesod Web Framework. I had made a decision to design Yesod around a large collection of smaller libraries to allow for easier modularity. There are certainly arguments to be made for and against this decision, but for now, the important point is: Yesod relied on 100-150 packages. And I ran into many cases where end users simply could not build Yesod.

At the time, the only real Haskell build tool was `cabal`. It featured a dependency solver, and respected version bounds for dependencies. In principle, this would mean that `cabal` should solve the problem for Yesod just fine. In practice, it didn't, for two important reasons:

1. At the time, there were some fundamental flaws in `cabal`'s depedency solver. Even when a valid build plan was available, it would on a regular basis take more than an hour to run without finding it.
2. Many packages did not include any information on dependency versions in their metadata files, instead simply depending on any version of a dependency.

For those familiar with it: point (2) is definitely the infamous PVP debates, which I've been involved with a lot. Again, I'm not discussing the trade-offs of the PVP, simply stating historical facts that led to the creation of Stackage.

So, as someone who believed (and still believes) Yesod is a good thing, Haskell is a good thing, and we should make it as easy as possible to get started with Haskell, solving these "dependency hell" issues was worthwhile. Stackage did this for me, and I believe a large percentage of the Haskell community.

Later, we also created the Stack build tool as an alternative to `cabal`, which defaulted to this snapshot-based approach instead of dependency solving. But that topic is also outside the scope of this blog post, so I won't be addressing it further.

## Advantages of Stackage

There are some great advantages of to Stackage, and it's worth getting them out there right now:

* Virtually no dependency hell problems. If you specify a Stackage snapshot and only use packages within that snapshot, you're all but guaranteed that your dependencies will all build correctly, no futzing required. Depending on how well maintained the libraries and their metadata is in your language of choice, this may either be a minor convenience, or a massive life changing impact.
* It's possible to create [documentation sites](https://www.stackage.org/lts-15.3) providing a coherent set of docs for a single package, where links in between packages always end up with compatible versions of packages.
* For training purposes (which I do quite a bit of), it's an amazing thing to say "we're using LTS version 15.3" and know everyone is using the same compiler version and library versions.
* Similarly, for bug reports, it's great to say "please provide a repo and specify your Stackage snapshot."
* It's possible to write [Haskell scripts](https://www.fpcomplete.com/haskell/tutorial/stack-script/) that depend on extra library dependencies and have a good guarantee that they'll continue to work identically far into the future.

That sounds great, but let's not forget the downsides.

## Disadvantages of Stackage

Not everything is perfect in Stackage land. Here are some of the downsides:

* It's a maintenance burden, like anything else. I think we've done a decent job of automating it, and with the wonderful team of [Stackage Curators](https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md) the burden is spread across multiple people. But like any project, it does require time, investment, and upkeep.
* Stackage doesn't nicely handle the case of using a package outside of the snapshot.
* It introduces some level of "community wide mutex" where you need to either wait for dependent libraries to update to support newer dependencies, or drop those libraries. It's rarely a big fight these days, since we try to stick to some kind of defined timetables, but it can be frustrating for people, and it has sometimes led to arguments.
    * This brings up one of my guiding principles: avoid these kinds of shared resources whenever possible

OK, so now we understand the pluses and minuses of Stackage. And in the context of Haskell, I believe it was at the time absolutely the right thing to do. And today, I would not want to stop using Stackage in Haskell at all.

But the story is different in Rust. Let me step through some of the motivators in Haskell and how they differ in Rust. Again, keep in mind that my experience in Haskell far outstrips my experience in Rust, and I very much welcome others to weigh in differently than me.

## Cargo/SemVer vs PVP

What's wrong with this line from a Haskell Cabal file?

```cabal
build-depends: base, bytestring, text
```

Unless you're a hard core Haskeller, probably nothing obvious jumps out at you. Now look at this snippet of a perfectly valid `Cargo.toml` file and tell me what's wrong:

```toml
[dependencies]
random = ""
tokio = ""
```

Even a non-Rustacean will probably come up with the question "what's with the empty strings?" Both of thse snippets mean "take whatever version of these libraries you feel like." But the syntax for Haskell encourages this. The syntax for Rust discourages it. Instead, the following `Cargo.toml` snippet looks far more natural:

```toml
[dependencies]
random = "0.12.2"
tokio = "0.2.22"
```

When I first started writing Haskell, I had no idea about the "correct" way to write dependency bounds. Many other people didn't either. Ultimately, years into my Haskell experience, I found out that the recommended approach was the PVP, which is essentially Haskell's version of SemVer. (To be clear: the PVP predates SemVer, this was not a case of Not Invented Here syndrome.)

Unfortunately, the PVP is quite a complicated beast to get right. I've heard from multiple Haskellers, with a wide range of experience, that they don't know how to properly version their packages. To this day, the most common response I'm likely to give on a Haskell pull request is "please change the version number, this doesn't comply with the PVP." As a result, we have a situation where the easy, obvious thing to do is to include no version information at all, and the "correct" thing to do is really, really hard to get right.

By contrast, in Rust, the simple thing to do is mostly correct: just stick the current version of the package in the double quotes. The format of the file basically begs you to do it. It feels weird _not_ to do it. And even though it's not perfect, it's pretty darn good.

So first point against a Stackage for Rust: **Cargo's format encourages proper dependency information to be maintained**.

## Dependency solver works

This is where my larger Haskell experience may be skewing my vision. As I mentioned, I've had huge problems historically with `cabal`'s dependency solver. It has improved massively since then. But between the dependency solver itself having historic bugs, and the lack of good dependency information across Hackage (yes, in large part because of people like me and the presence of Stackage), I don't like to rely on dependency solving at all anymore.

By contrast, in Rust, I've rarely run into problems with the dependency solver. I have gotten build plans that have failed to build. I have had to manually modify `Cargo.toml` files to specify different versions. I have been bitten by libraries accidentally breaking compatibility within a major version. But relative to my pains with Haskell, it's small potatoes.

So next point: **Dependency solving emperically works pretty well in Rust**

## Culture of compatibility

In contrast to Haskell, Rust has much more of a culture around keeping backwards and forwards compatibility. The compiler team tries very hard to avoid breaking existing code. And when they do, they try to make it possible to rewrite the broken code in a way that's compatible with both the old and new version of the compiler. I've seen a lot of the same attitude from Rust userland packages.

This may sound surprising to Rustaceans, who probably view the Rust library ecosystem as a fast-changing landscape. I'm just speaking in relative terms to Haskell.

As a result, the level of churn in Rust isn't nearly as high as in Haskell, and therefore even with imperfect dependency information a dependency solving approach is likely to work out pretty well.

**Less library churn means code builds more reliably**

## Lock files

A minor point, but Cargo creates a `Cargo.lock` file that pins the exact versions of your dependencies. It's up to you whether you check that file into your code repo, but it's trivially easy to do so. Paired together with `rust-toolchain` files, you can get pretty close to reproducible build plans.

I can't speak to the latest versions of `cabal`, but historically this has not been the default mode of operation for `cabal`. And I don't want to get into the details of Hackage revisions and timestamp-based development, but suffice it to say I have no real expectations that you can fully rely on `cabal` freeze files. Stack _does_ support fully reproducible build plans and lock files, but that's essentially via the same snapshot mechanism as Stackage itself.

In other words: **In the presence of proper lock files, you regain reproducible builds without snapshots**

## Conclusion

I hope this post frames my thoughts around Stackage, Haskell, and Rust pretty well. My basic conclusion is:

* Snapshots are good thing in general
* Snapshots are less vital when you get other things right
* Rust has gotten a lot of things right that Haskell didn't
* As a result, the marginal benefit of snapshots in Rust are nowhere near the marginal benefits in Haskell

To restate: my calculations here are biased by having much more experience with Haskell than Rust codebases. If I was running the same number of lines of Rust code as Haskell, or teaching the same number of courses, or maintaining as many open source projects, I may not come to this conclusion. But based on what I see right now, the benefits of a snapshot system in Rust do not outweigh the costs of setting up and maintaining one.

All that said: I'm still very interested to hear others' takes on this, both if you agree with my conclusion, and even more if you don't. Please feel free to ping me on social media discussions, the Disqus comments below, or my personal email (michael at snoyman dot com).
