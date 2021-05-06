+++
title = "Haskell proposal: unified installer"
description = "A new proposal from the Haskell Foundation tech track, on a unified installer for Haskell tooling"
path = "/blog/2021/05/unified-haskell-installer"
date = 2021-05-06
+++
**`<quick-intro>`** In my [previous post](@/blog/haskell-foundation-board-meeting-minutes-april-8.md), I mentioned that we were opening up the Haskell Foundation Slack instance for more people. I'd like to emphasize that a second time, and mention that we are trying to use Slack as a tech track communication hub. We're actively looking for contributors to join up on some of these initiatives and bring them to reality. If the previous topics I've mentioned (like the Vector Types Proposal) or topics like improving Haskell educational material sound interesting, please [join the Slack](https://join.slack.com/t/haskell-foundation/shared_invite/zt-mjh76fw0-CEjg2NbyVE8rVQDvR~0F4A), introduce yourself in `#tech-track`, and tell us what you'd be interested in helping out with. You'll get to be part of some hopefully significant improvements to the Haskell ecosystem, and have access to a lot of other collaborators and mentors to make this happen. **`</quick-intro>`**

Also, we're working to get a unified listing of the active projects and what kind of contributors we're looking for, so stay tuned for that too. Anyway, on to the newest proposal!

## Stack

There are currently multiple ways to install Haskell tooling, and not all of them work for everyone. Personally, I've worked on two such methods: first [MinGHC](https://github.com/fpco/minghc) (a Windows installer), and later Stack, which subsumed MinGHC. Stack combines both a build tool and a toolchain installer, and manages GHC installations as well as&mdash;on Windows&mdash;install [msys](https://www.msys2.org/). And if it seems like Windows is coming up a lot here... that's a theme.

__Side note__ What's msys? Why do we need it? msys is essentially a bunch of POSIX utilities ported to run on Windows. The most basic problem is solves is allowing packages with `configure` scripts, such as `network`, to be installed on Windows. It can also be used to provide a package manager for getting shared libraries on Windows. For this post, take as a given that having msys on Windows is basically a necessity.

Anyway, with Stack, you have a set of different installers for the Stack binary itself: a GUI installer for Windows or a `curl`-able script for POSIX-like systems. Stack then can install GHC and `msys2` into user-specific directories, and provides commands like `stack ghc` or `stack --resolver ghc-8.10.4 exec` to run commands with the appropriate toolchain configured in environment variables.

There are basically three problems with this setup:

1. Some people really don't like user-specific installs, and want tooling installed system-globally. Or they really want package managers to maintain toolchains. Or a few other such goals. There are inherent problems with some of those goals, such as distributions having out-of-date installers, or permissions management, or others. The per-user toolchain installer has become pretty standards across different languages (Python, Ruby, Rust, etc), and is unlikely to change any time soon.
2. Some people really don't like typing in `stack ghc` instead of `ghc`. I'm overall a fan of needing the wrapper tooling, since it allows you to trivially switch which toolchain you're using, and makes it more blatant that certain environment variables (like `STACK_YAML`) and files (like `stack.yaml`) may affect behavior.
3. Stack isn't the only tool in town. For example, many people want to use `cabal-install` instead. Stack doesn't have support for downloading `cabal-install` binaries right now. Even if it did, it's not clear that the `cabal-install` team will consent to an installer that leverages Stack like this. But there are even _more_ binaries we may wish to install and manage going forward, such as linters, stylers, IDE integration tools, etc.

But if you're using Stack to build your projects, "how do I install GHC and associated tools" is basically a solved problem. Perhaps (2) [irritates some people](https://github.com/commercialhaskell/stack/issues/5515), but we'll get back to that. Perhaps you don't like `curl`-able scripts, or Windows installers, or user-specific installs. You can go ahead and manually download the Stack executable and use things like `{ system-ghc: true, install-ghc: false }`. But from my experience, most people just use the defaults and are happy enough.

## Everything else

But as mentioned, not everyone is on Stack. And there's other tooling we'd like to be able to install. And we don't want to bifurcate our install methods too much. And it's kind of weird that Stack is both a toolchain manager _and_ a build tool. And frankly, it would be nice to finally get a downloads page that everyone can agree with.

There _are_ other approaches to installing toolchains available today. `ghcup`, Haskell Platform (to some extent), manually `./configure && make install`ing GHC bindists, distro specific methods, etc. Some of these solves some of the three problems I mentioned with Stack. Some solve other problems as well. Some create new problems. But a recurring theme in particular that the Haskell Foundation members have heard is that the story for non-Stack users on Windows is particularly painful.

Let me clarify that I'm wearing two hats with what I'm about to say:

1. A maintainer of Stack and the original author of much of Stack's toolchain install logic, who would always rather see the Stack codebase size go down instead of up.
2. A member of the Haskell Foundation board, who is looking to improve the Haskell adoption story.

So the problem statement: can't we standardize an install method that meets most people's goal, most of the time?

## The goal

I'll start by laying out the ultimate goal that's in my head. This idea's been bouncing around for a few months now, but only came up in May 4, 2021 tech track meeting for the Haskell Foundation. I don't think it's that ambitious of a goal. But in the next section, I'm also going to lay out some shorter term milestones.

What I'd like to see is a well maintained Haskell library for installing Haskell toolchains, generally acknowledged as the de facto standard way to install GHC and other tools. It should be Haskell, so that it's easy for Haskellers to maintain. It should support as many operating systems as possible. It should provide an easy-to-use Haskell API that other tooling can leverage.

In addition, we should have some executables built against that library that provide a nice command line interface for managing toolchains. Some examples of these executables would be:

* The primary installer executable, that let's you say things like "install GHC 9.0.1" and "run the following command with env vars set so that it will use GHC 9.0.1"
* A `ghc`/`ghc.exe` shim executable that does the same thing as above, but doesn't require a longer command line like `haskell-install --ghc 9.0.1 --install-ghc exec -- ghc -O2 Main.hs`. Instead, with this `ghc`/`ghc.exe` shim, all of that would be implied. How does it know what GHC version to use? How do you override it? All fair questions I'm going to gloriously punt on for now.
* We can build out additional shim executables for other common tools, like `runghc`, or `ghci`, or... and this is the cool part... shim executables for Stack and `cabal-install`.

Next, we're going to build installers for all of these executables. On Windows, it will be a GUI installer. On Linux, OS X, and others, we can take our pick, though starting with `curl`-able scripts makes the most sense in my opinion.

Stack no longer will have logic in its codebase for installing GHC. Instead, it will rely on this new library. Or, perhaps, it won't even do that. Perhaps it will be the new tool's job to ensure the correct GHC is available for Stack invocations. I'm not quite sure.

`cabal-install` doesn't currently maintain a GHC toolchain, so I think it would fit into this model perfectly without any need for modification.

## The milestones

The biggest pain point we want to immediately solve is to get a Windows installer out the door. The short term goal for this is to provide a brand new executable, build against [Stack.Setup](https://www.stackage.org/haddock/nightly-2021-05-05/stack-2.5.1.1/Stack-Setup.html), and build an installer around it. Maybe that installer will include recent `cabal-install` and Stack executables. The details on what exactly it will do are still slightly fuzzy. But the point is: we're just writing an executable that reuses existing installer logic within Stack's library.

The next step is creating the new installer library. My recommendation is to simply cleave off the aforementioned `Stack.Setup` module into a new package and then modify as needed. Modifications may include adding support for installing other tools, updating the location of config files for specifying toolchain binaries, or more. But `Stack.Setup` is a module that's been around for a long time now, and has worked out a lot of these details. I'd advise against recreating the wheel here.

With that in place, we can begin to work on the other shim executables mentioned above, and begin to look at how this new installer library and the shim executables interact with the rest of the installer ecosystem. That's an open discussion yet to happen.

And in parallel to that, the Stack codebase itself can begin its move over to the new library.

## Inspiration

I'm trying to avoid sounding like a broken record, but a lot of the inspiration for my thoughts here come from [rustup](https://rustup.rs/). Honestly though, most of the ideas of how this installer should work predate Rust, and come from a combination of experience with MinGHC and seeing how tools like [virtualenv](https://pypi.org/project/virtualenv/) and [RVM](https://rvm.io/) work.

## Get involved!

I'm going to put this blog post on [Discourse](https://discourse.haskell.org/), as I've been doing with previous posts. But I'm going to strongly encourage anyone interested in this to join up on Slack too. We've created a new channel for this topic, `#tech-track-installer`. If you're interested in the topic, please join up!

I'm not just asking for people to provide some feedback, though that's welcome. I'm looking for people to contribute, work on code, and more broadly, take ownership of this work. I think this is a relatively small, but massively vital, piece of Haskell infrastructure we're trying to solidify. Someone looking to get deeper into open source maintenance would be great. And you'll have plenty of people available to provide guidance when desired.

## Stack/cabal-install/GHC mapping

One final idea I was playing with, and thought I'd leave as more of a footnote. One of the painpoints in maintaining build tooling (Stack or cabal-install) is maintaining backwards compatibility with old versions of GHC. However, other languages don't always do that. For example, with Rust, each new compiler version gets a new Cargo (the build tool) version:

```
> cargo --version
cargo 1.51.0 (43b129a20 2021-03-16)
> rustc --version
rustc 1.51.0 (2fd73fabe 2021-03-23)
```

With a toolchain installer in place like I'm describing, we could end up in that reality for Haskell too. Perhaps we could have a config file that essentially says, for each GHC version, what the recommended version is of cabal-install, Stack, msys2, stylish-haskell, and a slew of other tools. You could override it if desired, but the default would be the right thing for most people. Talking about a unified "version 9.2.1" Haskell toolchain that represents the compiler, standard libraries, and add-on tooling sounds like a huge step forward for easy adoption, at least to me.
