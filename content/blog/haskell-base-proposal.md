+++
title = "Haskell base proposal: unifying vector-like types"
description = "A semi-concrete, semi-fleshed out proposal for changes to GHC and base to unify vector, bytestring, and text"
path = "/blog/2021/04/haskell-base-proposal"
date = 2021-03-16
+++
__DISCUSSION__ I've opened up a [Discourse thread to discuss this post](https://discourse.haskell.org/t/base-proposal-around-vector-like-types/2112). I'll likely respond to comments here and on Twitter, but I'd like to move towards using the more official discussion channels when possible.

One of my greatest hopes for the Haskell Foundation is already beginning to be realized: better communication. I'm a member of the HF board and on the technical track. I've also been involved in some Discourse threads, and in particular [two](https://discourse.haskell.org/t/call-for-ideas-forming-a-technical-agenda/1901/33) [relevant](https://discourse.haskell.org/t/towards-a-prevalent-alternative-prelude/1986/74) threads for this blog post. I'm seeing a level of collaboration, interaction, and discussion of technical issues that has been lacking until now. I'm happy to see it happening, and hoping to see it continue.

One of the most animated discussions has been around the topic of revising `base`, a new standard library, and alternative preludes. Plenty of my [previous](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/) [blog](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/) [posts](https://www.snoyman.com/blog/2020/12/haskell-bad-parts-3/) have already touched on my thoughts, but in a negative light: here are some problems. Today, I want to focus in on one area with a concrete solution. And I'm choosing this because I think it's an underappreciated problem, and one that we can actually solve, now that we have real collaboration between GHC, core library maintainers, and alternative prelude authors.

## The problem

If you want to represent a sequence of data in Haskell today, you have too many options:

* A lazy list
* A `Seq` (which, unfortunately, almost no one uses)
* A strict and lazy `ByteString` for `Word8`s
* A strict and lazy `Text` for `Char`s
* A boxed vector (plus its mutable version)
* An unboxed vector for types with `Unbox` instances
* A storable vector for types with `Storable` instances
* `Builder`s for constructing data, both in `bytestring` and (unfortunately, more on that later) `text` flavors

I know this, because it's part of my [Applied Haskell training](https://www.fpcomplete.com/haskell/tutorial/data-types/), and I always feel a bit embarrassed having to explain all of this. It's a lot of cognitive overhead for new users, the libraries somewhat fight over which thing to use, and there are some hidden traps and performance costs. For example:

* Using a `ByteString` for small, long-lived data can lead to memory fragmentation due to pinned memory usage (which is why `ShortByteString` exists... something I didn't even mention above)
* `Text` and `ByteString` have completely different representations, and therefore converting between them is an expensive operation. Note that, contrary to popular belief, switching to UTF-8 in `text` won't solve this (though it's still a good idea for reducing memory usage). The problem is, again, pinned versus unpinned memory.
* The only packed representation of data that works for everything is a boxed `Vector` (ignoring other packages like `array` and `massiv`), but that boxing is a ridiculously high overhead for many cases.
* Choosing between an unboxed and storable vector is fairly tricky for newcomers, and creating appropriate instances of `Unbox` and `Storable` is far from trivial.

Compare this to most other languages, where there's a canonical array, vector, or similar type which is basically used for everything. That's the story I _wish_ we had in Haskell. The question is: can we get there?

## To split or not to split?

The first thing that comes up in these discussions is a fairly large question about splitting base. This comes down to the idea that, if we split `base` into multiple smaller packages, everything will get better. I'm going to challenge this in two different ways:

1. It's an orthogonal question. It _seems_ related because we can iterate faster on these changes if the changes can exist outside of the GHC release cycle. But I think we have a much better approach, which I'll discuss below.
2. As I've mentioned elsewhere (not sure where), there's a false sense of immutability about `base`. People are afraid to change it due to breaking backwards compatibility. I'm in favor of backwards compatibility, and like that approach overall. However, GHC itself breaks things so regularly that it's almost irrelevant if `base` has breakage as well. I'd love to see that fixed, but I don't see it as a blocker.

Given that the "split base" concept has been around for about as long as I've been using Haskell and still hasn't happened, I'm not going to hold my breath on it for this proposal.

## Unifying ByteString, Text, and Vector

Going back to that list of types above, I'm going to ask everyone to _ignore_ lazy lists and `Seq` as, currently, unrelated data types. They represent non-memory-packed data. Similarly, I want to ignore the lazy variants of `ByteString` and `Text` too. I think we should stop using them, but that's a discussion for another day. Instead, I want to focus in on `Vector` and its variants, `ByteString`, `Text`, and `Builder`. I also want to focus on the mutable variants and the `Unbox` and `Storable` typeclasses.

First, it's long been recognized that `ByteString` is, essentially, synonymous with a storable `Vector` of `Word8`s. Before bytestring 0.11, the only real difference was that there's a slight optimization in `Vector` to avoid holding onto an offset value. But the two are isomorphic, and since 0.11 that optimization exists in both packages. Therefore, representing `ByteString` as `Data.Vector.Storable.Vector Word8` is doable.

Similarly, `text` implements its own unpacked array implementation. That could be replaced with `vector`'s unboxed array.

So on our first step in this journey, my recommendation is simple: let's unify these types! For ease of transition, we can think of `ByteString` and `Text` as newtype wrappers for now. But as you'll see in the continuing discussion, I really have different plans.

## Unify unboxed and storable vectors

Credit where credit is due: Vincent Hanquez figured this out a while ago with his `foundation` package.

Unboxed and storable vectors differ in whether memory is pinned or not. Pinned memory is good, because it lets you share data with the FFI without the garbage collector moving it. But it has the downside that it can lead to heap fragmentation. And this is a real issue in real world codebases. (Side note: unpinned memory _can_ be used with unsafe FFI calls, but not generally.)

As a result of this, users need to choose which representation to use. Type authors need to implement both `Unbox` and `Storable` instances, which is non-trivial.

I would love to see a few improvements here, which are where GHC comes into play:

* Make it possible to have temporarily-pinned memory. I'd love to be able to allocate a byte buffer, mark it initially as unpinned, and then while making an FFI call temporarily pin the memory. This would allow us to have just one unboxed vector representation that subsumes both unboxed and storable vectors. (Alexey Kuleshevich has provided some [additional problems](https://github.com/snoyberg/snoyman.com/pull/15#discussion_r594950679) for the curious.)
* I'd love to see some built-in ability to derive an instance that can be used by this kind of byte buffer. This may sound too special cased, but let me push back on that idea: virtually every other language out there with unboxed memory representations will natively let you write your data into raw bytes.

There would be some potential limitations with those instances. Maybe it only works with fully strict fields. Maybe it requires constant-size representation (ala Rust's `Sized` trait and Dynamically Sized Types). But my goal is that, for the common cases of data that can be represented easily as raw bytes:

* Almost no overhead is necessary to derive appropriate instances
* There's a single obvious `Vector` type to use for the representation

And with that change in place, we no longer have a distinction between the representations of `Text` and `ByteString`. A `Text` in fact turns into a simple newtype around `ByteString` with some rules of character encoding (ideally UTF-8). This would avoid a lot of performance overhead around functions like `encodeUtf8` and `decodeUtf8`, and hopefully make the entire I/O system just a bit easier to work with.

## Automatic Boxed vs Unpacked selection

If I have a `Vector a` with no constraints on `a`, it _must_ be a boxed `Vector`. This is unfortunate, because for many types this is a very poor representation. We would like some method to automatically select the best representation for a `Vector`. There are some approaches right now that can allow this, using either associated types or closed type families. But these approaches either require explicit associated types be declared for each type, or prevent creating new types with an unpacked vector.

I'd like to see some kind of a feature in GHC that allows closed type families but with some overriding. I don't have a concrete proposal. I know this will probably run into open-world/orphan issues. But this is the kind of feature that I think would be generally very useful, and I've seen lots of workarounds in many different libraries due to its absence.

With that, instead of needing to ever talk about boxed versus storable versus unboxed vectors, we would simply have a `Vector` type, and it would be able to choose the best representation.

Side note: we would probably need to analyze the strictness of boxed versus unpacked vectors a little more closely.

## Put it in `base`!

And finally, I think these types need to go into `base`. Yes, I already hear the arguments about split-base popping up. Let me clarify. I'm not saying the _first version ever_ of these types goes into `base`. Let's get the GHC changes necessary in place, write an external library providing these types, and then move them into `base`.

And I'm also quite directly talking about the _types_. We need to reduce the barrier to adoption for all good datatypes as much as possible. Right now, `Vector` is often not used. I think a large part of that is the so-many-vector-types problem I'm trying to solve here. Another is that it takes so darn long for `vector` to compile. People don't want to depend on it.

I'm not recommending we put all of the `vector` package into `base`. Far from it. I'm recommending we construct a well designed, hopefully-universal packed data representation, standardize it, and put it in `base` where it can be used by everything. Including by `base`!

As it stands today, most of the I/O functions included in `base` work on `String`s. We almost all agree that we shouldn't be using `String`, but its usage persists because it is the only `base`-approved type. Let's fix that.

With a solid `Vector` type in `base`, we can then begin to develop sets of functionality around that type in external libraries. People can iterate on those designs, and then we can consider standardizing, either in `base`, in a CLC-approved library, or in a new split-base world.

## Builders!

I was originally going to write about `Builder`s, and how they are one of the best kept secrets in Haskell. This post is already long enough, so I think I'll stop here. I'll save my thoughts for another time. But suffice it to say that I think we should be radically embracing `Builder` in `base`, and should be standardizing on a single representation. For some hints at my thoughts, check out `rio`:

* [`Utf8Builder`](https://www.stackage.org/haddock/lts-17.6/rio-0.1.20.0/RIO.html#t:Utf8Builder)
* [`Display`](https://www.stackage.org/haddock/lts-17.6/rio-0.1.20.0/RIO.html#t:Display)

## Adoption

This section was added at the suggestion of Ben Gamari. I hadn't realized that this post initially came off as a massive breaking change in the language and library ecosystem. So let me clarify. I believe that the changes above can be made immediately with zero breakage (though plenty of effort). The problem would be further fracturing the ecosystem ala the [classic XKCD](https://xkcd.com/927/). However, I think we have a great move here: compatibility shims.

In this world I'm picturing, most of the existing data types, and especially the two most common ones (strict `ByteString` and `Text`) can be expressed as newtype wrappers over the new types. `Vector` can probably have the same kind of shim too, though the strictness requirements I'm envisioning may change the semantics of boxed `Vector`s; I'm not sure yet.

In any event, that's my goal here: put these types deep into `base`, make them ubiquitous, and move most/all existing code over to use them.
