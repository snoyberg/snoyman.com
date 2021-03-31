+++
title = "Haskell base proposal, part 2: unifying vector-like types"
description = "My summary of discussions until now"
path = "/blog/2021/03/haskell-base-proposal-2"
date = 2021-03-31
+++
[**Discourse thread for discussion**](https://discourse.haskell.org/t/vector-type-proposal-discussion-blog/2272?u=snoyberg)

Two weeks back, I [wrote a blog post](@/blog/haskell-base-proposal.md) with a proposal for unification of vector-like types in `bytestring`, `text`, and `vector`. This continued with a [discussion on Discourse](https://discourse.haskell.org/t/base-proposal-around-vector-like-types/2112), and has been part of some brainstorming sessions at the Haskell Foundation tech track to lock down some of the concrete details. ([Meeting minutes](https://discourse.haskell.org/t/tech-agenda-track-meeting-minutes-3-24/2257) have been posted to Discourse regularly.) I've discussed with a few other interested parties, and invited feedback from people who have been working on related projects. (And I even received some such feedback!)

At this point, I wanted to summarize what's come up, and propose some concrete next steps. Also, since this is one of the first such proposals we're trying to get to happen through the Haskell Foundation, I'll take a step back with some meta comments about the process itself.

## Refined goals

I threw quite a bit into the original proposal. The brainstorming since has helped make it clear what can be achieved, what will have major breaking impacts, and what will deliver the most value. Here are some outcomes:

* As much as I still think a single, unified `Vector` type that does boxing or unboxing depending on the contained type would be a Very Good Thing, there are technical hurdles, and not everyone is bought into it. I doubt it's going to happen any time soon.
* The temporary pinning of memory is likely not going to happen, since it would be too big a GC change. However, larger `ByteArray#`s (3kb IIRC) are already pinned, and using the copy-when-unpinned technique will not be too expensive for things under 3kb. So we have a path forward that requires no (or few) changes to GHC.
* It seems like one of the biggest advantages we may get out of this proposal is to move `ByteString` to unpinned memory. This would be good, since it would reduce memory fragmentation. The downside is the need to copy smaller `ByteString`s before performing FFI calls. But overall, as mentioned previously, we don't think that will have a major performance impact.
* There are already efforts underway, and have been for a while, to rewrite `text` to use UTF-8. Combined with this proposal, we could be seeing some serious improvements to how textual data is consumed and worked with in Haskell, but that's a bit too far off right now.

## Refined design

With that in place, a semi-coherent design is beginning to form around this proposal:

* We're not ready to move types into `base` yet, but fortunately we already have another package that's a good candidate for shared-vector-types: `primitive`. It's already used by `vector`, and can be used by `bytestring` and `text`.
* We have a `PrimArray` type present in `primitive`, but it doesn't support slicing. Let's add a `PrimVector` type with the minimal machinery necessary to get slicing in place.
* The big change: let's rewrite `ByteString` (the strict variant) to be `newtype ByteString = ByteString (PrimVector Word8)`.
    * We can recover backwards compatibility in most of the package, including in the `.Unsafe` module.
    * People directly using the `.Internal` module will likely be broken, though there may be some clever tricks to recover backwards compat there too.
    * We'll get the non-memory-fragmentation benefit immediately.
* Not yet discussed, but putting down for future brainstorming: what should we do with `ShortByteString`? If we move it over to `PrimVector`, it will end up with a little more overhead for slicing. Do we leave it alone instead? Move it to `PrimArray`?

There are additional steps I could write around `text` and `vector`, but honestly: let's stop it right there. If we get a working `bytestring` package on top of `primitive`'s `PrimVector`, I think that's a great time to take a break, analyze the performance impact, and see ecosystem impact, likely by compiling a Stackage snapshot with the tweaked `bytestring` and `primitive` packages.

## Action items

Next steps: find out who's interested in doing the work and dive in! This is still proof of concept, so no real buy-in is needed. We're exploring a possibility. There's a bunch of code that needs to be rewritten in `bytestring` to see if this is possible.

And while brainstorming in calls has been good, I don't think it's great. I'd like to move to discussions in a chat room to make it easier for others to engage. I'll comment a bit more on this below.

## Other packages

I've reached out to some authors of other packages to get their input on this proposal. I've received some, and incorporated some of that here. For example, both Alexey Kuleshevich and Alexey Lelechenko proposed using `primitive` as the new home for the shared data types. Others have expressed that they'd rather first see a concrete proposal. We'll see if there is further collaboration possible in the future.

## Process level comments

Let me summarize, at a high level, what the process was that was involved in this proposal:

1. Free-form discussions on the Haskell Foundation tech track in a live meeting. People discussing different ideas, different concerns, which ultimately triggered the idea of this proposal.
2. Blog post to the world and Discourse thread laying out some initial ideas and looking for feedback.
3. Public discussion.
4. A few private emails reaching out to specific interested parties to get their input.
5. In concert with (3) and (4): further tech track live meetings to discuss details further.
6. This second blog post (and associated Discourse thread) to update everyone and call to action.
7. Hopefully: some actual coding.

Overall, I think this is a good procedure. If I could make one change, it would be towards leveraging asynchronous communication more and live meetings less. I absolutely see huge value in live meetings of people to brainstorm, as happened in (1). But I think one of the best things we can do as the Haskell Foundation is encourage more people to easily get involved in specific topics they care about.

On the bright side, the significant usage of Discourse for collaboration and reporting on meeting minutes has been a Good Thing. I think blog posts like this one and my previous one are a Good Thing for collecting thoughts coherently.

That said, I realize I'm in the driver's seat on this proposal, and have a skewed view of how the outside world sees things. If people have concerns with how this was handled, or ideas for improvement, bring them up. I think figuring out how to foster serious discussion of complex technical issues in the Haskell ecosystem is vital to its continued success.
