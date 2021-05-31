+++
title = "Stack on Slack and ARM64"
description = "Two updates on the Stack project: two new chat rooms on Slack, and building on ARM64"
path = "/blog/2021/05/stack-slack-arm"
date = 2021-05-31
[extra]
ogimage = "stack-slack-arm.png"
+++
This blog post is covering two completely unrelated announcements about [Stack](https://haskellstack.org/) that happened to land at the same time.

## Haskell Foundation Slack chat rooms

I've long wanted to have a good discussion area for Stack in text chat. We've tried Gitter, Keybase, and Matrix. All of them lacked sufficient critical mass to get moving. We had a chatroom for Stack collaborators on the FP Complete Slack instance as well. However, for quite a while, we've been trying to move away from that, since we'd much prefer a community owned and operated area.

As I mentioned in my last blog post, such a place exists now: the [Haskell Foundation Slack](https://join.slack.com/t/haskell-foundation/shared_invite/zt-mjh76fw0-CEjg2NbyVE8rVQDvR~0F4A). I've created two new chat rooms over there: `#stack-users` for general discussion, and `#stack-collaborators` for discussion about working on the code base itself. If you're interested, come join us over there!

And in case anyone is wondering about more broad collaboration between Stack and Haskell Foundation: that's absolutely the plan. We're working through details on that. As someone sitting in both camps (Stack maintainer _and_ Haskell Foundation board member) I can tell you that there's no real impediment to getting more official [affiliation](https://haskell.foundation/en/affiliates/), it's simply a matter of times and priorities.

Also: apologies to everyone else out there, you'll now also be able to regularly confuse the words "Stack and Slack" :).

## Linux ARM64 builds of Stack

We've had an on-again/off-again relationship with ARM builds of Stack for a while. It's never been much of a code issue; from my recollection, any time someone has tried to build Stack on an ARM machine it's worked without a hitch. The issue instead is getting a good CI setup working. GitHub Actions (and Azure Pipelines) really simplified the process of building and testing on Linux, Windows, and OS X. However, ARM builds were always a bit more painful.

That's why I was so excited to read about [Oracle Cloud's Ampere A1 chips](https://www.oracle.com/cloud/compute/arm/). It was fairly straightforward to get a GitHub Actions runner onto one of these machines and configure CI task to generate ARM builds. This is something I've wanted to play with some more for a while, but the relatively weak ARM machines I could get access to always made this tedious. Getting a successful CI build in under an hour is a game changer.

Some caveats, however. This is only a Linux, dynamically linked, ARM64 build. There's no Windows support here, or Alpine Linux (like we use for x86 Linux builds), or 32-bit ARM support. And this is all early days, and may not last. Hopefully this simply runs smoothly.

If you're interested, you can [grab an ARM64 bindist from GitHub releases now](https://github.com/commercialhaskell/stack/releases/download/v2.7.1/stack-2.7.1-linux-aarch64.tar.gz).

And tying together these two points: if you're interested in ARM64 support, feel free to drop in on Slack and say hi.
