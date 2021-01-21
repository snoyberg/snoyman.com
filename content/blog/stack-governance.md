+++
title = "Stack Governance"
path = "/blog/2021/01/stack-governance"
description = "Proposal and call for applicants for more structured Stack governance"
date = 2021-01-21
[extra]
ogimage = "stack-governance.png"
+++
A few months back I [wrote about the Haskell Foundation](@/blog/haskell-foundation.md) and some of my plans with it. Earlier than that, I also spoke about [my thoughts on transparency](@/blog/transparency.md). Continuing on those topics, I want to talk today about Stack's governance.

While it takes many things to make a project successful, clarity around __goals__ and __governance__ is vital. Let me provide two concrete examples I'm involved in:

* Yesod's goal is: provide a standard MVC web framework that leverages Haskell's type system to avoid large classes of bugs. Its governance is: there are a bunch of committers who can do things, and ultimately a (hopefully benevolent) dictator for life, me.
* Stackage's goal is: test large sets of open source Haskell packages for compatibility, making it easy for library users to pull in compatible package sets, and easy for authors to maintain their packages. Its governance is: there's a group of curators who come to consensus on decisions.

With clarity on these two points, it's fairly straightforward for people to decide if they want to use a project, if they're willing to contribute to it, and how to try to effect change. And if these points haven't been clear to others: sorry, transparency is hard.

The Stack project should also have such clarity, and it's lacking right now. There is a channel for discussions, and there are decision making processes. But they are fairly relaxed. Plenty of people have asked to see this solidified, and I agree with that. It's time to do so, and I'm intentionally carving out some time and energy to make that happen.

Below is my initial proposal. I wouldn't call it a strawman, but I also wouldn't call it complete. Feedback is welcome. And as you'll see, Stack maintainers are welcome as well!

## Stack goals

Stack's goals are:

* Provide easy to use tooling for Haskell development
* Provide complete support for at least the three primary development environments today: Linux, Mac, and Windows
* Address the needs of industrial users, open source maintainers, and other individuals
* Focus on the curated package set use case
* Prioritize reproducible build plans

These goals are modifiable over time, but largescale veering away should be avoided. Any significant changes should involve significant public discussion and a public vote by the Stack maintainer team.

## Committers

Stack encourages a wide range of people to be granted commit bits to the repository. Individuals are encouraged to take initiative to make non-controversial changes, such as documentation improvements, bug fixes, performance improvements, and feature enhancements. Maintainers should be included in discussions of controversial changes and tricky code changes.

Generally: **it's easier to ask forgiveness than permission**. We can always roll back a bad change.

## Maintainers

Maintainers are long term contributors to Stack. Maintainers are listed in the project's README.md file (or perhaps elsewhere and linked, TBD). Maintainers are recognized for contributions including:

* Direct code contribution
* Review of pull requests
* Interactions on the Github issue tracker
* Documentation management
* External support, e.g. hosting, training, etc

The maintainer team will make decisions when necessary, specifically:

* If there is disagreement on how to proceed on a specific topic
* Adding or removing a maintainer

Generally, maintainers are only removed due to non-participation or actions unhealthy to the project. The former is _not_ a punishment, simply a recognition that maintainership is for active participants only. The latter will hopefully never be necessary, but would include protection for cases of:

* Disruptive behavior in public channels related to Stack
* Impairing the codebase through bad commits/merges

Following the same principle of committers, maintainers are broadly encouraged to **make autonomous decisions**. Each individual maintainer is empowered to make a unilateral decision, again with the principle that a bad change can be rolled back. Maintainers should favor getting consensus first if:

* They are uncertain what the best course of action is
* They believe other maintainers or users of Stack will disagree on the decision

As the de facto "maintainer" right now for Stack, I will be the initial maintainer, and add others to this group quickly. Some additions are obvious: people who are currently, actively, maintaining the code base and issue tracker. I intend to add more maintainers to this group, to an unspecified number. I hope that adding maintainers will increase participation in the project too.

## Support

*This section should be discussed by the maintainer group once it's off the ground. Initial feedback is welcome.*

A large part of the issue tracker and general discussion around Stack is support topics. In my opinion, these kinds of discussions clog up the issue tracker and make it more difficult to maintain the project. I believe we should have a dedicated support area, and think [haskell.org's Discourse instance](https://discourse.haskell.org/) may be a good choice for this. I would encourage getting the issue tracker into a state of maintainability by closing out old issues. (I am well aware that this is a _highly_ contentious proposal.)

## Discussion

We should clarify all of the official Stack discussion areas, and what their purposes are. My initial proposal would be something like:

* Close down the current mailing list, and move to Discourse, in line with other Haskell projects
    * Discourse will be used for general feature discussion and support requests
* Issue tracker will be for concrete feature proposals, bug reports, and other code base discussions (e.g. refactorings)
* Pull requests will be for... pull requests :)
*   We should also create three public text chat channels, covering:

    1. General public discussion of any type
    2. Committer and maintainer discussions to work out specific code issues in real time. Everyone can read and write to this channel, but off topic requests will be turned elsewhere
    3. A maintainer only discussion, which is publicly viewable, but only intended for maintainers to contribute to. This will be rarely used, only when a controversial topic comes up

    I don't have strong opinions here, but the initial discussion about this topic in the Haskell Foundation leaned towards Matrix/Element, and I have no objection to that. Overall, I would like to make choices in line with Haskell Foundation choices.


## Bootstrapping

To get this off the ground, as mentioned above, I'll be selecting the initial maintainer group, and we'll put together the more official "Stack charter" based on my comments above. This will be informed by general community input.

## Providing feedback

One of the problems with these charter discussions is that we don't yet have an official place for the discussion to take place! I'll try to pay attention to:

* Reddit
* Twitter
* Discourse
* Stack mailing list

Unfortunately some messages will be missed. Apologies in advance.

## What you should do

If you want to see this take off, you should:

* Share this post
* Provide constructive feedback on how to improve the proposal
* Speak up if you want to be on the initial maintainer team, or if you think someone should be included in it
