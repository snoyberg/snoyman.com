+++
title = "Babies and OSS maintenance"
description = "We just had baby twins! Just a quick blog post on OSS maintenance related to that."
date = 2021-09-29
+++
I'm very happy to let everyone know: after some fun times with preterm labor, Miriam and I welcomed two new babies into the world this past Saturday. Between the hospital trips and the Jewish holiday season (we just finished the final holiday of this season, Shmini Atzeret), I didn't have a chance to let everyone know. But I finally got around to an announcement tweet this morning, which has some pictures:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">On Saturday, <a href="https://twitter.com/LambdaMom?ref_src=twsrc%5Etfw">@LambdaMom</a> and I welcomed two new babies into the world! Mom and babies are both healthy, and hopefully returning home today. <a href="https://t.co/s3oP80M0h9">pic.twitter.com/s3oP80M0h9</a></p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/1443074656454561793?ref_src=twsrc%5Etfw">September 29, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Before the babies were born, I had thought about putting out a tweet about open source maintenance, but never got around to it. So in the few hours I have now before heading off to pick up the kids from the hospital, I thought I'd write up something brief.

## My OSS maintenance

I try to generally stay responsive on my projects, but odds are pretty good that I'm going to fall short of the mark in the next few weeks/months. It's true that I have a bit of experience with managing kids and OSS, but twins are a new curveball I've never encountered before. The main points I want to get across are:

1. Apologies in advance for delays, or worse, me completely missing some conversations
2. Now's a great time to volunteer as a comaintainer on any of the packages I maintain

## How I maintain packages

I've never actually codified my current stance on OSS project maintenance, so it's worth getting out there. I'll include Haskell-specific concepts in here, though most of what I say would apply to non-Haskell projects too.

1. Stability of projects is a huge feature in its own right. It's only with great hesitation that I'll include breaking changes in packages these days. Going along with that, when it comes to new and experimental features, I'll usually request they be put in separate packages when possible to avoid version dependency churn on established packages.
2. My favorite way of including changes is pull requests. I used to be much more trigger-happy to implement new features and write bug fixes myself. But simply the constraints of time, plus the desire to include more people in project maintenance, has changed this. If you've opened an issue on a project of mine in the past few years, you probably got the response "PR welcome."

3.  When reviewing a PR, I'll ask for it to include:

    * The code change itself, together with inline documentation
    * A version number bump in the `.cabal` or `package.yaml` file
    * An entry in the ChangeLog
    * A `@since` Haddock comment for any newly exported identifiers

4. Once a PR is ready to merge, I want to see all of CI pass, and then follow up with both merging _and_ releasing to Hackage. If I don't immediately release to Hackage, I'm going to forget to do so. And there's generally no good reason to hold off on such releases. Also, each release should include a Git tag for that release.

5. There are a few time saving mechanisms I've taken on over the years as well. These include:

    * I won't spend time on Hackage revisions. I'm not starting a fight here, I'm stating a fact. When people ask me to make revisions on Hackage to fix the dependency solver, my response will almost always be: I'm happy to give you Hackage maintainer rights to do so yourself, but I won't do it.
    * GHC has overly aggressive releases which cause a lot of breakage. I won't guarantee compatibility with more than three GHC major versions going forward (e.g. GHC 8.6, 8.8, and 8.10) Even maintaining the CI matrix for more than that is a major burden. I realize dropping support for 1.5 year old compilers is aggressive, and I'd prefer not to do that. But it's a simple cost/benefit trade-off here.
    * I maintain a lot of my projects in "mega-repos", or more appropriately named these days monorepos. I use the tools [mega-sdist](https://www.stackage.org/package/mega-sdist) to help maintain releases of these packages.
    * When it comes to dependency bounds in my `.cabal`/`package.yaml` files, I will include lower bounds on my packages in two cases:
        * I know for a fact that I don't support an old version of a package.
        * To exclude versions of GHC (via a `base`-library bound) that my CI scripts don't cover.
    * I almost never include upper bounds on my dependencies, since it causes more work than it saves. (Yes, I know that this is a topic of contention, but these are my personal rules.) If a new release of a dependency breaks my package, by far the best way to fix that is to release a new version with support for both the old and new version. This has the least ecosystem impact of all alternatives I've tried.

I'm sure I've missed some things, and if I think of them I'll try to update this post in the future. But hopefully this gives some clear guidelines for people considering getting involved in comaintaining.

## Preferred CI system

My OSS projects have used Travis, AppVeyor, and Azure CI in the past for CI systems. At this point, I consider GitHub Actions the easiest and most integrated experience, and wherever possible I've been moving my CI systems over to it. I no longer maintain dual Cabal/Stack build systems, since it's simply too much of a burden to maintain. A nice example of a Stack-based GitHub Actions script is available in Conduit at https://github.com/snoyberg/conduit/blob/master/.github/workflows/tests.yml.

I haven't moved all of my projects over to GitHub Actions. But if you're interested in picking up co-maintenance on any projects, switching over the CI system would be a great first step in making them more maintainable.

## Which packages do I maintain?

The easiest place to get a comprehensive list of Haskell packages I maintain is the Hackage user page:

https://hackage.haskell.org/user/MichaelSnoyman

Note that there are many packages in that list that are deprecated, so just because a package exists there doesn't mean it's in need of a comaintainer.

Also, going along with this, it's worth recalling a point above. I'm trying for the most part to keep my packages stable, which means few breaking changes, and reduced feature expansion. As a result, many of the packages above require little in the way of maintenance, outside of dealing with breakages coming from upstream packages or, more likely, GHC itself.

## Special call out: Stack

Stack is by far the largest project I maintain. I don't maintain it alone, but none of the maintainers are spending a significant amount of time on it. Frankly, for all of my needs, it's checking the boxes right now. Most of my pain points come from changes coming upstream from GHC or Cabal causing breakage, or introducing new features (like multiple libraries per package) that require large overhauls to how Stack operates.

I haven't said this before, but I'll say it now: I'm not interested in investing any time in staying on that treadmill, or introducing new features to Stack. I had hoped that with the Haskell Foundation launch this year, we would have an affiliation process for projects that allowed better interproject communication and reduced the maintenance burden. That never came into existence, and so now I feel pretty comfortable in saying: outside of making sure Stack continues to work for my primary use cases, I'm not going to be investing my own time much going forward.

People are free to take those statements as they will. I'm not sure how large an overlap there is between Stack users and people looking to use the latest GHC and Cabal features that it doesn't support. I know that it becomes a blocker for the Stackage team sometimes. And I know regressions in Stack with newer GHC/Cabal versions (like overly aggressive recompilation, or broken deregister support for private libraries) causes the Stackage team pain and suffering. I'd love to see these addressed. But I'm not going to do it myself.

In other words, this is a more serious call to action than I've made previously. If people want to see changes and improvements to Stack, you need to get involved personally. I'll continue maintaining the project in its current state, together with other maintainers currently doing so. But in the past few months, as I've been thinking about where I wanted to spend my limited time after the babies were born, I decided that it was time to call out Stack as needing motivated maintainers, or to see it sit in a feature complete, non-evolving state. I'm quite happy with either outcome.

## How to volunteer as comaintainer

In many ways I timed this blog post terribly. I should have put it out two months ago, when I still had time to deal with incoming messages. But I guess better late than never!

If you're interested in becoming a comaintainer on a package, the best way to go about is to open a GitHub issue stating:

* That you want to be a comaintainer
* Which packages you want to comaintain (especially necessary for monorepo projects)
* Your Hackage username

I'm going to try prioritizing responses to these kinds of requests over other OSS duties. Once you've got access to the repos, I'd make one more request: please update the README to include yourself as a comaintainer of the package.

## Thank you!

I'm already getting lots of congratulations messages on Twitter, and I won't have a chance to respond to everyone. So I'll put down here: thank you! Miriam and I are both very touched to be part of an online community that we can share our [*simchas*](https://en.wikipedia.org/wiki/Simcha) with.
