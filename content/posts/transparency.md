In [yesterday's blog post](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1), I (lovingly) attacked a few things about Haskell. My goal was pretty explicit: I wanted to educate. I was hoping that by announcing some of these language warts loudly, people could avoid some footguns, and their Haskell experience would be more pleasant. This would lead to more Haskell success, which IMO is a Very Good Thing.

I can tell you all that not once did the thought cross my mind that I should try to get any of these issues resolved. Maybe that's because I spend a lot more time educating these days. The reason doesn't really matter.

The really exciting thing, though, is that I almost immediately found out that one of the warts I mentioned will hopefully soon be fixed:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">That&#39;s the moment where I am pleased to announce that I have started a proposal to fix sum and product in base:<br><br>-&gt; <a href="https://t.co/oPTFobN7ab">https://t.co/oPTFobN7ab</a><br><br>And a merge request is in the works to implement it : <a href="https://t.co/W7Ua4XfXwd">https://t.co/W7Ua4XfXwd</a><br><br>Hope this addresses some of your concerns, <a href="https://twitter.com/snoyberg?ref_src=twsrc%5Etfw">@snoyberg</a> :) <a href="https://t.co/ujere27Bzi">https://t.co/ujere27Bzi</a></p>&mdash; Ἑκάτη (@TechnoEmpress) <a href="https://twitter.com/TechnoEmpress/status/1321391161496489990?ref_src=twsrc%5Etfw">October 28, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

This is wonderful news, and I'm happy to hear it. In the ensuing discussion, there were some comments directed at me indicating that there is some desire to make changes like this more approachable. This isn't the first I'm hearing of this. Over the past few months, I've had a few discussions with Haskell community members. I wish these discussions were more public and accessible, but we'll get to that.

I truly believe people are trying to improve Haskell community processes right now. I've shared some thoughts with people to help encourage them towards what I think are vital improvements to make Haskell a better language, a better ecosystem, and a better community. Now that these conversations are (thankfully) beginning to happen more openly, I'd like to put some of my thoughts down in writing here.

The notes below turned out to be something of a stream-of-consciousness brain dump. There's no particular order or priority. Also, one final thing. I'm making aspirational statements below. I certainly try to achieve these goals. I don't always succeed. My failings as an individual and a maintainer do not preclude that correctness of my points below.

## What is transparency?

Before diving into my points, I want to clarify what I consider transparency. It's not simply about an existence proof that a sufficiently dedicated person could discover information. It's the property that a reasonably interested person can easily find out information about a project. But it's more than that. Simply having introspection is the first step. Understanding how things work, how to contribute, how to influence, and how to disagree are vital too. A truly transparent project, in my definition, provides an easy onramp for new and interested parties to come up to speed on current status, see the inner workings of current members, volunteer to be "part of the team," and constructively argue for a new direction.

OK, that's it for my definition. You can disagree with that definition, but hopefully you can now understand better my concrete points below.

## Multiple media

Do you like mailing lists? Twitter? Reddit? Blog posts? RSS feeds? Slack? Discourse? Discord? Gitter? Keybase? Zoom?

I'm willing to bet no one in the world is going to answer yes to all of these.

An open, transparent community needs to advertise information. And it needs to do this in multiple media. A simple example of this is:

* Write a blog post
* In the blog post, reference a dedicated discussion point (comments section, a GitHub issue, a Slack channel, etc.)
* Reference it from an index page on a Wiki or website
    * Personally, I think [wikis are a waste of time](https://www.yesodweb.com/blog/2015/08/thoughts-on-documentation)
* Publish an RSS feed from that blog
* Provide a "subscribe via email" feature on that RSS feed
* Post the blog on Twitter, LinkedIn, Facebook, and Reddit
* Monitor feedback on multiple venues, and if worthy, write a follow up blog post to provide updates/clarify points, since not everyone will see a Reddit comment you make
    * As an example: this blog post is _exactly_ that

It takes a bit more effort to do all of this. But it's completely worth it.

## Centralize information

Almost as a counterpoint to the above: information needs to be centralized too. It's great to have a Reddit thread where 23 comments deep someone finally explained something. But having that comment's content and permalink put on the project's main website or the `README.md` of its GitHub repo is vital. Information discovery is difficult. If something is important, make it available centrally. And then, if relevant, advertise the presence of that information broadly, using the techniques above.

## Appropriate signal to noise ratio

In the beginning of Hitchhiker's Guide to the Galaxy, Arthur Dent discovers that, for months, there have been plans to demolish his house sitting in the basement of the planning office. He's told he needs to take initiative to be involved in government to know about these kinds of things. Ultimately, some galactic aliens pull the same thing on us Earthlings and blow us up. (Oh, sorry, spoiler alert.)

Being told "but there was a GitHub issue 5 months ago that proposed this" is the same thing. Burying high priority information in the middle of a highly active channel (issue tracker, active mailing list, etc.) obscures things.

It's not always obvious when a change is something that warrants a heavy signal. Mistakes will happen. Sometimes you'll overshare. But much worse is undersharing. Examples:

__OVERSHARE__

> BREAKING! Super super important! Everyone needs to know that __we changed which hue of purple we used for our logo__!!!!1111oneoneoneeleventyone

__UNDERSHARE__

> Yeah, of course we removed the `+` operator from the language. We thought it wasn't important. Didn't you see the 17th comment on the issue discussing the new color scheme for the website?

Try to identify what people will consider a major change versus a minor change. If you're not sure, __ask__. And if something will be a major change for some and minor for others: create multiple distribution channels. Not everyone in the Haskell community will consider a breaking change to Yesod a big deal. But Yesod users will. That's why we have a dedicated Yesod blog and mailing list.

## Canvass people directly

People are busy. People are lazy. People are socially awkward. People are unsure if their opinions are valuable. People are unsure if their opinions are welcome.

It is insufficient to put a message on a mailing list and ask for feedback. Even if you go through the steps above, you're going to miss people.

When I've made major changes, I've tried (and sometimes failed) to reach out privately to affected parties and get their feedback. It's vital.

## Notifications

Somewhat redundant with the above, but worth pointing out. Use technology that _notifies_ people instead of requiring people to seek out information. "We updated a Wiki page, didn't you go and check it out again in the past three months?" isn't great. People are busy, and they forget things. A push model, rather than pull, is preferable.

## Outsiders

Include outsiders. It's too easy to reach for the input of the ingroup and your friends and end up creating a bubble. It's almost impossible to detect that you've done this.

Strive to include people who think differently than you. Strive to include people who disagree with you. At least within reason. Which brings us to the next point...

## Goals

Be upfront about your goals. Tying it back to the previous point: let's say I'm trying to make Yesod a more open, transparent, and inclusive project. I personally am a strong believer in server-generated HTML for user interaction. But my _goal_ for Yesod is to support Single Page Applications (SPAs) as well. Knowing this, it's important for me to make sure that there are Yesod contributors and advisers who believe SPAs are important.

On the other hand, continuing with outsiders: we don't need to go beyond the goals of the project. I don't need to find a Go developer to advise on how Yesod can be better. Go is not part of the goals of Yesod. It's OK that Yesod fails to meet the goals of a Gopher. They can, and should, and have, create their own web framework. And the two can live in harmony, possibly with competition between the two projects trying to win the ideological and technical battle of "which set of goals is better."

My go-to example for this is Stackage. Stackage is incredibly clear about its goals. We want to build Haskell packages. Haskell packages are defined as using the Cabal format. We want to build snapshots of packages that build together. We want to make the process of contribution as lightweight as possible. We want to make it possible for end users to consume these snapshots and get more reliable builds.

There are other details, but that basically sums it up. Based on that, you can decide if this is a project you want to be a part of, contribute to, and use. Are you a Rubyist? This probably isn't for you. Do you want to exclusively use dependency solving and provide no support for snapshots? More power to you, you do you. Do you write Haskell code, publish it on Hackage, and want to get notified if your package stops compiling with new dependencies or has restrictive upper bounds? Bingo.

Poorly defined goals have, in my opinion, been one of the central problems in the Haskell community. Furthermore, __evolving__ goals have been worse. Goals of course need to evolve over time. But the fact that goals change implicitly instead of explicitly is a huge problem.

If you're going to make a fundamental shift to the goals of a project, especially a project in massive use by many people, and especially if you know those people _disagree_ with the new goals: you need to have a very open conversation about this. I'd probably go further and say that you basically _can't_ change the goals in this way. Rather, a subproject would make more sense.

And if you're paying attention: that's basically what Stackage is. Hackage's starting goal was about an open source repository of Haskell code. It said nothing about snapshots. Forcing people uploading to Hackage to suddenly adopt a new goal would be inappropriate. Instead, a second project&mdash;clearly deriving from and depending upon the original project&mdash;was the right move here.

## Definitions

Be clear about what the various parts of the project are. Be clear about how decisions are made. Be clear about how members are chosen. Be clear about what responsibilities and authorities those members have. Be clear about how someone goes about trying to make changes.

## Ask for help

People want projects to succeed. I can all but guarantee you that anyone who calls themselves a "Haskeller" wants the Haskell language to be successful (however they define success). People are also busy, and lazy, and distracted, and lack confidence, and lots of other things that will prevent them from participating. You won't be able to overcome all of that. But there are some blockers you _can_ fix:

* I want the project to succeed, but I don't know what needs to be done
* I know what we overall need to do, but I don't know how to do it
* I don't know who I should speak with to volunteer

Reach out to people for help! Put out blog posts (and advertise them) calling for contributors. Give information on what needs to be done. Reach out to people individually that you think would be a good fit.

People may ignore the posts. People may say no to a direct invite. But you won't know if you don't ask. And even if someone says no, knowing that they were wanted in a project is wonderfully inclusive.
