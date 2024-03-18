+++
title = "How I Stay Organized"
description = "A review of my personal organizational system and tools I use to enhance it"
date = 2024-03-18
+++
When I describe the Yesod web framework, one of the terms I use is the
boundary issue. Internally, I view Yesod as an organized, structured,
strongly typed ecosystem. But externally, it's dealing with all the
chaos of network traffic. For example, within Yesod, we have clear
typing delineations between normal strings, HTML, and raw binary data.
But the network layer simply throws around bytes for all three. The
boundary issue in Yesod is the idea that, before chaotic, untyped,
unorganized data enters the system, it has to be cleaned, sanitized,
typed, and then ingested.

This represents my overall organizational system too. I've taken a lot
of inspiration from existing approaches, notably Getting Things Done and
Inbox Zero. But I don't follow any such philosophy dogmatically. If your
goal in reading this blog post is to get organized, I'd recommend
reading this, searching for articles on organization, and then
determining how you'd like to organize your life.

## The process

I like to think of *chaotic* versus *ordered* systems. Chaotic systems
are sources of *stuff*: ideas, work items, etc. There are some obvious
chaotic sources:

-   Mobile app notifications

-   Incoming emails

-   Phone calls

-   Signal/WhatsApp messages

I think most of us consider these kinds of external interruptions to be
chaotic. It doesn't matter what you're in the middle of, the
interruption happens and you have to choose how to deal with it. (Note:
that may include ignoring it, or putting notifications on silent.)

However, there's another source of chaos, arguably more important than
the above: yourself. When I'm sitting working on some code and a thought
comes up, it's an internally-driven interruption, and often harder to
shake than something external.

Taking heavy inspiration from Getting Things Done, my process is simple
for this: record the idea and move on. There are of course caveats to
that. If I think of something that demands urgent attention (e.g., "oh
shoot I left the food on the stove") chaos will reign. But most of the
time, I'm either working on something else, taking a shower, or kicking
back reading a book when one of these ideas comes up. The goal is to get
the idea into one of the ordered systems so I can let go of it and get
back to what I was doing.

For me, my ordered systems are basically my calendar, my todo list, and
various reminders from the tools that I use. I'll get into the details
of that below.

## Other people

How do you treat other people in a system like this? While I think in
reality there's a spectrum, we can talk about the extremes:

-   Chaotic people: these are people who don't follow your rules for
    organization, and will end up randomizing you. This could be a
    demanding boss, a petulant child, or a telemarketer trying to sell
    you chaos insurance (I'm sure that's a thing). In these cases, I
    treat the incoming messages with chaos mode: jot down all work
    items/ideas, or simply handle them immediately.

-   Ordered people: these are people you can rely on to participate in
    your system. In an ideal world, this would include your coworkers,
    close friends and family, etc. With these people, you can trust that
    "they have the ball" is equivalent to writing down the reminders in
    your ordered systems.

That's a bit abstract, so let's get concrete. Imagine I'm on a call with
a few other developers and we're dividing up the work on the next
feature we're implementing. Alice takes work item A, Bob takes work item
B, etc. Alice is highly organized, so I rely on her to record the work
somewhere (personal todo list, team tracker, Jira... somewhere). But
suppose Bob is... less organized. I'd probably either create the Jira
issue for Bob and assign it to him, or put a reminder in my own personal
systems to follow up and confirm that Bob actually recorded this.

You may think that this kind of redundancy is going overboard. However,
I've had to use this technique often to keep projects moving forward. I
try as much as possible to encourage others to follow these kinds of
organized systems. Project management is, to a large extent, trying to
achieve the same goal. But it's important to be honest about other
people's capabilities and not rely on them being more organized than
they're capable of.

As mentioned, no one is 100% on either the order or chaos side. Even the
most chaotic person will often remember to follow up on the most
important actions, and even the most ordered will lose track of things
from time to time.

## Tooling

Once you have the basic system in mind for organizing things, you need
to choose appropriate tooling to make it happen. "Tooling" here could be
as simple as a paper-and-pen you carry around and write everything down.
However, given how bad my handwriting is and the fact that I'm
perpetually connected to an electronic device of some kind, I prefer the
digital approach.

My tooling choices for organization come down to the following:

### Todoist

I use Todoist as my primary todo list application. I've been very happy
with it, and the ability to have shared projects has been invaluable. My
wife (Miriam, aka LambdaMom) and I use a shared Todoist project for
managing topics like purchases for the house, picking up medicines at
the pharmacy, filing taxes, etc. And yes, having my spouse be part of
the "ordered world" is a wonderful thing. We've given the advice of
shared todo lists to many of our friends.

One recommendation if you have a large number of tasks scheduled each
day: leverage your todo app's mechanisms for setting priorities and
times of day for performing a task. When you have 30 items to cover in a
day, including things like "take allergy medicine in the afternoon" and
similar, it's easy to miss urgent items. In Todoist, I regularly use the
priority feature to push work items to the top.

### Calendars

While todo lists track work items and deliverables, calendars track
specific times when actions need to be taken: show up to a meeting, go
to the doctor, etc. I don't think anyone's too surprised by the idea of
using a calendar to stay organized.

### Email

Email is another classic organization method. Email is actually a much
better ordered system than many other forms of communication, since it
has:

-   Unread: things that need to be processed and organized

-   Read in inbox: things that have gone through initial processing but
    require more work

-   Snooze: for me a killer feature. Plenty of emails do *not* require
    immediate attention. In the past I used to create Todoist items for
    following up on emails that needed more work. But snoozing email is
    now a common feature in almost every mail system I use, and I rely
    on it heavily.

### Other chat apps

But most communication these days is *not* happening in email. We have
work-oriented chat (like Slack) and personal chat applications (Signal,
WhatsApp, etc). My approach to these is:

-   If the app provides a "remind me later" feature, I use it to follow
    up on things later.

-   If the app doesn't provide such a feature, I add a reminder to
    Todoist.

Technically I could use "mark as unread" in many cases too. However, I
prefer not doing that. You may have noticed that, with the approaches
above, you'll very quickly get to 0 active notifications in your apps:
no emails waiting to be processed, no messages waiting for a response.
You'll have snoozed emails pop up in the future, "remind me later"
messages that pop up, and an organized todo list with all the things you
need to follow up on.

## Notifications and interruptions

This is an area I personally struggle in. Notifications from apps are
interruptions, and with the methods above I'm generally able to minimize
the impact of an interruption. However, minimizing isn't eliminating:
there's still a context switch. Overall, there are two main approaches
you can take:

-   Receive all notifications and interruptions and always process them.
    This makes sure you aren't missing something important and aren't
    blocking others.

-   Disable notifications while you're in "deep work" and check in
    occasionally. This allows better work time, but may end up dropping
    the ball on something important.

For myself, which mode I operate in depends largely on my role. When I'm
working as an individual contributor on a codebase, it's less vital to
respond immediately, and I may temporarily disable notifications. When
I'm leading a project, I try to stay available to answer things
immediately to avoid blocking people.

My recommendation here is:

-   Establish some guidelines with the rest of your team about different
    signaling mechanisms to distinguish between "please answer at some
    point when you have a chance" and "urgent top priority please answer
    right now." This can be separate groups/channels with different
    notification settings, a rule that urgent topics require a phone
    call, or anything else.

-   Try to use tools that are optimized for avoiding distractions. I've
    been particularly enamored with Twist recently, which I think nails
    a sweet spot for this. I'm hoping to follow up with a blog post on
    team communication tools. (That's actually what originally inspired
    me to write this post.)

## Work organization

I've focused here on personal organization, and the tools I use for
that. Organizing things at work falls into similar paradigms. Instead of
an individual todo list, at work we'll use project management systems.
Instead of tracking messages in WhatsApp, at work it might be Teams. For
the most part, the same techniques transfer over directly to the work
tools.

One small recommendation: don't overthink the combining/separating of
items between work and personal. I went through a period trying to keep
the two completely separate, and I've gone through periods of trying to
combine it all together. At this point, I simply use whatever tool seems
best at the time. That could be a Jira issue, or a Todoist item, or even
"remind me later" on a Slack message.

As long as the item is saved and will come up later in a reasonable
timeframe, consider the item handled for now, and rely on the fact that
it will pop back up (in sprint planning, your daily todo list review, or
a notification from Slack) when you need to work on it.

## Emotions

A bit of a word of warning for people who *really* get into
organization. It's possible to take things too far, and relate to all
impediments to your beautifully organized life as
interruptions/distractions/bad things. Sometimes it's completely
legitimate to respond with frustration: getting an email from your boss
telling you that requirements on a project changed is difficult to deal
with, regardless of your organizational system. Having a telemarketer
call in the middle of dinner is always unwanted.

But taken too far, a system like this can lead you to interpreting *all*
external interruptions as negative. And it can allow you to get overly
upset by people who are disrupting your system by introducing more
chaos. Try to avoid letting defense of the system become a new source of
stress.

Also, remember that ultimately you are the arbiter of what you will do.
Just because someone has sent you an email asking for something doesn't
mean you're obligated to create a todo item and follow up. You're free
to say no, or (to whatever extent it's appropriate, polite, and
professional) simply ignore such requests. You control your life, not
your todo program, your inbox, or anyone who knows how to ask for
something.

My recommendation: try to remember that this system isn't a goal unto
itself. You're trying to make your life better by organizing things. You
expect that you won't hit 100%, and that others will not be following
the same model. Avoiding the fixation on perfection can make all the
difference.

## Further reading

For now, I'm just including one "further reading" link. Overall, I
really like Todoist as an app, but appreciate even more the thought they
put into how the app would tie into a real organizational system. This
guide is a good example:

- [Todoist Getting Things Done guide](https://todoist.com/productivity-methods/getting-things-done)

Beyond that, I'd recommend looking up getting things done and inbox zero
as search terms. And as I find other articles (or people put them in the
comments), I'll consider expanding the list.
