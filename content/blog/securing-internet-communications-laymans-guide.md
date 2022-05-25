+++
title = "Securing internet communications: a layman's guide (2021)"
description = "A hopefully helpful guide to understanding private communications and censorship-proof social media in 2021. Targeted at non tech people trying to understand this crazy world.\n"
path = "/blog/2021/01/securing-internet-communications-laymans-guide"
date = 2021-01-10

[taxonomies]

[extra]
ogimage = "securing-internet-communications-laymans-guide.png"
+++
There has been rising concern over the past number of years around security in personal communications. More recently, with censorship on social media platforms occurring on a grand scale, people are wondering about securing social media platforms. I wouldn't call myself an expert on either topic, but I have good knowledge of the underlying technologies and principles, and have done some investigative work into the specific platforms for work. With many friends and family suddenly interested in this topic, I figured I would write up a layman's guide.

One word of caveat, since everything is politically charged today. I definitely have my own political views, and I mostly elect to keep them separate from my discussions of technology. I'm going to do so here as well. I'm not endorsing or objecting to any recent actions or desires of people to communicate in certain ways. If I ever decided to publicly share my political views, I would do that separately. For now, I'm simply going through the technological implications.

__UPDATE__ I decided to make a video version of this information as well for those who prefer getting the content in that format. You can check it out [on YouTube](https://www.youtube.com/watch?v=RT69LwB2Pyk) or [on BitChute](https://www.bitchute.com/video/zuT7MMGdEX7c/).

## Executive summary

I know there are a lot of details below, but I strongly encourage people to read through it, or at least skim, to understand my recommendations here. But for the busy (or lazy), here are my recommendations:

* Private communication:
    * Use [Signal](https://www.signal.org/), [Wire](https://wire.com/en/), or [Matrix](https://matrix.org/)
    * Be careful about what you say in a group chat
    * Assume anything you say will last forever
    * If you want semi-secure email, use [ProtonMail](https://proton.me/mail), but don't rely on it
* Social media
    * Your best bet for censorship freedom: host the content yourself, but that's hard, and you have to find a place to host it
    * When you use _any_ platform, you're at their mercy regarding censorship
    * Each platform is pretty upfront at this point about what you're getting. Facebook and Twitter will remove some voices, Gab and Parler claim they won't
* General security
    * Use a password manager!!!
    * Don't install random executables and extensions
    * Don't trust messages from people, confirm who they are, check URLs

OK, with that out of the way: the analysis.

## Security doesn't mean anything

This is the first major point to get across. People often use the term "security" for lots of different things. In a vacuum, the term doesn't mean much. That's because security only applies in the presence of a specific kind of attack, and we haven't defined that attack. It could be a Denial of Service attack, where someone tries to prevent the service from working. It could be a physical attack, like stealing your phone.

What I hear people concerned about right now are two kinds of attacks. They think they're related, but in reality they are not. Let me explain those attacks:

* I'm worried that my private communications are being read by Big Tech or the government
* I'm worried that my social media posts are going to be censored by Big Tech

Notice that, in many ways, these are _opposite_ concerns. The former is about ensuring you can say something without anyone else knowing it. The latter is about ensuring you can say something loudly without anyone stopping it. The two different threats necessitate two different analyses, which I'll provide below.

Also, let me address a threat I'm _not_ addressing, since it's an inherent contradiction. You can't worry about privacy on social media, not in the "blast to the world" public concept it's typically used in. If you want something to be private, don't put it on social media. This may seem obvious, but many people seem to want to have their cake and eat it too. If you post on social media, you can always be held accountable for what you've said there. If you want privacy, use private communications.

## Metadata versus data

Metadata is a term like "algorithm" which has a real meaning, but is (ab)used in the media so much to make it seem like scary, unknowable information. It's not. Metadata is just "data about data." Let's take a simple private communication example. My doctor wants to send me a message about my test results. Most people, and most governments in fact, recognize a right to privacy for this, and enforce this through law (e.g., HIPAA).

In this example, the "data" would be the results themselves: what test I took, who administered the test, when I took the test, and the results. The metadata would be information about sending the message: who the sender is, who the receiver is, the size of the message, the timestamp of when it was sent, etc.

Many messaging protocols try to ensure two things:

* The data is completely private to only the participants of the conversation
* The metadata has as little useful information in it as possible

The reason is that, often times, metadata can be read by intervening services. In our test result example, it would be best to assume that a nefarious party will be able to find out that my doctor sent me _some message_ at 4:32pm on Tuesday, and that it was 5mb in size. Most messaging systems try to hide even that, but you don't usually get the same guarantees as with the underlying data.

And that brings us to the first important point.

## Email is busted

Don't use email for private communications, full stop. It's an antiquated, poorly designed system. There are a lot of tools out there to try to secure email, but they are all as holey (not holy) as swiss cheese. The primary issue: most of them have no ability to secure your metadata, and that will often include the subject. Imagine your nefarious character can read:

> From: Dr. Smith <drsmith@prostatecancer.org>  
> To: John Doe <john@doe.com>  
> Subject: Test results, not looking good

Sure, the rest of the message is secure, but does it matter?

Email is a necessary evil. Many services require it as some kind of identity provider. You'll have to use it. But don't consider anything you put in email safe.

With major providers like Gmail and Outlook, you can safely assume that the companies are spying on everything you're saying. I use both, and assume nothing that goes on there is truly private. If you want to harden your email usage a bit, [ProtonMail](https://proton.me/mail) is a good choice.

## Messaging apps

Perhaps surprisingly, your best bet for privately communicating with others is messaging apps. These include options like WhatsApp, Telegram, and Signal, and I'll get into the trade-offs. As with most things in security, there's a major trade-off between *security* and *convenience*. Since I'm writing this for a general purpose audience, I'm not going to go into the intricacies of things like secure key transfer, since I don't think most people will have the stomach for what's involved here. Suffice it to say: these options are, in my opinion, the best options that I think most people will be comfortable using. And they're secure enough for me to use, as you'll see.

### Encryption

The primary question for messaging apps is _encryption_, and specifically _public key cryptography_. With public key cryptography, I can send you a message that only you can read, and you can verify that I'm in fact the one who sent it. Done correctly, public key cryptography prevents many kinds of attacks.

But there's a twist here. Let's say Alice and Bob are trying to talk to each other using a messaging app called SecureTalk. Alice uses the SecureTalk app on her iPhone, and it sends messages to the SecureTalk server, which sends the messages on to Bob's Samsung device. Alice encrypts her message, so only the recipient can read it. The question is: who is the recipient? This may seem obvious (Bob), but it's not. There are two different possibilities here:

1. In something called *end-to-end encryption*, Alice will encrypt the message so only Bob can read it. She'll send the encrypted message and some metadata to the SecureTalk server. The server will be able to read the metadata, but won't know what the message itself says. Then the SecureTalk server sends the metadata and encrypted message to Bob, who is the only person who can read the message itself.
2. The simpler approach is that Alice will encrypt the message so that the SecureTalk server can read it. This prevents random other people on the internet from reading the message, but doesn't prevent SecureTalk from reading the message. SecureTalk then reencrypts the message for Bob and sends it to him.

You may think that (1) actually sounds simpler than (2), but for various technical reasons it isn't. And therefore, a lot of systems out there still do not provide end-to-end encryption. The primary culprit I want to call out here is Telegram. Telegram is viewed as a secure messaging platform. And it _does_ provide end-to-end encryption, but only if you use its "secret chat" feature. But you lose features with it, and most people don't use it most of the time. In other words:

> Telegram is not a good choice for security, despite its reputation to the contrary

### Identity

How do you identify yourself to the messaging service and your colleagues? Most apps use one of two methods: phone number (verified via SMS) and email address (verified via confirmation email). For the most part, the former is most popular, and is used exclusively by systems like WhatsApp, Telegram, and Signal. This is good from an ease of use standpoint. But there's a major issue to be aware of:

> Your secure chat identity is tied to your phone number, and most countries track ownership of phones

Maybe you can buy a burner phone without the number being tied to your identity, I'm not sure. I've never needed that level of privacy. But the other system, email address, is easier for creating a more anonymous identity. Most people can easily create a ProtonMail account and have an anonymous experience.

This is outside the bounds of security, but another advantage of email-based identity is that family members without their own cell phones (like my kids) can use those systems.

If you want to use email as your identity, and make it easier for people to communicate fully anonymously, systems like [Wire](https://wire.com/) and [Matrix](https://matrix.org) are your best bet. Wire honestly overall seems like the best system for secure communication to me, but it has the downside of not being as popular.

### Exploding messages

Many systems offer a feature called "exploding messages" or similar. It's a nice idea: you can send a message, and the message will be deleted in a certain amount of time. I've used it at work for sending temporary passwords to people when signing into new accounts. It works great when you have full trust in the other side.

**DO NOT RELY ON EXPLODING MESSAGES**

There is no way at all to prevent a nefarious message receiver from screenshotting or otherwise capturing the contents of the message. We've probably all heard horror stories of highschool girls sending their boyfriends inappropriate Snapchat messages, and the boyfriend screenshotting and sharing those pictures with his friends.

### Group chats

The easiest way to keep a secret among three people is to kill two of them. Group chats are the same. Security is always multilayered, and there are lots of ways of breaching it. In the case of secure messaging, group conversations are far easier to intercept, since:

* If you break into just one person's device, you've won
* If you can get just one bad actor included in the group, you've won
* If you can spoof just one person (pretending that you are that person), you've won

Treat as suspect any group chats, unless you absolutely know and trust every single person in the group. And maybe not even then. Like email, consider group chats semi-compromised at all times.

### Ownership

WhatsApp ostensibly uses the same secure protocol as Signal. WhatsApp is widely used. In principle, it's an ideal platform for secure communication, assuming you're OK with phone number based identity. But there's a huge elephant in the room: WhatsApp is owned by Facebook.

If you trust their claims, the messages in WhatsApp are fully end-to-end encrypted. WhatsApp cannot read what you're sending to your friends and family. But they can definitely read the metadata. And their privacy policy is troubling at best.

I used to recommend WhatsApp, but I no longer feel comfortable doing so. It's not the worst communication platform, not by far. But at this point, I would recommend that you move over to a different platform, and begin recommending the same to your friends and family.

### Conclusion

The top contenders in this category are WhatsApp, Telegram, and Signal. I've eliminated Telegram due to poor encryption, and WhatsApp due to privacy policy and corporate ownership. Signal wins by default. Additionally, if you're looking for a platform that allows more anonymity, Wire lets you identify yourself via email, and is just as easy to use. The downside is that it's less popular. Finally, Matrix is more sophisticated, and offers some really nice features I haven't touched on (specifically, federation). But it's not as easy to use as the others. Look to it if you're trying to create a community, but it's probably not the best choice.

Also, start considering using Signal's voice chat as an alternative to normal phone calls, it's also end-to-end encrypted.

## The platform

As I said above, security is multilayered. You can have the most secure message protocol, with the best encryption, and the most well written application to communicate. But if your platform (your phone, tablet, or computer) are compromised, none of that will matter.

I honestly don't know what to say about phones. Apple has a good reputation here. Android has a middling reputation here. There are concerns on spying from certain manufacturers, I don't know if there's validity. Windows and Mac computers presumably don't spy on people, but they might.

Again, I'm writing this for normal people, so I don't want to go too deep on the options here. Experts in the field will start looking at hardened operating systems, and hardware vulnerabilities, and other such things. For the most part, for most people, I'd recommend making sure your devices are secured. That means good passwords, two factor authentication where relevant, and not losing the physical device.

## Social media

If you want to share cat pictures with your family, probably any social media platform will allow you to do it. That is assuming, of course, that you don't express the wrong ideas elsewhere. Ultimately the thing to keep in mind is:

> If you use someone's service, they control your voice

There's no value judgement in that statement, it's a statement of fact. If you use Twitter, they currently have the full authority, with no legal impediments, to silence you at will, for any reason. Some people believe that they are judiciously exercising a necessary power to protect people from violence. Others believe that they are capriciously censoring people with WrongThink. Again, this post isn't about the politics of it, just the facts.

I'm assuming here that your goal is to get your message out to the world without being stifled. The bad news is: you can't do it. Any social media platform can ultimately exercise control over you, despite what they claim. And even if you trust the people running the company, the company may be purchased. Or the people running the computers that the social media platform is using may deny _them_ usage. Or the government may shut them down.

If you want a message out there that no one can stop, your best bet is hosting it yourself. Even _that_ is difficult. I run this website myself (snoyman.com). I bought a domain name. The government could decide I don't have a right to have that domain name and take it away. That's happened in the past with copyright claims and pirating. I use Amazon as a cloud provider. Amazon may decide I'm not entitled to use their services. So you have more control with your own hosting, but not total control.

There are more sophisticated censorship-resistant ideas in the world. These focus on decentralization, and leveraging technologies like blockchain to mathematically prevent removal of data. Those systems are not yet mainstream, and there are many practical things that may prevent them.

So if you want your message out there without censorship, here is my advice:

* Recognize the reality that if you say the wrong thing _anywhere_ on the internet, some platforms my remove you.
* Look for providers that espouse a speech policy that you agree with. If you like Twitter and Facebook's policies on hate speech, awesome, use them! If you like Gab and Parler's approach to free speech, use them!
* Don't be afraid to use multiple platforms simultaneously. I anticipate an uptick in tooling to automatically post to multiple platforms, though for now simply having a presence in multiple places is possible manually.

You may notice that we're heading towards a world of bifurcated communication. Not passing a value judgement, but it's likely that Twitter and Facebook will become more filled with people in favor of censoring hate speech, and Gab, Parler, and others will become more filled with people who believe there's no such thing as hate speech.

## Specific options

Here's a short analysis of different platforms I'm aware of for different purposes. I don't actually use all of these, and can't really speak to how well they work, what kind of content they have, or what their censorship practices are. There are _many_, _many_ other options out there too.

### Text based microblogging

* Twitter is the obvious king here, with the largest platform by far. It's also clearly on the side of censoring some content.
* Gab is one of the oldest free speech competitors to Twitter. It's been labeled as "Nazi Twitter," and so far has stood up to all attempts to censor it. It runs its own physical machines to avoid companies like Amazon, and has been banned from many payment processors. It has no mobile apps in the app stores, since Apple and Google have banned them. Without much experience with it, it's probably the best "free speech microblogging" platform out there. It's also developing additional features, like news aggregation, video hosting, and commenting.
* Parler is recently becoming popular as a free speech competitor. Unlike Gab, it has (or, at least, had) mobile apps. And it hosts on Amazon. Unlike Gab, it seems to have a strong presence from major right-wing political figures, which has seemingly given in meteoric popularity. However, as I'm writing this, Parler is being banned from the Google PlayStore, may be banned by Apple, and Amazon is planning on kicking them off their servers. Ultimately, it looks like Parler may simply be relearning lessons Gab already learned.

You likely will be tarred and feathered as a Nazi for using Gab or Parler. I'll admit that I've mostly avoided Gab because the content I discuss is purely technical and not at censorship risk, and I didn't feel like bringing ire down upon myself. But I am starting to post my content to Gab and Parler as well. Getting ever so slightly political: I hope people start using these platforms for everyday posts so that we don't have two raging political extremes yelling at each other from separate platforms.

### Video hosting

* YouTube is the clear winner here. It's owned by Google, and has been pretty much in line with Twitter and Facebook's approaches to censorship.
* Vimeo is one of the oldest alternatives to YouTube. I've used it in the past. I honestly have no idea what its take is on censorship.
* BitChute and Rumble are two alternative video hosting platforms I've seen. I've barely used either one. I will admit that I was a little shocked one of the times I checked out BitChute to see some hardcore Nazi videos. Whether you see that statement as an indictment or endorsement of BitChute should give you an indication of whether you embrace anti-hate speech or free speech more.

### Live streaming

* YouTube and Twitch are the two most popular live streaming platforms, and both have embraced some level of censorship.
* I've heard of DLive as an alternative, and tested it out briefly when I was learning about live streaming myself, but haven't investigated it further. I've also heard it referred to as "Nazi Twitch."
* I've heard that Rumble is going to be adding live streaming functionality.

### Others

* I don't know anything about photo sharing ala Instragram
* I don't know about podcast apps, I've recently given up on podcasts to spend more time on more useful activities
* I don't think there's a real alternative to Facebook from a "stay in touch with family" standpoint
* I can't even begin to guess at what would be considered a "good" news aggregator.
* Reddit is the largest forum/discussion board. Famously, they blocked `r/thedonald`, which created its own site TheDonald.win.

## Network effects

With any of these systems, an important aspect is what's known as "network effects." That means that the system becomes more valuable the more people that use them. WhatsApp is dominant _because it's dominant_. The more people using it, the more people want to use it. In economic terms, these might be termed _natural monopolies_.

The fact that people who use alternative platforms can be attacked for this increases the network effects. "You're using Signal? What are you, some tinfoil hat conspiracy theorist?" "You're using Gab? What are you, a Nazi?" Again, not passing judgement, just an honest assessment. It's mostly considered non-controversial to use Twitter, Facebook, YouTube, and WhatsApp. You must have some weird reason to use the others.

Keep that in mind when making your decisions. If you decide to ask your family to communicate on Signal, give them a reason for why you're doing it. And if you can't justify it, maybe the privacy and anti-censorship arguments don't really resonate with you.

## Blockchain?

No blog post in the past 5 years is complete without mentioning blockchain. I've worked extensively in blockchain for the past four years or so. My company has built blockchain systems, and we've audited a number of them as well. I can tell you a few things:

* There are a _lot_ of incorrect ideas out there about blockchain
* Blockchain concepts could definitely be used to build private communications systems and anti-censorship social media
* There is a majorly incorrect belief that systems like Bitcoin are "anonymous." They are not. All transactions are publicly recorded. If anyone figures out what your address is, they know what you've done with your money. Caveat emptor.

There are other blockchain systems that introduce true privacy through very sophisticated means. I personally don't use cryptocurrencies in any meaningful way, and I'm not going to recommend them to everyday people. Maybe at some point in the future I'll write more about them, but not today. For now: I don't consider blockchain any kind of solution to the problems listed here, for most normal people, at today's level of maturity of the technology.

## Other security recommendations

If you've gotten this far, congratulations. I've covered everything I promised I'd cover. But since you're here, let me lay out a few other security recommendations for laymen.

* Install a password manager! Stop using the same password on every site. Password managers are secure and easy to use. I personally use [BitWarden](https://bitwarden.com/) and have gotten my family and company onto it. Others include LastPass, 1Password, and KeePass.
* Don't install random software for the internet. You're far more likely to lose privacy by installing some spyware than anything else.
* Limit the number of browser extensions you use. Each extension is a potential intrusion into your online communications. I use the BitWarden extension, with some level of trepidation.
* Look at URLs before clicking a link to avoid phishing. Phishing attacks, and social engineering attacks in general, are a primary way to be compromised online.
* Secure your devices. Make sure you have a password or biometric lock on your devices. Don't lose your devices. If you do lose your device, reset your passwords.
* Use advanced credential management like two factor auth whenever possible. _Especially_ if you use the same password everywhere. But seriously: use a password manager. Consider using a system with cloud backup like Authy.

OK, that's it, I promise. Let me know in the comments below, or on [Twitter](https://twitter.com/snoyberg) or [Gab](https://gab.com/snoyberg), if you have additional questions that you'd like me to answer. And if people want, I may turn this blog post into a video as well.
