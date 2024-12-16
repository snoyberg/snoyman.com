+++
title = "Hello Nostr"
description = "An introduction to the Nostr social media protocol, written by a fresh user who's been on the platform for less than a week."
path = "/blog/2024/12/hello-nostr"
date = "2024-12-16"
+++
This blog post is in the style of my [previous blog post on Matrix](@/blog/guide-to-matrix-riot.md). I'm reviewing and sharing onboarding experience with a new technology. I'm sharing in the hopes that it will help others become aware of this new technology, understand what it can do, and if people are intrigued, have a more pleasant onboarding experience. Just keep in mind: I'm in no way an expert on this. PRs welcome to improve the content here!

* [https://nostr.com/](https://nostr.com/)  
* [https://nostr.org/](https://nostr.org/)  
* [https://nostr.how/](https://nostr.how/)  
* [https://github.com/aljazceru/awesome-nostr](https://github.com/aljazceru/awesome-nostr)
* [Onboarding post from Derek Ross](https://njump.me/nevent1qgsr7acdvhf6we9fch94qwhpy0nza36e3tgrtkpku25ppuu80f69kfqqypw05fjshncr453ltvtvn67swrv2zhz8xvwxcjxr7vl8wse6lv6njzum09e)
* [Onboarding post from Rod](https://njump.me/nevent1qgsrtuazdng7waddzmdd3nyw36w5sfvler95vgcte9nreqf8jtwlyvgqyz3htukyplqltu65rlcx4p7fa2w8c5r39mg05ghaj9ae9saqlyr277geeeu)

## What is Nostr? Why Nostr?

I’d describe Nostr as decentralized social media. It’s a protocol for people to identify themselves via public key cryptography (thus no central identity service), publish various kinds of information, access through any compatible client, and interact with anyone else. At its simplest, it’s a Twitter/X clone, but it offers much more functionality than that (though I’ve barely scratched the surface).

Nostr has a high overlap with the Bitcoin ecosystem, including built-in micropayments (zaps) via the Lightning Network, an instantaneous peer-to-peer payment layer built on top of Bitcoin.

I'll start off by saying: right now, Nostr's user experience is _not_ on a par with centralized services like X. But I can see a lot of promise. The design of the protocol encourages widespread innovation, as demonstrated by the plethora of clients and other tools to access the protocol. Decentralized/federated services are more difficult to make work flawlessly, but the advantages in terms of freedom of expression, self-custody of your data, censorship resistance, and ability to build more featureful tools on top of it make me excited.

I was skeptical (to say the least) about the idea of micropayments built into social media. But I'm beginning to see the appeal. Firstly, getting away from an advertiser-driven business model fixes the age old problem of "if you're not paying for a service, you're the product." But I see a deeper social concept here too. I intend to blog more in the future on the topic of non-monetary competition and compensation. But in short, in the context of social media: every social network ends up making its own version of imaginary internet points (karma, moderator privileges, whatever you want to call it). Non-monetary compensation has a lot of downsides, which I won't explore here. Instead, making the credit system based on money with real-world value has the promise to vastly improve social media interactions.

Did that intrigue you enough to want to give this a shot? Awesome! Let me give you an overview of the protocol, and then we'll dive into my recommendation on getting started.

## Protocol overview

The basics of the protocol can be broken down into:

* Relays
* Events
* Identities
* Clients

As a decentralized protocol, Nostr relies on public key cryptography for identities. That means, when you interact on the network, you'll use a private key (represented as an `nsec` value) to sign your messages, and will be identified by your public key (represented as an `npub` value). Anyone familiar with Bitcoin or cryptocurrency will be familiar with the keys vs wallet divide, and it lines up here too. Right off the bat, we see the first major advantage of Nostr: no one controls your identity except you.

Clients are how a user will interact with the protocol. You'll provide your identity to the client in one of a few ways:

* Directly entering your `nsec`. This is generally frowned upon since it opens you up for exploit, though most mobile apps do work by direct `nsec` entry.
* Getting a view-only experience in clients that support it by entering your `npub`.
* Using a signing tool to perform the signing on behalf of the client without giving away your private keys to everyone. (This matches most web3 interactions that rely on a wallet browser extension.)

Events are a general-purpose concept, and are the heart of Nostr interaction. Events can represent a note (similar to a Tweet), articles, likes, reposts, profile updates, and more. Anything you do on the protocol involves creating and signing and event. This is also the heart of Nostr's extensibility: new events can be created to support new kinds of interactions.

Finally there are relays. Relays are the servers of the Nostr world, and are where you broadcast your events to. Clients will typically configure multiple relays, broadcast your events to those relays, and query relays for relevant events for you (such as notes from people you follow, likes on your posts, and more).

## Getting started

This is where the suboptimal experience really exists for Nostr. It took me a few days to come up with a setup that worked reliably. I'm going to share what worked best for me, but keep in mind that there are many other options, I'm a novice, other guides may give different recommendations, and you may not like my selection of tools. My best recommendation: don't end up in shell shock like I did. Set up _any_ kind of a Nostr profile, introduce yourself with the `#introductions` hashtag, and ask for help. I've found the community unbelievably welcoming.

Alright, so here are the different pieces you're going to need for a full experience:

* Browser extension for signing
* Web client
* Mobile client
* Lightning wallet
* A Nostr address

I'm going to give you a set of steps that I hope both provides easy onboarding while still leaving you with the ability to more directly control your Nostr experience in the future.

### Lightning wallet: coinos

First, you're going to set up a Lightning wallet. There are a lot of options here, and there are a lot of considerations between ease-of-use, self-custody, and compatibility with other protocols. I tried a bunch. My recommendation: use [coinos](https://coinos.io/). It's a custodial wallet (meaning: they control your money and you're trusting them), so don't put any large sums into it. But coinos is really easy to use, and supports Nostr Wallet Connect (NWC). After you set up your account, click on the gear icon, and then click on "Reveal Connection String." You'll want to use that when setting up your clients. Also, coinos gives you a Lightning address, which will be `<username>@coinos.io`. You'll need that for setting up your profile on Nostr.

### Web client: YakiHonne

I tried a bunch of web clients and had problems with almost all of them. I later realized most of my problems seemed to be caused by incorrectly set relays, which we'll discuss below. In any event, I ultimately chose [YakiHonne](https://yakihonne.com/). It also has mobile iOS and Android clients, so you can have a consistent experience. (I also used the Damus iOS client, which is also wonderful.)

Go to the homepage, click on the Login button in the bottom-left, and then choose "Create an account." You can add a profile picture, banner image, choose a display name, and add a short description. In the signup wizard, you'll see an option to let YakiHonne set up a wallet (meaning a Lightning wallet) for you. I chose not to rely on this and use coinos instead to keep more flexibility in swapping clients in the future, but if you want to simplify, you can just use the built-in wallet.

Before going any further, *make sure you back up your `nsec` secret key!!!* Click on your username in the bottom-left, then settings, and then "Your keys." I recommend saving both your `nsec` and `npub` values in your password manager.

![YakiHonne keys](/img/hello-nostr/yakikeys.png)

*No, this isn't my actual set of keys, this was a test profile I set up while writing this post.*

Within that settings page, click on "wallets," then click on the plus sign next to add wallets, and choose "Nostr wallet connect." Paste the NWC string you got from coinos, and you'll be able to zap people money!

Next, go back to settings and choose "Edit Profile." Under "Lightning address," put your `<username>@coinos.io` address. Now you'll also be able to receive zaps from others.

Another field you'll notice on the profile is NIP-05. That's your Nostr address. Let's talk about getting that set up.

### NIP-05 Nostr address

Remembering a massive `npub` address is a pain. Instead, you'll want to set up a NIP-05 address. (NIP stands for Nostr Implementation Possibilities, you can see [NIP-05 on GitHub](https://github.com/nostr-protocol/nips/blob/master/05.md).) There are many services&mdash;both paid and free&mdash;to get a NIP-05 address. You can see a [set of services on AwesomeNostr](https://github.com/aljazceru/awesome-nostr?tab=readme-ov-file#nip-05-identity-services). Personally, I decided to set up an identifier on my own domain. You can [see my live nostr.json file](https://www.snoyman.com/.well-known/nostr.json), which at time of writing supports:

* `michael@snoyman.com` and an alias `snoyberg@snoyman.com`
* The special `_@snoyman.com`, which actually means "the entire domain itself"
* And an identifier for my wife as well, `miriam@snoyman.com`

If you host this file yourself, keep in mind these two requirements:

* You cannot have any redirects at that URL! If your identifier is `name@domain`, the URL `https://domain/.well-known/nostr.json?name=<name>` _must_ resolve directly to this file.
* You need to set CORS headers appropriately to allow for web client access, specifically the response header `access-control-allow-origin: *`.

Once you have that set up, add your Nostr address to your profile on YakiHonne. Note that you'll need the hex version of your `npub`, which you can generate by using the [Nostr Army Knife](https://nak.nostr.com/):

![Nostr Army Knife](/img/hello-nostr/nok.png)

**BONUS** I decided to also set up my own Lightning wallet address, by rehosting the Lightning config file from `https://coinos.io/.well-known/lnurlp/snoyberg` on my domain at `https://snoyman.com/.well-known/lnurlp/michael`.

### Signer

As far as I can tell, [Alby](https://getalby.com/) provides the most popular Nostr signing browser extension. The only problem I had with it was confusion about all the different things it does. Alby provides a custodial lightning wallet via Alby Hub, plus a mobile Alby Go app for accessing it, plus a browser extension for Nostr signing, and that browser extension supports using both the Alby Hub wallet and some other lightning wallets. I did get it all to work together, and it's a pleasant experience.

![nos2x](/img/hello-nostr/nos2x.png)

However, to keep things a bit more simple and single-task focused, I'll recommend trying out the [nos2x extension](https://github.com/fiatjaf/nos2x) first. It's not pretty, but handles the signer piece very well. Install the extension, enter the `nsec` you got from YokiHonne, click save, and you're good to go. If you go to another Nostr client, like [noStrudel](https://nostrudel.ninja), you should be able to "sign in with extension."

You may also notice that there's any entry area for "preferred relays." We'll discuss relays next. Feel free to come back to this step and add those relays. (And, after you've done that, you can also use a [nostr.json generator](https://snowcait.github.io/nostr-json-generator) to help you self-host your NIP-05 address if you're so inclined.)

Final note: once you've done the initial setup, it's not clear how to get back to the nos2x settings page. Right-click the extension, click manage extension, and then choose "extension options." At least those were the steps in [Brave](https://brave.com/), it may be slightly different in other browsers.

### Relays

This has been my biggest pain point with Nostr so far. Everything you do with Nostr needs to be sent to relays or received from relays. You want to have a consistent and relatively broad set of relays to make sure your view of the world is consistent. If you don't, you'll likely end up with things like mismatched profiles across relays, messages that seem to disappear, and more. This was probably my biggest stumbling block when first working with Nostr.

There seem to be three common ways to set the list of relays:

* Manually entering the relays in the client's settings.
* Getting the list of relays from the signer extension (covered by [NIP-07](https://github.com/nostr-protocol/nips/blob/master/07.md)).
* Getting the list of relays from your NIP-05 file.

Unfortunately, it looks like most clients don't support the latter two methods. So unfortunately, any time you start using a new client, you should check the relay list and manually sync it up with a list of relays you maintain.

You can look at [my nostr.json file](https://www.snoyman.com/.well-known/nostr.json) for my own list of relays. One relay in particular I was recommended to use is `wss://hist.nostr.land`. This relay will keep track of your profile and follow list updates. As I mentioned, it's easy to accidentally partially-override your profile information through inconsistent relay lists, and apparently lots of new users (myself included) end up doing this. If you go to [hist.nostr.land](https://hist.nostr.land/) you can sign in, find your historical updates, and restore old versions.

### Mobile

You're now set up on your web experience. For mobile, download any mobile app and set it up similarly to what I described for web. The major difference will be that you'll likely be entering your `nsec` directly into the mobile app.

I've used both [Damus](https://damus.io/) and [YakiHonne](https://yakihonne.com/yakihonne-mobile-app). I had better luck with YakiHonne for getting zaps working reliably, but that may simply be because I'd tried Damus before I'd gotten set up with coinos before. I'll probably try out Damus some more soon.

Note on Damus: I had trouble initially with sending Zaps on Damus, but apparently that's because of [Apple rules](https://njump.me/nevent1qgsr9cvzwc652r4m83d86ykplrnm9dg5gwdvzzn8ameanlvut35wy3gqyp6m8dphmzjfpynmvzsr88mkkulmq2npm92eewr0h5p92jzujn0lvz79rnt). You can enable Zaps by visiting this site on your device: <https://zap.army/>. Thanks [William Cassarin](https://njump.me/jb55.com) for the guidance and the great app.

## Introductions

You should now be fully set up to start interacting on Nostr! As a final step, I recommend you start off by sending an introduction note. This is a short note telling the world a bit about yourself, with the `#introductions` hashtag. For comparison, here's [my introduction note](https://yakihonne.com/notes/nevent1qgsw8wj7rgrwz8yxqqmtt30x3qqjhc4ggas2hsrx4s62px2ntepnxegqyrnqq2ljscepjmr3spkrwcequhqkl7vtgps292e8uxac9qr5dkljul7t767) (or a [Nostr-native URL](nostr:nevent1qgsw8wj7rgrwz8yxqqmtt30x3qqjhc4ggas2hsrx4s62px2ntepnxegqyrnqq2ljscepjmr3spkrwcequhqkl7vtgps292e8uxac9qr5dkljul7t767)).

And in addition, feel free to `@` me in a note as well, I'd love to meet other people on Nostr who joined up after reading this post. My identifier is `michael@snoyman.com`. You can also check out my [profile page on njump](https://njump.me/michael@snoyman.com), which is a great service to become acquainted with.

And now that you're on Nostr, let me share my experiences with the platform so far.

## My experience

I'm definitely planning to continue using Nostr. The community has a different feel to my other major social media hub, X, which isn't surprising. There's a lot more discussion of Bitcoin and economics, which I love. There's also, at least subjectively, more of a sense of having fun versus X. I described it as joyscrolling versus doomscrolling.

Nostr is a free speech haven. It's quite literally impossible to fully silence someone. People can theoretically be banned from specific relays, but a banned user could always just use other relays are continue to create new keys. There's no KYC process to stop them. I've only found one truly vile account so far, and it was easy enough to just ignore. This fits very well with my own personal ethos. I'd rather people have a public forum to express _any_ opinion, especially the opinions I most strongly disagree with, including calls to violence. I believe the world is better for allowing these opinions to be shared, debated, and (hopefully) exposed as vapid.

The process of zapping is surprisingly engaging. The amount of money people send around isn't much. The most common zap amount is 21 satoshis, which at the current price of Bitcoin is just about 2 US cents. Unless you become massively popular, you're not going to retire on zaps. But it's far more meaningful to receive a zap than a like, it means someone parted with something of actual value because you made their day just a little bit better. And likewise, zapping someone else has the same feeling. It's also possible to tip providers of clients and other tools, which is a fundamental shift from the advertiser-driven web of today.

I'd love to hear from others about their own experiences! Please reach out with your own findings. Hopefully we'll all be able to push social media into a more open, healthy, and fun direction.
