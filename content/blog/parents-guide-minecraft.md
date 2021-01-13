+++
title = "A parents' guide to Minecraft"
description = "Some hopefully helpful information for fellow parents who have kids that want to play Minecraft.\n"
path = "/blog/2021/01/parents-guide-minecraft"
date = 2021-01-09

[taxonomies]

[extra]
ogimage = "https://i.imgur.com/7Hyo979.png"
+++
With the COVID-19 lockdowns, our children started getting interested in Minecraft. It started with our eldest (12 years old), who wanted to play on a server with his friends. Later, our 10 and 8 year olds wanted to get involved too. This seems like it should have been a straightforward experience: buy the game, install it, walk away.

However, between the different platforms, accounts, editions, and servers, we ran into sufficient pain points that I decided it would be worth writing up a guide for other parents getting their kids started with things.

As a quick plug: among video games for kids, we put Minecraft towards the top of our recommended list. It requires thought, planning, and strategizing to play. Especially during lockdown, together with a good [Discord audio channel](https://discord.com/), Minecraft can get a great way to keep the children semi-social with their peers. And as parents, even we have gotten into the game more recently.

OK, let's get into it!

## Editions

If you look around the internet, there are a _lot_ of different editions of Minecraft. Most of that is historical. These days, there are just two editions: Java and Bedrock. Pocket, Console, and "Nintendo Switch" editions (and I'm sure there are others) are now part of Bedrock.

This is the first major decision you'll need to make for/with the kids: which edition of Minecraft are you going to buy? My short recommendation is: if you don't have a reason to do otherwise, go with Bedrock. But let's dive into the reasons for Java first, and then come back to Bedrock.

### Java edition

The Java edition of Minecraft is the original version of the game. It runs on Windows (including older Windows versions), Mac, and Linux. There are a lot of "mods" out there that allow you to change how the games work. There are various servers with interesting alternative versions of the game in place.

Each individual user of the Java edition will need their own license to play. You can definitely let multiple children all play on a single computer, but they won't be able to play together.

And finally: the Java edition can only play multiplayer with other Java players. So if your kids have friends playing on Java (which ours did), you'll need to use Java as well to play.

### Bedrock edition

After Microsoft bought Mojang (the company behind Minecraft), they created a new version of the codebase in C++. I can only assume that they did this to make a more efficient and more compatible version of the game. That's the first thing to point out: Bedrock runs a bit faster than Java, and runs on many more platforms.

Bedrock is available for Windows 10 PCs, though no other computers. If you're on older versions of Windows, or on Mac or Linux, Java is your only option. There are mobile versions of iOS and Android. And there are versions of Xbox, PlayStation, and Nintendo Switch. If you have any desire to play from a mobile or console, you'll have to use Bedrock.

The ability to share the game across multiple users depends entirely on the platform. On the Switch, for instance, everyone in the family can play from their own account on our main Switch device. I believe family sharing for iOS and Android would let the whole family play with a single purchase, though we haven't tried it.

The Windows 10 story is the most confusing. I've seen conflicting information on family sharing for the Microsoft Store. I _think_ the intent is that you can make one purchase for the entire family, but I'm honestly not certain. Microsoft should improve their documentation on this. In any event, here's [an article on microsoft.com from 2015](https://docs.microsoft.com/en-us/archive/blogs/henrikn/sharing-windows-10-store-apps-with-your-family) that seems to address this case. Given that I really don't know Microsoft's intent, I'm not going to give any concrete recommendations on how many licenses to purchase.

### Our decision

We bought the Nintendo Switch edition a while ago (which ultimately changed to the Bedrock edition). We also bought the Java edition for our eldest, Bedrock next for the Windows 10 PCs in the house, and ultimately after a _lot_ of nagging bought the other two kids the Java edition as well. On the one hand, the costs add up. On the other hand, this game has been a hit for about 9 months now, so amortized the cost is completely reasonable.

## Accounts

Microsoft jumps in yet again to confuse things! This one's a bit easier to explain though, now that we know about editions. Java edition originally had its own user accounts, known as Mojang Accounts. Those are currently being converted into Microsoft accounts, which I'll describe next. If you purchase a new Java license, just go ahead and use a Microsoft account. If you already have a Java license, you'll need to move the Mojang account to a Microsoft account in the near future.

If you're not in the Microsoft world&mdash;and that describes me about 2 years ago&mdash;you may know nothing about Microsoft accounts. Microsoft accounts tie into many different services, such as Microsoft 365 (no surprise there), Skype, and Xbox Live. Minecraft is really leveraging that last bit. If you're on Bedrock edition, Minecraft will use your Xbox Live profile, your Xbox Live friends, and so on.

As a parent, you can create a family group and add your children to it, which will let you know how much time they spend playing games on Windows 10 and Xbox.

**NOTE** Microsoft _really_ screwed up naming, and has both a "Microsoft Account" and a "Microsoft Work or School Account". If you're at a company that uses something like Microsoft Teams, you have one of the latter. Just be aware that these two kinds of accounts are _mostly_ separate, except for a few websites (like Azure DevOps) that let you log in as both. All of this is stated as someone who pulled off a Google=>Microsoft migration for my company about a year ago.

## Multiplayer

Server based play, which we'll get to next, is one way of doing multiplayer Minecraft. But it's not the only one, and definitely not the easiest method. This point definitely confused me at first.

When you start a Minecraft game, you are creating a world. On the Java edition, you'll do this from the "Single Player" menu. Despite that nomenclature, you are still able to play with other people in a world you created in single player. The same idea applies in Bedrock, though Bedrock fortunately doesn't apply the confusing term "single player" to it.

On both Java and Bedrock, you can find games being played by others inside your local area network (LAN), meaning other people in the house. If you're on Bedrock, you can join worlds of other Xbox Live friends. The latter is a much easier way to connect with friends.

The caveat to this is that you can only play this multiplayer game when the host&mdash;the person who created the world&mdash;is online. If you want to be able to play regardless of who's online, you'll need a server.

## Servers

If you want to leave a world up for any of your family or friends to join, you'll want a server. Both Java and Bedrock provide server software which you can freely run yourself. Personally, I decided that I spend enough time at work running servers for people, and I'd rather just pay someone else to do it for me.

Microsoft offers a feature called Realms, where you can pay them $8 a month (at time of writing) to host your realm. It's basically just a server with membership controlled via Xbox Live accounts. It's a nice, easy way to invite others to join you.

There are plenty of other companies out there offering free and paid hosting options. Keep in mind that, since it's relatively new, there aren't as many Bedrock hosting options out there. And the free options typically have some significant limitations, such as requiring that you watch an ad on their site every 90 minutes to keep the server running.

If you're like me, and don't want to have to bother with maintaining this stuff, I'd recommend budgeting $5-$10 per month for a server, if that's what your kids are going to enjoy.

### DNS madness

And one final note. The thing that finally got me to write this blog post was a _really_ frustrating bug where my daughter's computer couldn't connect to servers. I still haven't discovered exactly why that happened, but I learned more about how DNS resolution works:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Alright, I think I figured it out. I don&#39;t know where exactly this is properly documented, but it seems you need an SRV record on:<br><br> _minecraft._tcp.&lt;HOSTNAME&gt;<br><br>I&#39;ve been planning on writing a &quot;Minecraft guide for parents&quot; blog post... now I think I have to.</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/1347946862008000516?ref_src=twsrc%5Etfw">January 9, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

If, like me, you're a tech person/network guy who is stumped as to "where the hell is the DNS record," you're looking for an SRV record with `_minecraft._tcp` prepended. I have no idea where this is officially documented, but since I spend so much time pulling out my three remaining hairs on this, I figured I'd share it here.

## Encouragement

Our kids have absolutely fallen in love with Minecraft, and spend basically every waking moment talking about it. They play Minecraft music videos, have convinced the 4-year-old to play make-believe Minecraft, play with Minecraft foam swords and lego sets, etc. You get it, it's an obsession. If you go down this path, prepare for the possibility that they will talk at you about Minecraft every chance they get.

For me, when I finally started playing the game, it was a major bonding experience with the kids. They got to teach me a lot, which was fun for them. I had better planning and organization skills, and for once they were willing to listen to me when I told them to organize their closets (i.e. chests).

If you're looking for milestones and achievements to set, I'd recommend these. Keep in mind that I'm still quite a newb at the game, and there are likely much more exciting things to discover too.

* **Build a base** There are so many variations on bases you can build. We're currently working with an underground base, but there are so many other styles you can make.
* **Mine diamonds** Diamonds are one of the best materials in the game, and proper mining of them requires quite a bit of patience. Read up on some tutorials about getting down to Y=12 (meaning, 12 layers from the bottom of the game) to maximize your mining potential.
* **Explore the map** Consider taking a locator map and a boat for this.
* **Explore the nether** You will die.
* **Start enchanting items** You'll need diamonds, obsidian, and will want bookshelves to maximize your enchantments. That will require lots of leather (from cows) and paper (from sugar cane). The planning to make all of that work is a great exercise.
* **Mix potions** I haven't done this yet, we still haven't found Nether Wart.
* **Defeat the Ender Dragon** We did this once, and it was _incredibly_ exciting.
* **Play with red stone** I don't know much about it yet, but from what I've heard it's a lot like programming with transistors. Red stone conducts "red stone energy" (basically electricity), and then there are things you can create for gating that energy and other such things.
* **Be creative** Survival mode, the default mode, will result in a lot of deaths from enemies, lava, falling, drowning, and more. Creative mode lets you explore your creative side, with no death, and an infinite supply of all of the materials. It can be a fun way to get started.

If I got anything wrong or left anything out, let me know! Happy Minecrafting!
