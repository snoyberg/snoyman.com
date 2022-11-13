+++
title = "Why my video calls sucked (and how I fixed it)"
description = "My ISP gave me a crappy router and I had 3 years of terrible video calls and gaming. Deep dive explanation of what happened and how to fix it for yourself."
date = 2022-11-13
+++
A few years ago, I [wrote a blog post](https://www.snoyman.com/blog/2019/06/my-new-home-network-setup/) about how I set up the networking in our house following some major construction. I was really excited about how much better the internet would be. No more WiFi dead zones, and a wired connection to my computer to provide extra speed and stability to my office. Overall, the results were great. Speed test anywhere in my house showed I was getting the full 500mbps promised by my cable company. However, not everything was working as expected:

* Video calls stuttered, a lot. The most egregious and confusing behavior was that, during a call, I would continue to see the video moving while people went silent for between 10 and 20 seconds.
* My kids complained off-and-on about problems in online games (Minecraft in this case), and had trouble talking with friends over Discord audio chat.

If you're looking for a summary "try this if you're having trouble," here are my three recommendations:

* Diagnosis: try tethering to your smartphone instead of using the internet in your house and see if the behavior is better. You'll almost certainly have slower speeds, but video calls and gaming may be more consistent.
* Stop gap measure: try running a VPN of some kind and see if that improves the situation. One possibility is trying out [Cloudflare Warp](https://1.1.1.1/). This helped significantly for me, but wasn't perfect.
* Real fix for the underlying problem: buy a new router, connect it to the modem/router from your Internet Service Provider (ISP), and put the modem into bridge mode.

The rest of this blog post will try to explain what the problem is. We're going to get into the technicals, but I'm hoping the content will make sense to anyone with basic experience on the internet, not just networking engineers.

Finally, it's worth calling out two coworkers for their involvement in this story. First is Niklas Hambüchen, who years ago warned me of the perils of ISP-provided routers. I should have listened to him then. The second is Neil, who not only helped me debug this along the way, but also accidentally gave me the clue I needed to isolate the problem.

## Analyzing the problem

If you're suffering from the problems I describe above, it can be _incredibly_ frustrating. Not only do video calls turn into a source of endless pain and miscommunication, but no one will believe you. Call the ISP, and they'll tell you your speed tests are fine. Same with hardware manufacturers, operating systems, and the video software itself in most cases. Nothing sees the problem. You know something is broken, but you're essentially told you're crazy.

The big hints to me that _something_ more complicated was happening under the surface was which things worked well and which didn't. Watch some videos online? No problem at all. Browse websites? Fine. Massive downloads (pretty common in my line of work)? Incredibly fast. The fact that calls and gaming were broken was the first indication something was weird.

The final puzzle piece hit a few weeks ago. The aforementioned Neil had told me for a while how great VR gaming was, especially a game called Echo Arena, and so we ordered an Oculus. I loaded up the game, went into the lobby... and timed out. I tried that a few more times, and it kept happening. Then I tried using my phone as a mobile hotspot, and the game worked perfectly.

Before I explain why that was so important, we have to talk about a few lower level details of networks.

## Packets, addresses, and ports

We often use terms like "connecting" to a website. In a physical sense, that doesn't happen. When I go to YouTube.com, I don't have a physical cable, radio signal, or any other physical manifestation of a connection between my computer and some computer at YouTube headquarters. Instead, the way the internet works is a series of computers that connect to each other and pass data around to each other. This is known as _routing_.

Every computer on a network has an Internet Protocol (IP) address. These are numbers that look like 192.168.54.32. You've probably seen them at some point. The basic idea of routing traffic is I say to the next computer in the line "hey, I want to talk to a computer with that address." The next computer may have connections to 5 other computers, and it knows which of those computers is closest to that IP address. It figures this out using a routing table. The data then "hops" from that computer to the next one, from there to another computer, and so on until it reaches its destination.

But like I said, there aren't any "connections." Instead, internet traffic is made up of a bunch of "packets." You can think of these as envelopes. They have an IP address on the outside, and a small amount of data inside. When you "connect" to another computer, you're actually sending a bunch of these packets over the network. The computers in the middle route your traffic by looking at the outside of the envelope (called the _header_). And your packets make it to their destination.

One other thing to keep in mind. Each computers can talk to lots of other computers at the same time. Each computer may provide different ways to talk to it (known as _protocols_, such as the web, or email, or video calling). To allow a single computer to do all these things at the same time, we have one more important number: the port number. This is a number between 1 and 65,536, and tells the computer which "connection" traffic is trying to use. When you send a packet, your header includes the destination IP address and destination port number. It also includes the source IP address and source port number. This allows the other computer to respond to you.

## Packet loss, UDP, and TCP

OK, one more topic to learn. There are actual physical mechanisms that control networks. It could be a network cable, a WiFi signal, a statelite connection to Starlink, or the 4G cellular signal on your phone. They all share one thing in common: they can lose data. Maybe you're driving through a tunnel and the cell signal is interrupted. Maybe you live in my neighborhood, and the cable company still hasn't properly protected their cables from water and you lose internet every time it rains. Whatever the case, and for whatever reason, it's entirely possible to lose some of the data. This is known as _packet loss_.

There are two basic approaches in networking to dealing with packet loss, each with their own advantages and disadvantages.

* User Datagram Protocol, or UDP, is a protocol that works as a "fire and forget" message. I send data to the other side, and I have no guarantees of if it arrived or the order it will arrive in (maybe packet 513 will get there before packet 512). Maybe the same packet will get received multiple times. No one knows.
* Transmission Control Protocol, or TCP, is what people normally think of as a "connection" on the internet. TCP adds a bunch of bookkeeping rules to address the limitations of UDP. It makes sure packets arrive in the correct order, resending them if they didn't get to the other side. It makes sure the order of packets is correct. And it lets you know if the other side breaks the connection.

You may be wondering: why in the world would anyone ever use UDP? It sounds terrible! Overall, TCP is more commonly used for sure, because most people need those guarantees most of the time. But there are some use cases where UDP is far superior. (And finally this weird tangent will connect back to the beginning of the blog post.) UDP is great when:

* You don't actually need every single bit of data to arrive on the other side.
* You care much more about raw speed than other factors.

There are two great examples of this:

1. Audio calls! It turns out that if you take a stream of audio, you can break it down into a whole bunch of tiny data packets containing a slice of time. Then you can send them over the network. If one of those packets is lost, the other side can usually understand what you said from all the other audio packets. And having to add in the delays imposed by TCP to ensure all the data arrives would impose more delays, causing audio calls to become "laggy," or to be more technical, would introduce _latency_.
2. Gaming. In lots of video games, we don't need to have every single bit of data about what the user did. There are ways to write gaming protocols that say things like "don't tell me the user pressed up for 5 seconds, tell me their new position." If one of those updates gets lost, it's no big deal, the next update will give the newer position. You may "glitch" a bit in the game and jump around, but again, overall, the speed is more important than every piece of data.

There are other examples of UDP being superior, but I won't bother covering them here, because now we know enough to see what was happening in my house.

## What's in a router?

It turns out that Echo Arena, that game I tried playing, was communicating with the server over UDP. And for whatever reason, it was more sensitive to the breakage in my house than other things like video calls and Minecraft. It turns out, the router in our house was mishandling UDP packets.

Most people get a router from their cable, DSL, or fiberoptic company when they pay them for internet access. But this device generally is _not_ just a router. It's actually doing three different jobs most of the time, and we need to separate those out:

* *Modem*. The term modem means a device that converts one kind of physical connection into a network connection. Cable modems, for example, convert the TV cable wires already running into your house into a network signal, something they weren't originally designed for. Fiber modems will convert the fiberoptic light-based signals into a network signal. DSL does the same with phone lines. Even old-school dial-up modems are simply using audio over the phone line for the same purpose.
* *Wireless access point*. You can connect to your "router" by plugging in a network cable to the back and connecting that to your computer. But most people these days are using a WiFi signal instead. A wireless access point is the translator between WiFi signals and your wired network. In the case of your modem/wireless router combo, it's built into the device, but you _could_ use an external one. (And this is a great way to extend the range of your wireless network if you need to.)

And finally, we get to the router itself. There are two more things we need to learn about IP addresses:

* There are some IP addresses that have been reserved as "private," meaning they can be used inside people's homes or businesses, but can't go on the internet. This includes anything that starts with `10.` or `192.168.`. If those look familiar... just wait a second, we'll get to it.
* There are only 4 billion IP addresses possible. That may seem like a lot, but it turns out that it isn't nearly enough for all the people, servers, Internet of Things devices, and everything else that wants to be on the internet. We have an IP address shortage.
    *   Side note: the current common IP address standard is called IPv4, and is what I'm referring to. There's a new standard, called IPv6, that totally solves this problem by introducing an insane number of addresses. To [get a sense of how big](https://www.techtarget.com/whatis/feature/IPv6-addresses-how-many-is-that-in-numbers):

        > 340,282,366,920,938,463,463,374,607,431,768,211,456, which is approximately 340 undecillion addresses...
        >
        > So we could assign an IPV6 address to EVERY ATOM ON THE SURFACE OF THE EARTH, and still have enough addresses left to do another 100+ earths.

Unfortunately, IPv6 is having trouble taking off, so we're stuck with IPv4 and a shortage of IP addresses. And this is where your router comes in. Its job is to get a public IP address from your ISP, and then create a local network inside your house. It does this by a few different technologies:

* The router creates a private IP address for itself. This is commonly 192.168.0.1, or 192.168.1.1, or something along those lines.
* It runs something called a DHCP server that lets other computers on the network ask for a new IP address and connection information. It will hand out private addresses like 192.168.1.27.
* And finally, the part we care about the most: your router does _Network Address Translation_, or NAT, to convert your packets from private to public addresses.

## What's in a NAT?

Let's break this down. Suppose you're trying to connect to a website like youtube.com. Your computer will look up (using a different system called DNS which I'm _not_ covering right now) that website's IP address. For example, I just got the address 172.217.18.14 when looking up www.youtube.com. I want to connect to it as a secure website (HTTPS), and the standard port number for that is 443. My computer knows its own _private_ IP address (let's say it's 192.168.1.27), and randomly chooses an unused port number (let's say 4001). Then my computer makes a packet that looks like:

* Destination IP: 172.217.18.14
* Destination port: 443
* Source IP: 192.168.1.27
* Source port: 4001
* Data: please start a TCP connection with me

Then, it sends that packet to my router so the router can pass that packet on to the rest of the internet. However, as it stands right now, that packet will be blocked, because private IP addresses are not allowed on the internet. And that's a good thing, because _lots_ of computers in the world have the IP address 192.168.1.27, and YouTube wouldn't know which comptuer to send it to.

Instead, the router translates the network address (e.g., NAT). The router has some public IP address it got from my ISP, let's say 82.20.63.76. It will then translate the header on the packet above to say:

* Source IP: 82.20.63.76
* Source port: 54542 (we'll come back to why this is different in a moment)

The router has to remember that it did this translation, and it sends off the packet to the internet. Eventually YouTube receives the request, processes it, and sends out a packet of its own that looks like this:

* Destination IP: 82.20.63.76
* Destination port: 54542
* Source IP: 172.217.18.14
* Source port: 443
* Data: OK, starting a TCP connection

The router receives this packet, notices the destination port, and remembers "hey, I remember that 192.168.1.27 should get these." It then replaces the destination IP and port with:

* Destination IP: 192.168.1.27
* Destination port: 4001

It then sends that data into the local network inside my house, where my computer receives it, and thinks it's talking directly to YouTube.

Side point: why did the router change from 4001 to 54542? Because other computers in my network may _also_ be using source port 4001, and the router needs to distinguish which computer should receive these packets.

This is a horribly ugly hacky workaround for not having enough IP addresses. But it (mostly) works just fine, and the entire internet is built on it right now.

If you want to see evidence of this happening, check your local computers settings and see what IP address it thinks it has. Then compare with the number you get from https://whatsmyip.com/. That website is seeing the IP address from the router, not from your local computer, and so you'll almost certainly get two different numbers.

## About that "mostly"

I said this all mostly works. Let's start with TCP. With TCP, there's a whole protocol of how to connect over TCP. The router understands this, looks at the headers and the data, and remembers the mapping between the original source IP/port and the new source port. Almost every router under the sun handles this situation really well.

Unfortunately, the situation isn't as good for UDP. That's because there's no real "connection." UDP is just a bunch of packets. Good routers handle UDP really well, keep track of the mappings, and intelligently decide when a source port has been unused for long enough that it's allowed to forget about it.

And that brings me to my video call problems. The router included with the modem from my ISP sucks. It would forget about these mappings at the wrong time. The result would be that, in the middle of a call, the UDP packets carrying the audio from the other side would suddenly get "stuck" on the router and not get sent to my computer. Eventually, the router would remember a new port mapping and the call would resume. But I'd lose 10-20 seconds of audio while that happened.

For various technical reasons that I'm no expert at and aren't really relevant, the video data in calls often goes over TCP instead of UDP, and that's why I would continue to see the video move while people went silent.

Similarly, the kids could play Minecraft for a while before packet loss ensued and they'd get sent to "limbo." Discord calls would work until they'd glitch for a bit. And finally, the final puzzle piece: Echo Arena detected the situation _much_ faster than anything else and simply refused to play at all.

## The solution

With the problem identified, the solution is simple: don't use the router in the modem I got from my ISP. I bought a new router, plugged it into the modem, and switched the modem into "bridge mode." This disables the router functionality in the modem. Now my shiny new modem got a public IP address and could send data directly to the internet. It's responsible for giving out IP addresses in my house and doing all the NAT work. And since it's a good router, it does this all correctly. With this device installed, video calls instantly became near-perfect, my kids stopped complaining about Minecraft, and I could play Echo Arena (which I still suck at, but hey, that's what I get for writing blog posts instead of practicing my video game skills).

In my case, I already had Wireless Access Points (WAPs) throughout the house, so I did _not_ need a wireless router. Instead, I bought an ER605 from TP-Link. I've been very happy with the EAP245 WAPs I got from TP-Link before, and this is part of the same business class of devices. However, if you don't have your own WAPs, it's probably a better idea to get a wireless router, which includes both router and WAP functionality.

Anyway, I hope that explanation is helpful to someone else. When discussing with Neil, he pointed out how sad it is that many people in the world are probably affected by this crappy-internet problem and have no way of diagnosing it themselves. (Hell, I'm a network engineer and it took about three years for me to figure it out!) Good luck to all!
