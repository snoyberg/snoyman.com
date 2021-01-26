+++
title = "Moving to Fastmail: custom domain, family accounts, and more"
description = "I recently, and finally, found an email and cloud accounts setup that I'm happy with for my own email management, privacy, and family setup."
date = 2021-01-26
[extra]
ogimage = "moving-to-fastmail.png"
+++
This blog post is much more about the journey, and what makes it so difficult to balance different needs in a good setup for cloud and email accounts. To forestall any complaints about "just give me the information," here's the ultimate setup I've landed on:

* Email: Fastmail, with aliases forwarding my family's email to their respective Gmail accounts
* Neither Google nor Microsoft having special handling for my domain name (`snoyman.com`)
* Cloudflare for domain name registration (via transfers) and domain name serving
* Netlify for hosting static sites, though I'll look into Cloudflare Pages soon hopefully
* Code for sites on Github
* Dynamic sites still hosted on an [FP Complete Kube360 cluster](https://www.fpcomplete.com/products/kube360)

Alright, does that jumble of words mean anything to you? Probably not much. The real questions are what I had tried previously, what problems I encountered, and how these choices overcame them. Let's hit it!

## Michael, unmarried, sets up email

Many years ago, I purchased `snoyman.com` so that I could host my own email and have the nifty looking email address `michael@snoyman.com`. I was probably around 20 at the time, and this seemed like the coolest thing in the world to me at the time. (Or so I think, I really can't remember that long ago well.) In any event, the first question is: where should I set up my mail hosting? I'm not sure if I tried other providers first, but at some point Google offered Google Apps for free (later renamed G Suite, later renamed Google Workspace), and I signed up and began using it for hosting my own email.

The first problem with this was that `michael@snoyman.com` became a _quasi Google account_. What I mean by that is that, on all Google systems, `michael@snoyman.com` was a valid username/account ID/whatever you want to call it. But not all of them supported Google Apps. If I remember correctly, in the glory days of Google Reader, I wasn't able to access it from Google Apps. And this is the first lesson in this saga:

**The cloud providers offer neutered accounts on their business-geared email hosting.** We'll see this issue get worse with kids on Google, and completely repeat with Microsoft.

So I ended up creating a separate Gmail account `@gmail.com`, and setting up forwarding from one email address to the other. That meant I got full access to Google services, had a single inbox to check, and kept my snazzy `michael@snoyman.com` account. Downside: I often had the annoying question of "which Google account did I log into this service with?"

## Forwarding for the family

Others in the family decided they liked these `@snoyman.com` email addresses, and wanted them too. It was easy enough to set up some forwarding for various family members through Google Apps to their Gmail accounts. I don't know if they ran into similar issues with the multiple-sorta-Google-accounts issue.

## The kids need accounts

At some point, many years later, my parents bought my kids Android tablets. This necessitated setting up Google accounts for them. And this is where things broke down completely. Assuming I had a daughter Alice, the obvious email address would be `alice@snoyman.com`. Which would most directly mean creating a user account on the Google Apps instance (now called G Suite).

However, G Suite accounts didn't have full support for the family sharing and child protection limits we wanted to place. So again, we ran into the situation of needing both regular Gmail accounts and Google Apps accounts. So instead, I decided to go towards the forwarding setup I'd used for other family members:

* Each child gets a Gmail account, joined to our family's Google Family account
* Children log into their tablets using those accounts, allowing us to control screen limits and other issues
* I set up forwarding from G Suite from their `alice@snoyman.com` address to `alice.snoyman@gmail.com`

And finally, within Gmail, I set up aliases for each child, so that `alice@snoyman.com` can be used as a login for their Gmail account. This means that if anyone tries to invite them to a document, for example, it goes to the correct Gmail account, which is nice.

## Dropping G Suite

At this point, the only person who had email actually hosted on G Suite was me. I was the only one left with two of these Google-ish accounts (a real `@gmail.com` address and the G Suite `michael@snoyman.com`). Getting rid of that was pretty easy: have G Suite forward emails from `michael@snoyman.com` to my `@gmail.com` account, and set up an alias in my Gmail account.

This left two problems, one minor, one slightly-less-minor. The first is that I had this entire G Suite setup for no reason other than forwarding emails around, which is silly. That's minor. The other problem is about email security. When you have forwarding set up like this, and when you send email from a different domain through Gmail, the security protocols of email don't perfectly reconcile things. I'm probably getting some of the details wrong, but:

* Email sent to `michael@snoyman.com` would arrive in my Gmail inbox with a DKIM failure, since the domain names (snoyman.com vs gmail.com) didn't match
* Email sent from Gmail through my `michael@snoyman.com` address would say "via gmail.com" on the receiving end
* If I used a desktop mail client instead of the web interface, there wasn't a way to have the email come from `michael@snoyman.com` at all

If you're not familiar with the email security systems of SPF, DKIM, and DMARC, consider yourself lucky. But suffice it to say that I realized I was pushing the limit of what "correct" email usage was.

Was this a big deal? Arguably not. Arguably I should have just stopped here. And for a while I did. But it always irked me.

## Microsoft to the rescue? Nah

The real issue above was that I was trying to use Gmail to handle a different domain name, which it's definitely not intended to do. And on the other hand, using G Suite for that domain name&mdash;which _is_ intended to work&mdash;doesn't work well for accessing the full suite of Google services. So I got a great idea: what if I moved `snoyman.com` from G Suite over to Microsoft's online offering? I had made that move for work, where we'd recently embraced Microsoft 365, and the migration was fairly painless.

Unfortunately, the _exact same issue_ immediately popped up. On the Microsoft side, there are two kinds of accounts: A "Microsoft Account" and a "Microsoft Work or School Account." Both Google and Microsoft could really do a better job of signposting the differences between their account types and giving them useful names. But the exact same restrictions came into play. If I put `snoyman.com` as a domain in Microsoft 365 (and/or Azure AD, for anyone keeping score at home), I would have conflicts with normal Microsoft Accounts for myself and the kids. And as the kids were right then getting into online classes due to COVID, using Windows 10 laptops with Microsoft account logins, and playing a lot of [Minecraft](@/blog/parents-guide-minecraft.md), I didn't want to go down this path.

Instead of all that, on the Microsoft side, I created each kid their own Microsoft account with their `@snoyman.com` email address, and never had Microsoft manage our emails at all.

## Dropping G Suite, hello Google Domains

At some point, I noticed that Google Domains had support for some useful features:

* Bare name redirects (e.g. redirecting from `http://snoyman.com` to `https://www.snoyman.com`)
* Email forwarding

I moved a few of my domains over to Google as the registrar, configured some email forwarding and various website redirect rules, and I was finally free of my G Suite account. And just in time to avoid Google renaming _yet again_ to Google Workspace. Hurrah!

I still had the email security issues I mentioned above. I solved one of them by setting up outgoing email to be sent via Mailgun, which allowed proper handling of SPF and DKIM. But this was yet another component I needed to maintain, and still didn't feel satisfactory.

## Privacy

I realized at some point in all of this that I was obsessing over fairly unimportant details around email. No one really cares if my emails said snoyman.com or gmail.com in the headers. I wasn't checking DKIM signatures on incoming messages. Why was I still bothered by all of this?

And the reality was that I simply wanted to get off of Google for my email. Like many others, I'm bothered by Google's (lack of) privacy practices, and them knowing too much about me. I didn't really like them controlling my domain names either.

So over the weekend, I decided to finally bite the bullet and look into a solution one of my coworkers had told me about.

## Moving to Fastmail

I'd investigated Fastmail earlier in this saga, but was concerned I wouldn't be able to handle the large number of email forwards needed for all my family members. Maybe when I initially investigated, that was in fact correct. However, when I investigated this weekend, I found out that Fastmail does, in fact, support "600+ alias addresses." I was out of excuses.

I purchased a standard account for myself at $50/year, set up aliases for all of my family members, and switched over my DNS records. I imported my email and contacts from Gmail into Fastmail. I configured my local Outlook desktop client (which I was already using for work) to talk to Fastmail. I installed the Fastmail app on my phone. And just like that, I had everything I wanted! Plus a mail storage limit of 30GB instead of 15GB. And honestly, I was starting to edge towards that 15GB limit.

Since the rest of my family is happily using Gmail and doesn't intend to switch, no additional changes were necessary. But it's very nice to know that, any time they want, I can sign them up for a Fastmail account on our domain and switch over fairly seamlessly.

## Moving to Cloudflare

Now that I didn't have any email forwarding going on with Google Domains, the only feature I was using from them was the redirect rules. But I had enough experience to know that Cloudflare offers the same, plus much more functionality (such as [Cloudflare Workers](https://www.fpcomplete.com/blog/serverless-rust-wasm-cloudflare/), and the current-in-beta Cloudflare Pages). I transferred my domains from Google Domains to Cloudflare for name serving and added appropriate page rules for the redirects. Then I transferred registration over to Cloudflare as well.

I didn't do the second step for a cost savings, but I was pleasantly surprised to see the Cloudflare charged $8.50/year for most of my domain names, versus Google's $12/year. Since I have 9 domain names, this worked out to $31.50 in savings. Meaning the opportunity cost of switching over to Fastmail for email essentially dropped to $18.50/year, making the exchange even nicer.

## Conclusion

That was quite a few steps, but I hope this better motivates why I ended up at these decision points. To summarize what I ended up with:

* Better privacy guarantees, by using a paid service offering from Fastmail instead of Google's free email service. Remember, "if you are not paying for the product, you are the product."
* No servers to maintain. Everything I'm doing is using standard SaaS offerings. Yes, I could run my own servers, but I do that plenty at work. I'd rather let email, static file hosting, name servers, and redirects be Somebody Else's Problem&trade;
* My family members get to keep their nice looking email addresses, use whatever email service they want today, and retain the flexibility to move elsewhere in the future if needed
* There is no conflict between email hosting and Google and Microsoft's ideas of accounts, including for family management
* Nothing here breaks the bank; ultimately, I'm spending about $20/year more than I was previously

My recommendation to others out there is: if you care about maintaining your online identity, and keeping your privacy, it's worth owning your own domain name. For between $9-$12/year you can get your own domain name, and for $50/year get a Fastmail account. You can host a personal homepage for free using a service like Netlify. For anyone somewhat tech inclined, I think it's worth doing it.
