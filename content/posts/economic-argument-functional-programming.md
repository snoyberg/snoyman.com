This is my "write it as a blog post" version of my LambdaConf Global Edition 2020 talk.

* * *

Good morning, afternoon, and evening to everyone. I'm honored to get the privilege of being the first speaker for LambdaConf's new global edition. I hope everyone has been staying safe and healthy in these interesting times. I know I look forward each year to LambdaConf, and the amazing community that it brings together. I'm very happy that, despite the current global situation, we'll still be able to live out some of the experience.

With that: I promise I will not be bringing up COVID-19 any more, unless it's actually relevant to this talk. Onward!

This may seem a little self indulgent, but I want to start off this talk by talking about myself just a bit. My background and education is different enough from many people in the functional programming world that sometimes I will see things radically differently than others. So a little context of where I come from may help explain the prompt for this talk.

Like many people in our community, I learned programming from a book at a young age. I was doing some level of programming from the time I was 8 years old, and was working as a programmer on small jobs before entering university. I've always loved software development. However, it's not what I studied in university, and not what my professional credentials are actually focused in.

For various reasons that I won't get into now, I decided to pursue a career as an actuary. For those who don't know, actuaries focus on risk assessment. I personally worked in casualty insurance pricing, meaning I helped put together models for determining how to price automobile and homeowner's insurance. To skip to the end of the story: due to my programming background, I ended up working on the actuarial tooling team in my first and only actuarial job. We wrote most of our "code," and I mean that with the biggest scare quotes I can muster, in Excel, Access, Visual Basic for Applications, and believe it or not a bunch of Perl scripts. I never trusted these tools to stand up to time, and was constantly seeking something more reliable for writing analysis software. Ultimately, I discovered Haskell and functional programming, I fell in love with them, and decided to make a career shift.

But let's go back to the days before I'd found functional programming. My degree was in actuarial sciences. I don't think the degree was particularly well designed. I took a few applied mathematics courses. I took some really useful statistics and probability courses. But the degree I took included a huge number of economics courses. I say this wasn't well designed, because most of those courses had no bearing on my actuarial studies. I ended up taking six of the actuarial exams before dropping out and becoming a full time developer, and almost none of the information from economics courses appeared on those tests. And furthermore, very little of my day-to-day job involved economics knowledge.

At least, not directly. I think most people in this virtual room will agree that, as a software engineer, we're practiced at thinking differently than many other people. We learn ways to approach problems that involve debugging the issue, testing the limits of a theory, trying to isolate a problem down to its simplest form, and logically step through all pieces of a puzzle. It's a capability that certainly helps us write software, but has carry-over impact to everything else in life.

That's how I feel about economics. I may not regularly need to know the exact details of demand curves, or the difference between average and marginal supply. In fact, it's been so many years since I took those courses that I probably don't remember most of the details correctly. However, there's a way of approaching the world that I learned in studying economics which I continue to pursue today. I regularly use these methods when discussing topics with the rest of the engineering team at work. I'll have conversations with my children using economic concepts as a way of understanding whatever topic is on the table. In other words, I generally like to think of economics not as the science, but as a way of thinking.

I think that's enough background information on me to continue, so thank you for indulging me. Now that you know that I tend to look at a problem differently than others, it's time for a story. I was at another conference a few months ago, and there was a roundtable discussion among the speakers. I wasn't able to participate, but I was able to watch. One of the questions that was brought up was something to the effect of:

> Why is all software slow? Why is all software buggy? Isn't this exactly what functional programming solves? Why is our software today so much worse than software from 30-40 years ago?

My mind immediately went to an economics-based answer to this category of questions. I'll get back to that. Instead, since I wasn't participating in the discussion, I got to see the answers of the other speakers. The answers went in the direction of things like:

* Our software today does more than software used to
* We have the wrong abstractions
* People build too many layers of abstraction because they don't understand the lower layers
* Functional programming is relatively recent and hasn't fixed the industry yet

It was a good discussion. I don't disagree with any of the points here. I'm sure many of you in the audience are nodding your heads in agreement with these points, and adding a few more.

But let's take an economics-driven bent on the question. Why is software slow? Simple:

> The people making budget decisions in software companies have determined that the marginal revenue to the company is less than the marginal cost of speeding up the software.

Software developers will probably respond to a sentence like that with one (or both) of the following reactions:

* What do those words mean?
* That's stupid, you can't answer "why is software slow" without talking about software!

If I'm successful in this talk, by the end you'll all not only understand what I meant by my sentence, but you'll also agree that it is a pretty good root cause analysis of the problem. And hopefully that will help you better understand how to advocate for better software going forward.

## Marginal cost, marginal revenue

Let's say I'm really good at making birdhouses. People like my birdhouses, and they're willing to pay a decent amount of money for them. I can make one birdhouse a day. It costs me $10 in material to make a birdhouse, and I sell it for $75.

Now let's say I discover how to gold-plate my birdhouses. It still takes a day to make the birdhouse, but now the material costs $100, and I can sell the birdhouse for $150. Should I start making gold plated birdhouses?

One way to analyze this is to analyze the _profit_ from each venture. For a normal birdhouse, the profit is $75-$10=$65. But for a gold birdhouse, the profit is $150-$100=$50. So even though my _total revenue_ is higher for the gold birdhouse, my profit is lower. That's one form of analysis.

Another is looking at _marginal cost_ and _marginal revenue_. Marginal here means "at the margin," or "the delta from the last unit." The marginal cost of a gold birdhouse versus a normal one is $100-$10=$90. The marginal revenue is $150-$75=$75. Since my _marginal cost_ is higher than my _marginal revenue_, I have no economic incentive to make gold birdhouses. And since math actually works, you may notice that the profit analysis provides the same $15 difference as the marginal analysis.

"But wait a second," you might say. "This means that no one will ever make gold birdhouses in the world. I don't want to live in a world where financial analysis prevents beauty." I may question your love of gold birdhouses, but you're certainly entitled to think that way. There are three different cases I want to look at:

1. The person buying the birdhouse thinks gold birdhouses are far more valuable than normal birdhouses
2. The person making the birdhouses is more proud of a gold birdhouse
3. Neighbors who look at the gold birdhouses get a lot more joy from them than normal birdhouses

You may be wondering at this point "what does this possibly have to do with software development?" Trust me, it does.

### Underpriced goods

I said that the price of a gold birdhouse is $150. Where does that number come from? Presumably, there's some open marketplace where people can buy and sell birdhouses based on category. And the "gold plated" category is set to $150. Buy who decided that? This is what we call market forces. It's almost, but not quite, a circular argument: the price of a gold birdhouse is $150, because the market has decided that, and therefore I can sell it at that price.

Almost, but not quite. Let's say there's someone in the world who is willing to pay $500 for a gold birdhouse, because he just loves gold birdhouses so much. In economics terms, we like to express all values in terms of some standard like dollars, so we say the value this person gets from the gold birdhouse is $500. The fact that he's willing to pay $500 for it demonstrates this value.

Anyway, he comes onto allthingsbirdhouse.com, finds the category of gold birdhouses, and sees he can buy one for $150. For him, this represents $350 of value creation. His $150 is valued to him, by definition, at $150. The birdhouse is valued to him at $500. So in an economic sense, he becomes wealthier by $350 by buying that birdhouse.

Meanwhile, the guy making the birdhouse spent $100 on material, and sold it for $150, so he's made a profit of $50. Except this isn't the full story. The birdhouse maker could have been spending his time making normal birdhouses instead. If he did that, he would have made $65 in profit. So when we analyze what's happening with the gold birdhouses, we have to take into account his _opportunity cost_ of spending his time on something less valuable. When we look at it that way, making the gold plated birdhouse is actually costing him:

```
$150 revenue - $100 material costs - $65 opportunity costs = $-15
```

This situation represents a sad state of affairs for the buyer: he wants a gold plated birdhouse, and assuming everyone in the world has the same basic cost structures, he'll never get one.

This is where economic forces come into play. Or the "invisible hand of the market." Next time I go to buy my gold plated birdhouse on allthingsbirdhouse.com, I'll see a price of $150. But I won't see anyone providing them! This is called a shortage. I really, really want one. So I go and contact a birdhouse maker. He tells me that it's not worth it to him to make a birdhouse for $150. But at $200, he could be incentivized to do that. The buyer still makes $300 worth of "profit," the maker makes $100 in real profit, and $35 in marginal profit versus making normal birdhouses, and so he's willing to do it. Everyone wins, and the market price of gold birdhouses moves up.

Before I lose my audience completely, let me give you a hint of where this is heading. Think about a piece of software you use daily, and which has some kind of a limitations you'd love to see fixed. How much more would you be willing to pay to see that improvement happen?

### Pride of the creator

This kind of analysis of birdhouses probably feels not only contrived, but a bit stale. Birdhouses aren't just an item. They represent a piece of the creator's soul! When I build a birdhouse, I create yet another mark on the world. That joy and pride is something I can't put a price tag on.

Well, that's not true according to economics. In economics, we ultimately put a price tag on everything, including your pride, joy, and ethical standards. This is what probably makes people think economists are unfeeling, uncaring monsters. To that I'd respond: yeah, we kind of are.

Anyway, let's say the fact that one of my birdhouses exists in the world is worth something to me. How much is it worth? Well, let's do an experiment. Forget about allthingsbirdhouse.com. Let's say I've got one birdhouse, and some random guy comes in to buy it. He wants to put it in his front yard. What's the absolute minimum price I'd be willing to sell it to him for? After some thinking, I decide the bare minimum I'd take is $50.

Now the guy tells me he isn't going to put it in his front yard. He's going to blow it up. He just loves blowing up birdhouses. I no longer get that pride and joy of "my birdhouse exists in the world." Would I still be willing to sell it for $50? Probably not. Now I'd want a little bit more to take care of my hurt feelings. But how much more? Someone who says "I wouldn't sell it for any price" is probably lying. For a billion dollars, you'd sell it I bet.

Ultimately, this birdhouse maker decides that he'd be willing to sell a birdhouse for exploding purposes for $55. Under this analysis, our "pride of a birdhouse" value to the creator is $5.

Now let's do the same analysis for a gold birdhouse. Maybe our guy would be willing to sell a gold birdhouse for a front yard for $120, and a gold birdhouse for explosions for $145. He gets more joy and pride from seeing his gold birdhouses out there in the world. So we can say that his pride in gold birdhouses is $25, and in normal birdhouses is $5. That's something we didn't take into account in our initial analysis. Let's go back to that:

```
Normal birdhouse:

$75 revenue - $10 material + $5 pride = $70 profit

Gold birdhouse:

$150 revenue - $100 material + $25 pride = $75 profit
```

In other words, even though our creator will make less profit in a financial sense, he gets _non-monetary compensation_ in the way of pride from his gold birdhouses. You could also make it a bit more tangible and say he considers his gold birdhouses to be good advertisements, which will lead to future business. Whatever the case, in our new and more complete analysis, he's better off making gold birdhouses than normal birdhouses.

So let me ask you, the software development audience: how much of a paycut would you be willing to take to work with functional programming, or the tools you prefer using in general? I'm not advocating paycuts for functional programming, but I _am_ trying to get you to think about what the value of FP is to you, and why it's there.

### Externalities

Let's say I'm the guy buying the birdhouses. I look at the going market rates and see:

```
Normal birdhouse: $75
Gold birdhouse: $150
```

I look at this, and decided that the _marginal benefit_ to me of a gold birdhouse versus a normal birdhouse is only $25. Therefore, I buy the normal birdhouse, since it's the smarter economic move. All done.

But now, let's say I live in a nice suburban neighborhood where my 10 neighbors will all see the birdhouse on a daily basis. They all have identical tastes, and each of them says that the extra prettiness they'd get from a gold birdhouse would be worth $10 to them. So their marginal benefit of me getting a gold birdhouse is $10 each, or a total of $100. When you look at it that way, the gold birdhouse is clearly the better value: for $75 extra spent, the owner gets $25 of extra benefit, and the neighbors get an extra $100.

Unfortunately, that's not how the birdhouse-purchasing-decision gets made. The owner doesn't go around asking all his neighbors for their input. He's also not a nice enough guy to spend extra money to make them happier. What we say is that buying the gold birdhouse would have a _positive externality_ for the neighborhood of $100. But since the person making the purchasing decision won't benefit from this, he doesn't make that decision.

In this audience, many are probably all familiar with the concept of "the tragedy of the commons" with regards to open source software. If Alice is working on an open source project, and is giving $1000/month of benefit to 10 companies, it would make sense for them to pool their resources and pay her $10,000/month, right? Except why would they do that, when they can get the work for free? And ultimately, Alice isn't willing to work full time on this projects for less than $6,000/month. No single company is willing to spend that money. Even though the positive externality far outweighs that, the reality often is that Alice ends up dropping the project and getting a "real job."

## Perfect knowledge

## Risk mitigation
