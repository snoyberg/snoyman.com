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

### Summary up until here

Let's do a quick recap of what we've looked at up until now, and hammer home how it relates to programming. An important aspect in economics is defining all value in some standard, measurable unit. If you can do that correctly&mdash;and that's a very big if&mdash;you can make direct comparisons between seemingly unrelated things like "an ounce of gold" and "my pride in a job well done." That allows you to make more informed decisions.

A common way we measure this is by asking questions like "how much would you spend to solve that problem," or "how much would someone have to pay you to give up on some thing you like?"

Once we've assigned some value to everything, we can start comparing the options in front of us "at the margins," by comparing the relative benefits and costs in different alternatives, and finding the one where `max(delta benefit - delta cost)`. This accounts for not only the tangible things like "money someone pays me for my goods and services" or "raw material costs," but if done correctly also accounts for "extra advertising from displaying my work" and "I feel happy that I got to use the tools I like using."

There are _many_ big holes in this breakdown. One of them is the fact that measuring the value of everything is all but impossible to do correctly. We do the best we can, but we're going to go into this flaw in a more detail in a bit. The other big flaw we've mentioned is externalities. The person making the decisions only pays attention to benefits and costs that apply to him or her. This is going to be absolutely crucial to understanding how we make functional programming successful in industry, and how we generally try to improve software. Hold onto that thought though, because I want to build up a few other ideas.

## Perfect knowledge

I'm sure everyone's tired of birdhouses. Let's talk about going out to buy groceries instead. For those of you who don't remember: before COVID-19, it was common for people to leave their houses on a fairly regular basis to buy food from things called supermarkets, instead of hoping that delivery trucks would drop food out in front of their houses.

Anyway: let's say I want to buy bananas. I have two supermarkets equal distance from my house. I'm lazy, so I'm going to drive to whichever one I go to. I look up on the websites for each supermarket to see which one is selling bananas cheaper. And then I go and buy my bananas from that store. I don't worry about the cost of gas or the cost of my time, because both supermarkets will take the same amount of time to get to.

But now, let's say that supermarket A will take 10 minutes to drive to, and supermarket B will take 20 minutes. The bananas will cost $10 at supermarket A, and $5 at supermarket B. Now it's time to look up the going cost of gas. And figure out: how much is 10 minutes of your time worth? OK, that's not too terribly difficult, and you can come to a conclusion fairly quickly.

Now let's say you want to buy apples, bananas, and pears. Supermarket A is cheaper for apples and bananas, supermarket B is cheaper for pears. Now you decide to analyze three plans:

1. Buy everything at supermarket A
2. Buy everything at supermarket B
3. Go to both supermarkets A and B and buy the cheapest stuff at each place

Again, you _can_ do this analysis, but it's started to get a bit more involved. And here's the rub: the time you spend on performing the analysis is worth something too! If you value your time at $40/hour, for example, and you spend 15 minutes performing this analysis, the analysis itself cost you $10. How much are you going to optimize on your supermarket expenditure for that $10?

A lot of microeconomics is built around an assumption of perfect knowledge, that everyone has access to all information they would need to make an informed decision. That's a nice simplifying assumption, but it's obviously not true in practice. In the supermarket case, here's what I think most people in the world do. You have a general gut feeling of whether produce is cheaper at supermarket A or B. If you need produce, you go to the cheaper one. If you start getting annoyed about travel time, your brain automatically starts making adjustments to justify going to the more expensive but closer supermarket. If you notice that the prices are going up, you'll either consciously or subconsciously start updating your opinion of the two supermarkets.

Also, if I know I'm about to make a major shopping trip, such as stocking my house for the next 3 months of quarantine, I may decide to pull open a newspaper and more directly compare prices. For a truly massive trip, I may even do a full analysis with a spreadsheet and estimate where my bill is going to be smaller.

This is an intuitive and imprecise process. The gut reaction of not just engineers, but accountants, mathematicians, and many others would be: that's inefficient! You're wasting money! With my economics hat on, I see it very differently: this is a judicious trade-off between the expensive act of acquiring knowledge, and the unspecified potential gain of having that knowledge. It's liberating to know in advance: yes, I'm going to make imperfect decisions, but I know why I'm making them, and it's worth doing it in some cases.

This concept applies to my job as a software engineer on a daily basis. In our line of work, gathering knowledge is in my opinion about 70% of the battle. Implementing solutions, once you have a clear plan of action, is relatively speaking the easier and more straightforward part. There are many extremes we can take on the acquisition of knowledge, anywhere from "I'm going to keep using PHP for all my apps, even a desktop GTK app, because I know it works" all the way to "I need to investigate the differences between these two RPC libraries with a line-by-line code analysis to determine which one will fit our use case better."

One of the greatest skills you can achieve as an engineer is learning how deep into a topic you need to dive before you can make an informed decision. You'll see these kinds of discussions come up with management all the time: please provide an estimate, what are the risks, if we give you 3 more developers how much more quickly can you achieve the results, and so on. Understanding that acquisition of knowledge itself is a costly action, and talking about it as its own, separate work item is a great approach.

I gave a talk in 2018 called "Functional Programming for the Long Haul." I made the argument there that as developers, we often ignore some of the real costs in maintenance of software development. We can use acquisition of knowledge as another explanation for that. It's _incredibly_ costly to acquire the knowledge to answer the question "is it more expensive to maintain software in Java or C#?" I believe, in response to this, most developers lie to themselves and say productivity and benchmarks are far more important than they are. That's because these pieces of knowledge are much cheaper to acquire. And we don't want to admit to ourselves that we're making uninformed decisions.

Instead, I argue for being brutally honest about this. For example, I'd say that:

* The single most important metric for a programming language is how difficult it is to maintain software in it
* It's all but impossible to assess this metric yourself without actually writing a real life application
* Therefore, we have to take some guesses at this
* We're going to base our guesses on less reliable data sources, like:
    * Marketing material
    * Proof-by-authority claims from leaders
    * Popularity

And this is one of the areas where we are failing as functional programmers. We _could_ be writing marketing material. We should be. We have lots of success stories, but we don't talk about them. Instead, we talk about how elegant our code is, and explore more advanced solutions.

Try to put yourself in the shoes of a developer or a manager who isn't a Haskell or Scala aficionado. They have limited time to gather knowledge on "this FP thing." They pull up some articles on Go, and see what a success it is in the DevOps space. They look at Rust's subreddit and see a page full of "look at this weekend project I threw together." They come to the Haskell mailing list and read about effect systems, and dependently typed coding, or to make fun of myself: a bunch of criticisms of monad transformer stacks and async exception caveats.

We believe that functional programming solves real world problems better than the alternatives. But we're not effectively signaling that. And the highly rational breadth-first search most companies and developers will follow to find a language will probably not select FP based on this information.

## Risk mitigation

We're almost through the economics lesson, don't worry. Three topics left, and they're all much easier than the others.

Let's play a gambling game. I'm going to flip a coin. If it's heads, you get $2. If it's tails, you get nothing. How much would you be willing to pay to play the game? Most people would probably be willing to spend $1 for a 50% chance of winning $2. You may not be someone who enjoys gambling, and so you may not want to play. But the basic idea probably makes sense.

Now let's pretend that your entire life savings is in your pocket, and it's $10,000. I offer you the same basic game: if I flip heads, you'll get $20,000. If I flip tails, you'll get nothing. Would you spend all $10,000 of your money to play the game? Probably not. There are some people who would still play this game. But most people are _risk averse_. Even though on a purely monetary level this game is fair, the value of these outcomes is not equivalent. Doubling your net worth is really great, but wiping out all of your savings is completely devastating.

This situation with the coin is pretty simple. Assuming you trust me to be fair, you know we're talking about a 50/50 shot. But with most things in the world, we don't know the true probabilities of the different outcomes. Usually, to get a good feel for the likelihood of an outcome in any kind of complicated situation, you need to invest significant time in acquiring that knowledge.

Companies tend to be fairly risk averse, especially larger and established companies. If I come in and say "Hey, if you invest 5 months of dev time, we can rewrite your entire codebase from PHP to Haskell. And once rewritten, I'll reduce your hardware costs by 37%, your maintenance cost by 33%, and improve user satisfaction by 63%." As a company, we can weigh the investment of 5 months of dev against those benefits and come up with a decision. Except for one really big question: risk.

* Will the rewrite really take 5 months, or will it take longer?
* Will the new version of the codebase actually work, or will we be in firefighting mode?
* Will the claims of hardware cost reduction actually play out?
* Is Haskell actually more maintainable than PHP? This Michael guy seems really excited about it, but why should we believe him?
* I get it, the new code base will be more reliable, faster response time, and we can iterate on new features. But will user satisfaction actually rise 63%?

Investigating the veracity of each of these claims requires more knowledge acquisition, which as we know is expensive. Even then, we'll only get some basic idea of the risk. Maybe I've successfully migrated 8 companies from PHP to Haskell in the past, and I did it on time and within budget in 5 of those cases. In the other 3, the project went over on time by 25%, and over on budget by 40%. How does that affect my risk analysis?

Put yourself in the shoes of a manager who knows nothing about PHP and Haskell. Would you be willing to make these kinds of speculative investments with so much uncertainty in the air? I wouldn't.

## Fixed and sunk costs

Many people have probably heard the phrase "sunk cost fallacy." But less commonly discussed are the things which proceed it: fixed and sunk costs. Instead of some analogy, let's jump straight into the software on this one.

Let's say it takes three years to learn to be a software developer. Let's say I value that time at $10,000 a year. So the cost of learning to be a software developer is $30,000. Now as a freshly minted software developer, I spend six months writing my first project for a consulting gig, where I get paid $20,000. A naive view of this would be that my costs were $35,000&mdash;3 years learning at $10,000 each, plus half a year of work for $5,000. And the revenue I made is $20,000. So I lost $15,000 on the venture.

Now I get my second gig, also for $20,000 for 6 months. I still value my time at $5,000 for those 6 months, so now I've made a profit of $15,000.

But is it really accurate to say I lost money on the first gig? Not really. The terms in economics for what are going on here are fixed versus variable costs. Learning to be a software developer is a fixed cost which in reality gets spread across multiple projects, while the variables costs are incurred for each and every project. When I go to learn software development, I have to ask the question: how many years do I expect to work in this field? Based on that, I figure out whether the 3 year investment in learning programming is worth it.

Now let's kick it up a notch. I've learnt how to program in language X. Just as I finish learning language X, an announcement is made that language X is dead, no new software will be written in it. I'm able to get 1 single gig in this language. Should I take that gig or not? From a purely financial standpoint, it's worth taking the project. I've _already_ spent the time on the fixed cost of learning language X. Since I've already made that investment, we no longer call it a fixed cost. Now it's a _sunk cost_. The cost is sunk, and we no longer take it into account when making further decisions. By contrast, if someone could have told me 3 years before that language X was about to die, I would have learned something else instead.

So how does this "sunk cost fallacy" come into play? Let's say I've spent those 3 years learning language Y. That cost is sunk, and I should never, ever pay attention to it again for decision making purposes. However, we humans tend to let our emotions get in the way of logical analysis. Now let's say that language Z exists as well. And let's say I know somehow, with absolute certainty, it will only take me 6 months to learn language Z, and 6 months to implement a project in language Z. I also know for a fact that for some reason, it will take 18 months to implement the same project in language Y.

The sunk cost fallacy would be to focus on the 3 years of investment you made in learning language Y, and say you can't just throw all of that away. Ignoring the emotional toll that may take, the correct economic decision _is_ to "throw away" that investment. Looking at the marginal costs, it is cheaper to learn and use language Z than it is to use the already-learned language Y.

But "sunk cost fallacy" applies in a much wider way than human emotions. In fact, it can be an important defense mechanism in a corporate environment. No one wants to take the blame for making a bad technical decision 2 years ago, and may defend&mdash;seemingly irrationally&mdash;what you see as an obviously bad plan. "Don't throw good money after bad" is a great argument. But there can be other great counterarguments, such as "the devil you know." We _know_ language Y, we know how long it will take to use it, and language Z is a huge risk factor. Honestly, how do you know for certain that it will only take 12 months to learn and use language Z?

The point of this is that there is a logical way of approaching these decisions involving fixed costs. But there will be both confounding factors, like emotional involvement or preexisting biases, that will make it difficult to approach this all rationally. And the interaction with other legitimate concerns, like perfect knowledge and risk, makes this even more complicated.

## Needs versus wants

I think this was actually the first lesson I ever received in an economics class. I almost forgot to include it in this talk, because it's so fundamental to how I think about things. There is no such thing as a "need" in economics. There are wants, or desires, or more appropriately, different valuations of things.

"But wait a second," you say. "I _need_ oxygen!" Well, no. Oxygen is necessary in order to live, and you've decided you want to live. It sounds morbid and uncaring, but it's a way of exploring things. And it's vitally important. As soon as you begin saying "this is an absolute requirement and there is no way we can ever budge on it," we start to get into uncharted economics territory.

Let's take a simple software example. Most of us in this room would probably say "a network service needs to have high uptime, at least 99% or 99.9%." By defining it as a need instead of a want, we're not able to talk rationally about trade-offs. Suddenly, any cost necessary to achieve this goal is worthwhile. What if a customer came along and said, "Hey, look, I want to see a proof of concept of your app. I don't care if it crashes every 20 seconds, I just want to see it running. If I see that, I'm willing to sign a $5 million contract for the software." In the "need" analysis, this isn't an option. In a "want" analysis, we can value high uptime of an app at a certain dollar figure. And then in our new analysis for this specific customer, we can reevaluate the value of high uptime.

This disconnect between want and need is one of the deepest divides that occurs between engineering teams and management. We'll see why after a recap.

## Final recap

I appreciate all of you humoring me and letting me live out my fantasy of being a stuffy economics professor. Let's do one final recap of the key points before bringing it home.

* Compare cost and benefit at the margin versus other alternatives
* Quantify both tangible and non-tangible items when performing an analysis
* The person making the decision may not be paying attention to externalities that affect others
* Acquisition of knowledge to make perfect decisions is expensive
    * Therefore: it's logical to make imperfect decisions from imperfect knowledge
* When dealing with risks, risk averse people will often evaluate the downside to be far worse than the upside
* Once you've paid for a fixed cost, it's a sunk cost, and you logically should never pay attention to it for analysis purposes again
    * However, there are reasons why people may end up still taking them into account
* There are no economic needs, there are only wants, which should be valued objectively and quantitatively

## What should we do?

Now that we've covered all of these topics, it's time to bring it home. Can we use this knowledge to help us better approach improving software development?

Let's go back to the beginning. Why is software slow? I said before:

> The people making budget decisions in software companies have determined that the marginal revenue to the company is less than the marginal cost of speeding up the software.

If I'm the CEO of a company, and I'm making budget decisions, I ultimately care about just two things: how much money will it make, and how much money will it cost. When you step down the ladder a few rungs, you'll get to a director of engineer who is really annoyed that his engineers are wasting time waiting for their software to respond. You'll get to a customer relationship manager who is hearing constant complaints that the software sucks.

But often, the beauty of being CEO is you don't hear about this or care. If someone comes to you and says "I'm so annoyed that the software is slow," the CEO can legitimately ask "so what?" If you want to motivate a company to fix slow software, you need to quantify why it matters. Here are some examples:

* The QA team is slowed down by slow software. If we speed up the software, they can work 20% more efficiently.
    * The CEO is able to convert "20% more efficiently" into a dollar value by knowing how much he spends on QA.
* I spoke with the sales team, and they estimate that 7% of potential customers chose not to use our product because it was too slow.
* Our engineers are frustrated by working on this product because we never speed it up. I'm worried about losing good people.
    * This is usually as far as such a conversation goes. Take it to the next step:
    * Replacing an engineer and training them up is typically a 3-4 month process. Not only will it delay our deliverables, but it will cost us significantly on HR and engineering costs.

With this kind of language, you've established half the story with the CEO: what benefit do I get from speeding up the software? The CEO can look at the three points above, figure out the staffing costs that will be saved, plus the potential increase in revenue. Then comes time for the next question: how much is it going to cost?

Another typical engineering mistake is to throw the kitchen sink in at this point. "We should totally refactor our entire codebase to use latest best practices." This kind of approach throws warning bells up:

* It sounds like an engineer wishlist instead of something inherently valuable to the company. Keep in mind misaligned interests: engineers tend to get joy and satisfaction from engineering perfection, and non-engineers know to be careful of allowing this to happen.
* It's potentially a huge undertaking.
* The risk to the product is massive: such a refactoring almost always results in new bugs and potentially downtime.

Instead, make a priority list of different potential improvements. Start with the highest impact ones. For example: we're currently running on smaller machines. If we bump to bigger machines, the increased hardware cost will be X, and it will take Y days of engineering to make it happen. This has a potential to improve latency on requests by Z percent.

The first few things on the list may not be glamorous. You may think to yourself: I have the CEO's attention, I should try to get him to approve the code fixes that I know are absolutely vital. Ignore that temptation! Be honest, and give the most accurate engineering answer you can. My primary reason for saying this is business ethics: your company is relying on you to be a technical expert, and you can't take advantage of that. But since this is an economics talk, and I'm trying to put things in more tangible terms, here's another reason: you don't want to blow your credibility. If you establish early wins, and demonstrate that you give an accurate and business-friendly analysis, you're more likely to be listened to in the future.

## The Apollo missions

A common cry in our industry is that we've become so much worse than our predecessors. NASA was able to write nearly-flawless software that sent people to the moon. They did this with hardware far simpler than anything we have today. And now we can't even make a web app without it crashing every other week.

We like to lament bad practices in our industry. Or that software is too complex. Or many other things. They all may be true. But they are _symptoms_. Let's do the same kind of basic analysis I just applied to slow software. Companies are typically buying software these days based on feature sets. Or nice UI/UX. There is usually some quality bar we need to pass to make the software tolerable. But typically, that's as far as things go in analyzing software.

So when you go back to your CEO and say "our app is unstable," the completely fair follow-up question is once again "so what?" And here's the sad part: there may not be market forces available today to force higher quality into most software. The Apollo missions had such famously high software quality because the cost of a bug was _massive_: lots of money and even human lives wasted because of a bug. Therefore, the cost of writing software correctly was warranted by the risks inherent to the project.

How do we change this? It's actually pretty hard. It would require finding market forces that reward companies that produce more reliable software. This may come down to figuring out a better marketing message. Can you go to market with a less-featured piece of software with the claim "yeah, but it's less buggy!" Will people use it? Can you convince common review sites, or corporate magazines, or whatever purchasers tend to read, to start making software bugginess a key ingredient in their analysis?

One possible approach is instituting legal responsibility for bugs. If a company can be sued due to losses incurred by a bug in software it purchased, it may put pressure on providers of software to increase quality.

Personally, I think the world would be a better place if we could make that happen. But unlike features or flashy UI, it's much harder to analyze software reliability in a 1 hour session.

Important quote:

> u gotta 10x harder otherwise some young code wyzyrd is gonna eat your lunch by deploying a modern serverless webapp that can auto$cale to the moon in a lambo - Dr. Joseph Kachberg

## Functional programming

I'm not sure if you've all noticed, but I've done something pretty impressive. I've spoken for however long I've spoken so far without really getting into any concrete benefits of functional programming. There's a reason for that: the concrete benefits aren't important to the higher level economic analysis. A CEO isn't going to care too much about _why_ functional programming saves him or her money. They'll care much more about how much money you're saving them in costs, or making them in revenue. And how much it's going to cost them to get those benefits. And how much risk there is inherent to your plans.

But as the engineer who wants functional programming to take off in a company, you need to have a solid understanding of the concrete benefits, and a way to quantify those benefits, justify those benefits, estimate risks, and most importantly: put together a reasonable plan of action.

Let's start off with a purely technical review of some of the purported benefits of functional programming:

* More declarative code
* Immutable data by default
* Explicit effects
* Higher order functions
* Purity
* Combinators
* Mathematical abstractions
* Static type checking
* More fun to write

Can we turn these into quantifiable items?

* More declarative
    * Less time spent in code review
    * Easier to maintain code
* Immutable data by default
    * Reduce risk of race conditions
    * Reduce risk of business logic errors
* Explicit effects/Higher order functions/Purity
    * We _think_ it makes code easier to maintain
    * Really hard to make those kinds of arguments directly
* Combinators/Mathematical abstractions
    * Reusable concepts
    * Less time spent learning new concepts for each library
    * More consistent interface == reduced bug count
* Static type checking
    * Remove classes of bugs entirely (don't claim no bugs!)
    * Reduce overall testing burden
    * QA can focus on higher impact bugs
    * More maintainable codebases (would really love concrete data to back up that claim!)
* More fun to write
    * Probably don't lead with that one
    * But can point out: better staff retention, easier recruiting
    * Recruiting _is_ a concern often, more on that below

## Don't hide the downsides

I've been guilty of this in the past. I see it happening all the time. Sometimes, we fall so much in love with FP, or specific tooling, or anything else, that we become unrelenting advocates for it. This actually undermines your case. Putting it in economics terms: whoever you're pitching FP to doesn't have perfect knowledge. And more importantly, they _know_ they don't have perfect knowledge. When you describe all of the benefits with none of the costs, they know you're misrepresenting things.

You can establish credibility by acknowledging both the pluses and the minuses of whatever you're advocating. This can actually be used as a persuasion technique; if you're curious, check out the book "Presuasion." I am _not_ advocating that you try to manipulate people listening to you by giving them a stacked set of downsides. This isn't an interview where you get asked "what's your greatest weakness" and you say "well, sometimes I just work too hard and produce code that's too perfect." Again, like before, there's an ethical issue at hand. But more practically: you _will_ get caught eventually.

Some examples. I don't think Haskell is a good alternative to Kotlin for an Android app. I don't think Haskell is the best choice for frontend web development. If there's a project that requires hard realtime guarantees, I'm not going to recommend any garbage collected language, I'm going to reach for Rust.

The hiring pool for FP languages is not as broad as for languages like Python or Java. I think this can be mitigated by pointing out:

* The people who are looking for FP jobs are typically highly motivated people
* If you're willing to hire remotely&mdash;which in a post-COVID world I'm hoping is more likely&mdash;the hiring pool is plenty large
* Training people in FP languages is not as big an undertaking as it used to be
    * There is lots of good material out there now
    * FP concepts have permeated industry pretty well

I'm not here to beat up on FP, far from it. I truly believe it's the best approach for writing the vast majority of software today. But keep the downsides in mind. And importantly: save your ammo. By that, I mean:

## Maximize benefit, minimize cost

When you're ready to make a pitch for FP, make it on the project most likely to succeed. Choose a case where the benefits are highest, and the costs the lowest. That mythical "rewrite it all in Haskell" isn't the first thing to recommend. Instead, look at smaller side-projects first. Take something which would be particularly difficult to do in the language your company typically uses. Do something that plays to the benefits of the FP language you're advocating.

One of my big recommendations for Haskell is to implement a piece of complicated business logic as a separate microservice. I'm _not_ a huge fan of microservices in general. But providing a new Haskell service over a network API meets a number of our goals:

* The benefits are relatively high
    * You've established some complex business logic needs to be implemented
    * Haskell's purity makes it easier to test such logic
    * Haskell excels specifically in the domain of concurrent network services
* The costs are low
    * This is a well trodden path with lots of success points in industry
    * The worst case scenario is that the project has to be scrapped, which is far less bad than "breaks an existing mission-critical service"

And based on this, the risk factor is also lower than in many other cases.

If you do get the go-ahead to move forward, try to set up some measurable objectives. It's too easy for a successful project to be written off. I remember one case we had at FP Complete where we helped a company do a pilot project in Haskell, to test the waters for moving from C# to Haskell. At the end, they had their two top C# experts review the results to determine if it was something they should consider. The ultimate answer was "nah, we like C#." We had produced results in less time, less lines of code, with less bugs. But the company had set a completely subjective measure of "do our C# developers like it?" It undermined the entire process.

One word of warning here. Don't be an overly pedantic engineer. If asked to compare a Haskell and Java solution, you don't need to get hung up on the details of the differences between the two codebases. I give you permission to compare apples and oranges, and give a basic high-level comparison of bugs, code size, and so on. I can basically guarantee that's what management is interested in.

## Hidden benefits, hidden costs

Management is rarely intentionally blind to things that can increase revenue and decrease costs. They are usually unaware of the details. Your job as an engineer is to discover and communicate clearly about these details. I'm trying in this talk to point out what kinds of data points matter in an economic analysis. I hope it's resonating and sinking in. Your next step is to do your own analysis.

And then, and this is really important: you need to communicate clearly about it. The founder of FP Complete, and my boss for 7 years, Aaron Contorer, taught me a lot. One of these items is that you can have a feature that unlocks the value of all the other features. Clear communications is one of these things. You can perform all the analysis in the world, and have an absolutely perfect recommendation which will absolutely be golden for your company.

However, if you obscure that recommendation behind poor communication, you haven't done anything at all. As engineers, we often believe the truth speaks for itself. It doesn't. The truth needs help. It needs well structured words. It needs a well structured email. It needs a pretty graph. Sometimes, it needs a Powerpoint. You're not betraying some truth by dressing it up nicely. Be honest, be accurate, be clear, and be persuasive.

I've given you some examples of the hidden benefits and hidden costs throughout this talk. I'll give you another short list here. Think about how these apply at your company. See if you can make the argument that these metrics will be positively impacted by employing functional programming.

* Request handling latency
* Engineer morale
* Bug rate
* Time to market
* Hardware costs
* Maintainability

And if you can't make the argument for these, here's the unfortunate truth: maybe a move to FP _doesn't_ make economic sense at that time. Be ready to accept that. Don't place your own vested interests over what's actually best for your team and your company. Again, there's the ethical issue, but there's also the fact that such a weak argument will be transparent, and you'll lose credibility. Again, save your ammo for where it counts.

## Proof by authority

Do you agree with this statement?

> Java is obviously a great language, because so much software is written in it

Probably not. Someone attending LambdaConf probably doesn't pay much attention to prevailing wisdom in software development. We know that inferior languages sometimes win out for non-technical reasons. However, let me argue back just a bit: a lot of software truly is successfully written in Java. It may not be the best language, but at the very least we know it's capable of working.

The way the FP world often talks publicly about our languages, we can't really make the same claim. I know many, many companies and individuals successfully using Haskell at large scale. But unfortunately, I have little in the way of public information to point to. We are not establishing basic proof points. I would love to see a world where FP companies were regularly writing articles, both on their own blogs and in popular publications, about their successful deployment of FP languages.

When you go to your CEO, who's never heard of Haskell, and say that you want to invest six months of engineer time into some Haskell project, it's completely valid for him or her to ask for existing success stories. Talking about all the technical merits of Haskell doesn't cut it. Most companies don't want to be the first company to ever deploy a web service in Haskell. And the way we discuss things with them, that's what they think you're proposing.

Fintec is a good proof point for success of Haskell in industry. Also, I know that many companies paid attention when Facebook started talking publicly about their usage of Haskell. As engineers, we may care more about the technical merits. But companies have a right to be skeptical.

Point to other similar companies that have done similar things with FP. Or, by contrast, point to companies that have been hurt by other languages. To borrow an example from a related field: DevOps saw lots more interest when big companies started having major and public intrusions.

My request: everyone in this virtual room should start talking more, in public, about the successes they've had with functional programming.

## Conclusion

I hope this talk provided a fresh look for you at how decisions are made in our industry. Perhaps you have a better understanding of why your company is behaving a certain way right now. And hopefully you have a plan of how to move forward with technical recommendations.

If there's one final word I can leave you with in all of this, it's __empathy__. When discussing these topics with your friends, coworkers, boss, or customers, think about their point of view. Try to see how they will be analyzing the topics. Try to present them with information that will both be understandable to them, and answer the concerns they have. Spend the time structuring your communication in a way that respects them, and presents them with information to make a valid economic analysis.

Thank you to the virtual audience, and thank you to LambdaConf for restructuring the entire conference structure so we can all get together virtually like this. I'm looking forward to interacting with everyone over the course of the year!

And at this point, I'd be happy to take some questions.
