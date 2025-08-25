## Actuarial Thinking

* By Michael Snoyman
* Written August 2025
* Semi-sequel to [Economic Argument for Functional Programming](https://www.snoyman.com/reveal/economic-argument-functional-programming/)
* Avoiding complex math, focusing on intuition

---

## What is an actuary?

* Risk assessment
* Heavy on probability/statistics, also some econ
* Insurance
    * Calculate price of insurance (how likely are you to crash your car/die?)
    * Calculate reserves for insurance (how much money do we need to hold onto for potential losses?)

---

## What is actuarial thinking?

* There are no facts!
* We have estimates of likelihoods
* Estimates are known to be wrong
* We make the best decisions we can with known incomplete data
* Also somewhat morbid, you'll all understand my sense of humor a bit better soon

---

## People misunderstand risks

* Risks are _possibilities_
    * "Risk of dying some day" is nonsensical, we'll all die!
* Risks may not happen
    * I have a fire extinguisher _just in case_ there's a fire
    * I didn't waste my money on the extinguisher if there's no fire
* In aggregate, we can treat some risks essentially as guarantees
    * If I run 100,000 servers, one of them will have a hardware failure this year
* Unlikely things _will_ happen
    * Coin flip: How likely are 10 heads in a row?
    * How likely is a streak of 10 heads in 1 million tosses?
* I wasted my money on home owner's insurance because nothing bad happened to my house
    * Do you feel the same way about life insurance?

---

## Coin flip

* I flip a coin 100 times
* How many times should I expect to get heads?
* Thought experiment: what do you think of these scenarios?
    * I got heads 50 times
    * I got heads 51 times
    * I got heads 60 times
    * I got heads 95 times
    * I got heads 100 times

---

## Probability distribution

* Coin flips form a _binomial distribution_
* With large numbers, approaches the _normal distribution_
* We can ask questions like "how likely is it to get X heads?"
* Getting 100 heads == `(0.5)^100` == almost impossible
* So what do we do in that scenario?

---

## Updating priors

* Did I tell you that the coin was a fair coin? Nope!
* Baseline assumption: the coin is fair.
* If you get 100 heads, you need to update your priors
* Priors == assumptions about the data.
* Our expectations for the future can generally be wrong because either:
    * There was a random unlikely event
    * Our priors were wrong
* Often times, we can't distinguish these two. For example...

---

## Engineering estimates

* I estimate that a task will take 5 days to complete
* It ends up taking 8 days
* Non-actuary thinking: your estimate was wrong!
* Actuarial thinking:
    * How much confidence did I have in that 5 day estimate
    * Better way to express: there's a 70% chance that I complete the task in 5 days or less
    * How likely was an 8 day completion assuming the original estimate was correct?
    * Do I need to revise my estimation process for more accuracy in the future?

---

## Why analyze risk?

* Goal: maximize utility across all possible outcomes
* Yes, this is economics again
* Simple, no-risk case
    * Buy hamburger worth 5 HAPPY points for $4
    * Buy steak worth 6 HAPPY points for $3
    * Obvious: buy the steak
    * More complex if the steak is more expensive, then we need to compare value of money in HAPPY
    * All outcomes are guaranteed
* However...

---

## Gambling

You have $100,000 (that's your entire net worth). Let's consider these games.

| Game | Win on heads | Lose on tails |
| --- | --- | --- |
| 1 | $10 | $10 |
| 2 | $100 | $50 |
| 3 | $100,000 | $50,000 |
| 4 | $1,000,000 | $100,000 |

* Whether you play each game depends on _risk aversion_
* Are games 2 and 3 the same?
* Would you rather play game 1 or 4?

```
Utility = Sum(Probability(Event) * Utility(Event))
```

Losing all my money is more than twice as bad as losing half my money! (If I'm risk averse.)

---

## DevOps impact example

(Making up numbers.)

* A single AWS AZ has a 1% chance of failing during the course of a year.
* Failure of AZs is assumed to be _independent events_.
* What's the likelihood of 2 AZs failing?
* What's the likelihood of 3 AZs failing?
* Bad way of expressing DevOps best practices: we deploy to three AZs to ensure we never have downtime.
    * Do we really ensure that?
    * Why stop at 3 AZs?
* Good way of expressing this: the probability of three AZs simultaneously failing is so small that we consider this an _acceptable risk_
* SLAs are part of what helps us define acceptable risks

---

## Checkpoint summary

* Proper risk analysis helps us _understand possible outcomes_
* We need to get priors (assumptions), and make them as accurate as possible
* Priors can be refined by new, incoming data
* We aren't "wrong" if reality does not meet expectations, we either had bad priors or an unlikely event happened
* Risks don't mean "we avoid them entirely," it means we calculate the impact of different outcomes and make informed decisions

---

## Actuarial thinking in tech

Let's apply these concepts more directly to our own work.

---

## Risk assessment can be expensive

* How likely is it that I lose the coin-flip game after 10 rounds?
    * Define a betting strategy
    * Really easy to calculate the odds
    * Worth doing!
* How likely is it that a regulatory change from the US will result in catastrophic impacts to my business?
    * Much harder to estimate!
    * Requires deep research
    * There are different levels of analysis we can perform

---

## Risks on projects

Projects are chock-full of lots of different kinds of risk!

* **Technical Risk**: Bugs or system failures (e.g., a major bug slipping through testing).
* **Personnel Risk**: Team member illness or departure disrupting progress.
* **Hardware Risk**: Server or infrastructure failure causing downtime.
* **Competitive Risk**: A new competitor disrupting your blockchain project's market.
* **Customer Risk**: Changing requirements delaying delivery.
* **Regulatory Risk**: New blockchain regulations impacting project viability.
* **Process Risk**: Workflow bottlenecks or unclear requirements.
* **Financial Risk**: Budget overruns or unexpected cost increases.
* **Communication Risk**: Misaligned teams or stakeholders causing delays.

How do we deal with these?

---

## Identify most likely risks

* Use your own prior experience
* Brainstorm with experienced team members
* Do quick web searches
* This is a place where AI is _great_
* 25% chance that someone on the team will be sick over the next month: consider that risk
* 0.00000003% chance that AWS will stop supporting Linux: not so important
* Can ignore unimportant things (e.g. 45% chance I'll wake up drowsy one day this week and be 4% less productive)

---

## Identify highest impact risks

* Even unlikely risks, if impactful, should be considered
* 0.5% chance that our servers will have a hardware failure in the next year
    * Not very likely...
    * But can the project tolerate a 0.5% chance of an outage?
* Team member quitting/dying/major brain injury: could be catastrophic
    * Mitigations: documentation, knowledge transfer, code reviews, etc
* We'll still sometimes ignore impactful risks if they are too unlikely (e.g. we have no project plans for dealing with an alien invasion)

---

## Accept that we have "unknown unknowns"

* Priors: we already accepted that our risk assessments may have the wrong numbers
* We also sometimes will have unknown unknowns: risks we did not consider
* We need to do "due diligence" to find out what these are
* We'll never get all of them
* How much time we spend depends on importance of the project (upcoming slide)

---

## Accept that mistakes are likely

* We missed something
* Priors need updating
* Could have spent more time analyzing risks early
* Need to compare cost of analyzing risks vs likelihood of major negative impact from unknown unknowns

---

## Risk tolerance is project-dependent

* Proof of concept demo of a game? Run wild!
* Send a man to the moon? Be careful!
* Common mistake: being too risk averse when it's not needed
* Remember, being risk averse almost always increases costs!
    * Hardware: running in multiple AZs
    * Engineers: writing tests
    * Project: analyzing risks
* Helpful question: what's the worst that could happen?

---

## Wasted work

* Need to solve a problem, we come up with two ideas
* We rough estimate approach A at 2 days, approach B at 3 days
* We're not feeling great about the estimates
* It would take 4 engineer-hours to make better estimates
* Discuss: what should we do?

---

## Defaults

* Company standard: we like to use PostgreSQL, Rust, Docker, etc
* Non-risk reason: we build up expertise inside the company, people can swap between projects more easily, etc
* Risk related reason: we understand the risks of these tools better, less "unknown unknowns"
* Easier to make confident decisions since we know the priors are more accurate

---

## Eliminating risks

* Some risks can be completely eliminated!
* For example: if I use (safe) Rust, I have 0% risk of passing a `String` to a function that needs an `i32`
* Choosing tools that eliminate entire risk categories is great!
* Keep in mind that choosing those tools/processes/etc. may introduce its own costs
* Then we need to compare expected costs vs expected benefits of risk elimination

---

## Flexibility and feedback loops

* Bad way to operate: constantly worrying that we made the wrong decision
* Good way to operate: be responsive to new data
* "Hey look, we've gone over our estimates for the past 5 sprints, maybe we need to reassess our estimation process?"
* "We anticipated a 5% chance per day of overwhelming our database server, but it's only happened 0.5% of the time, are we over-provisioned?"
* Remember: it's not about "I made a bad decision."
* It's about: is this random bad luck, or are my priors incorrect?

---

## Conclusions

* We live in a quantum state of all possible outcomes
* Just because our expectations don't happen, doesn't mean we're "wrong"
* Life (and engineering) is about making the best decisions given our current data
* Assess the risks, determine how much effort to put into mitigation
* Stay flexible, be willing to change course when new data comes in
* Accept the fact that _mistakes will happen_
* Don't spend more on mitigating risks than the expected cost of those risks
* I don't want to get in a car crash, but I accept that it's a possibility every time I drive
* Insure yourself (through insurance, planning, etc.) against impactful, likely risks
