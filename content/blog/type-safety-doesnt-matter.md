+++
title = "Type Safety Doesn't Matter"
date = 2023-08-28
+++
I'm a huge believer in using strongly typed languages and leveraging type level protections in my codebases. Inspired by events at work, I have a new, modified stance on this:

**Type safety does not matter.**

What I mean is that, on its own, type safety is not important. It's only useful because of what it accomplishes: moving errors from runtime to compile time. Even that isn't a goal on its own. The real goal is reducing runtime errors. Type safety is one of the best methods of achieving these cascading goals, but it's far from the only one.

This may sound pedantic and click-baity, but in my opinion it's a vitally important distinction with real world ramifications. For example, when discussing architecture of code or reviewing a pull request, I will often times push back on changes that add more complexity in the type system. The reason is because, even if a change adds "type safety," this extra complexity is only warranted if it achieves our primary goal, namely reducing runtime errors.

Such an assessment is largely speculative, subjective, and risk-based. By that last point, I'm tapping into my actuarial background. The idea is that, when considering a code change, the question will always be: do I think there's a good probability that this change will meaningfully reduce bug count in the long term more so than other activities I could be spending this time on? And if you watched my talk [the economic argument for functional programming](https://www.youtube.com/watch?v=n7QETok5hYI) (or [read the slides](https://www.snoyman.com/reveal/economic-argument-functional-programming)), you may be familiar with this way of thinking as the _opportunity cost_ of spending more time on type safety.

This is why languages that provide for strong typing with type inference end up working out so well. There's relatively little cost for basic type safety mechanisms with significant gain. It's the 80/20 rule. I continue to believe that the vast majority of the value I've received from strongly typed languages like Rust, Haskell, and even TypeScript come from the "simplest" features like enums/ADTs and pattern matching.

Bug reduction is not the only benefit of strong typing. There's also: easier codebase maintainability, simplicity of refactoring, new engineering onboarding, potentially performance gains, and probably a few other things I missed. But for me, reduction in bugs is still the primary benefit.

This paradigm of assessing the value in bug reduction from type safety lets us broaden our scope a bit. If we want to reduce bugs in production, and we believe that moving bugs from runtime to compile time is a good way to do it, we can naturally find some related techniques. An obvious one is "static code analysis." But I'll simplify that with the 80/20 rule as well to *linting tools*. Using linting tools is a great way to get lots of benefits with little cost.

Just to prove this isn't only about types, let's take a concrete example from everyone's favorite language, JavaScript. If I'm writing a React application in JavaScript, I get virtually no type safety. (TypeScript is a different story, and it's the only thing that keeps me sane when working on frontend code.) Consider this bit of almost-correct React code:

```javascript
const { userName, gameLevel } = props
const [userScore, setUserScore] = useState()

useEffect(() => {
    const helper = async () => {
        const res = await fetchUserScore(userName, gameLevel)
        const body = await res.json()
        setUserScore(body.score)
    }
    helper()
}, [userName, setUserScore])
```

For those not familiar: `useEffect` allows me to run some kind of an action, in this case an asynchronous data load from a server. This is a common pattern in React. As the user is using this application and changes the game level, I want to perform an action to load up their current score from the server and set it in a local store that can be used by the rest of the application. `useEffect` takes two arguments: the function to perform, and the list of dependencies to use. When one of those dependencies changes, the effect is rerun.

There are plenty of improvements to be made in this code, but there's one blatant bug: my `useEffect` dependency list does not include `gameLevel`. This would be a bug at runtime: once the user's score is loaded for a level, we would never reload it despite moving on to other levels. This would be the kind of bug that is easy to miss during manual testing, and could end up in production pretty easily.

Automated testing, unit tests, QA acceptance guidelines... basically everything around quality assurance will help ameliorate bugs like this. But static analysis does even better. This kind of code will immediately trigger lints saying "hey, I see you used `gameLevel` in your function, but you didn't list it in your dependencies." This is a prime example of moving a bug from runtime to compile time, preventing an entire class of bugs from occurring, and it didn't need any type safety to do it. Sure, it doesn't eliminate every potential bug, but it does knock down a whole bunch of them.

As you might imagine, this blog post was inspired by a specific set of problems I was running into at work. I thought about getting into those details here, and if there's interest I can write a follow-up blog post, but honestly the specific case isn't terribly interesting. My point here is the general principles:

1. Understand _why_ you're trying to use type safety. Is it preventing some kind of a bug from occurring? Is the time you're spending on implementing the type-safe solution paying off in bug reduction and other benefits?
2. There are lots of other techniques worth considering for bug reduction. Static analysis is one I mentioned. Automated testing falls into this category as well. Don't be ideologically driven in which approaches you use. Choose the tool with the best power-to-weight ratio for what you're dealing with right now.
