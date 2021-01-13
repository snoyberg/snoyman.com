+++
title = "Haskell: The Bad Parts, part 2"
description = "The second part of a blog post series on the parts of Haskell we should avoid using.\n"
path = "/blog/2020/11/haskell-bad-parts-2"
date = 2020-11-09

[taxonomies]
series = ["haskell-bad-parts"]

[extra]
+++
If you didn't see it, please check out [part 1 of this series](/blog/2020/10/haskell-bad-parts-1) to understand the purpose of this. Now, for more bad parts!

## Partial functions (in general)

Laziness very likely belongs in this list. My favorite part of criticizing laziness is how quickly people jump to defend it based on edge cases. So let's be a bit more nuanced before I later get far _less_ nuanced. Laziness is **obviously** a good thing. Strictness is **obviously** a good thing. They also both suck. It depends on context and purpose. Each of them introduce different kinds of issues. The real question is: what's a more sensible default? We'll get to that another time.

I called this section partial functions. Am I having a senior moment? Maybe, but I intentionally started with laziness. In a strict language, function calls can result in exceptions being thrown, segfaulting occurring, or panicking. (And if I write a "Rust: The Bad Parts", believe me, I'll be mentioning panicking.) The fact that a function _acts_ like it can successfully perform something, but in fact fails in a predictable way (like failing a `HashMap` lookup), it should be reflected at the type level. If not, ya dun goofed.

Also, if you have a language that doesn't let you reflect this information at the type level: ya dun goofed.

Partial functions are the antithesis of this concept. They allow you to say "yeah dude, I can _totally_ give you the first value in an empty list." Partial functions are like politicians: you can tell they're lying because their lips are moving. ("But Michael," you say. "Functions don't have lips!" Whatever, I'm waxing poetical.)

Alright, so plenty of languages screw this up. Haskell tells those languages "hold my beer."

<img src="/static/images/holdmybeer.jpg" style="max-width:90%">

Haskell screws up partial functions way, way worse than other languages:

1. It promotes a whole bunch of them in the standard libraries and `Prelude`.
2. Some libraries, like `vector` (I'm getting to you, don't worry) make it _really_ confusing by providing an `index` and `unsafeIndex` function. Hint: `index` isn't really safe, it's just less unsafe.
3. There's no obvious way to search for usages of these partial functions.
4. And, by far, the worst...

### Values are partial too!

Only in a lazy language does this exist. You call a function. You get a result. You continue working. In any other non-lazy language, that means you have a value. If I have a `u32` in Rust, I actually have a `u32` in Rust. Null pointers in languages like C and Java somewhat muddy this situation, but at least primitive types are really there if they say they're there.

No, not Haskell. `x :: Int` may in fact not exist. It's a lie. `let x = head [] :: [Int]` is a box waiting to explode. And you find out _much_ later. And it's even worse than that. `let alice = Person { name = "Alice", age = someAge }` may give you a valid `Person` value. You can evaluate it. But Cthulhu help you if you evaluate `age alice`. Maybe, just maybe, `someAge` is a bottom value. Boom! You've smuggled a dirty bomb out.

I'm not advocating for removing laziness in Haskell. In fact I'm not really advocating for much of anything in this series. I'm just complaining, because I like complaining.

But _if_ I was to advocate some changes:

* Deprecate partial functions
* Introduce a naming scheme for partial functions to be more obvious
* Introduce a compiler warning to note partial function use (with a pragma to turn off specific usages)
* Warn by default on partial pattern matches
* Advocate strict data fields by default

### But ackshualllly, infinite loops

Someone's gonna say it. So I'll say it. Yes, without major language changes, you can't prevent partial functions. You can't even detect them, unless Turing was wrong (and I have my suspicions.) But Haskell community, please, please learn this lesson:

**DON'T LET THE PERFECT BE THE ENEMY OF THE GOOD**

We can get rid of many of the most common partial functions trivially. We can detect many common cases by looking for partial pattern matches and usage of `throw` (again, horribly named function). "But we can't get everything" doesn't mean "don't try to get something."

## Hubris

Given what I just said, we Haskellers have a lot of hubris. Each time you say "if it compiles it works," a thunk dies and collapses into a blackhole. We've got plenty of messes in Haskell that don't sufficiently protect us from ourselves. The compiler can only do as good a job as our coding standards and our libraries allow.

"But Haskell's at least better than languages like PHP." I mean, obviously I agree with this, or I'd be writing PHP. But since I'm being ridiculously hyperbolic here, let me make a ridiculous claim:

> **PHP is better than Haskell, since at least you don't get a false sense of security**
>
> *- Michael Snoyman, totally 100% what he actually believes, you should totally quote this out of context*

I've said this so many times. So I'll say it again. Using a great language with safety features is one tiny piece of the puzzle.

* Did you get the software requirements right?
* Did you leverage the type system to prevent the bugs you're trying to prevent?
* Do your underlying libraries have bugs?
* Did you find a way to implement a function with correct types but incorrect semantics?
* Did you host the thing on a dinky server sitting under your desk and forget that you have power outages on a daily basis?
* Did you forget to write a single test case?
* Do your test cases actually test anything meaningful?

There are *so many ways* for software to fail outside the purview of the type system. We've got to stop thinking that somehow Haskell (or, for that matter, Rust, Scala, and other strongly typed languages) are some kind of panacea. Seriously: the PHP people at least know their languages won't protect them from anything. We should bring some of that humility back to Haskell.

Haskell provides me tools to help prevent certain classes of bugs, so I can spend more of my time catching a bunch of other bugs that I'm absolutely going to write. Because I'm dumb. And we need to remember: we're all dumb.

## More partial functions!

You know what's worse than partial functions? Insidiously partial functions. We've all been screaming about `head` and `tail` for years. My hackles rise every time I see a `read` instead of `readMaybe`. I can't remember the last time I saw the `!!` operator in production code.

But there are plenty of other functions that are just as dangerous, if not more so. More dangerous because they aren't well known to be partial. They are commonly used. People don't understand why they're dangerous. And they fail only in edge cases that people aren't thinking about.

Exhibit A: I present `decodeUtf8`. (Thanks [Syd](https://twitter.com/kerckhove_ts/status/1321390954172063745?s=20).)

Go ahead, search your codebase. Be dismayed that you've found it present.

What's wrong with `decodeUtf8`? As we established last time, character encoding crap breaks stuff in production. UTF-8 works about 99% of the time, especially for people in Western countries. You'll probably forget to even test for it. And that function looks so benign: `decodeUtf8 :: ByteString -> Text`.

**DO NOT BE FOOLED**

This function is a ticking time bomb. Use `decodeUtf8'` (yes, it's named that badly, just like `foldl'`) and explicitly handle error cases. Or use I/O functions that explicitly handle UTF-8 decoding errors and throw a runtime exception.

"I can't believe Michael still thinks runtime exceptions are a good idea." I'll get to that another time. I don't really believe they're a good idea. I believe they are omnipresent, better than bottom values, and our least-bad-option.

## Law-abiding type classes

Now I've truly lost it. What in tarnation could be wrong with law-abiding type classes? They're good, right? Yes, they are! The section heading is complete clickbait. Haha, fooled you!

There's a concept in the Haskell community that all type classes should be law-abiding. I've gone to the really bad extreme opposing this in the past with early versions of `classy-prelude`. In my defense: it was an experiment. But it was a bad idea. I've mostly come around to the idea of type classes being lawful. (Also, the original namespacing issues that led to `classy-prelude` really point out a much bigger bad part of Haskell, which I'll get to later. Stay tuned! Hint: Rust beat us again.)

Oh, right. Speaking of Rust: they do _not_ believe in law-abiding type classes. There are plenty of type classes over there (though they call them `trait`s) that are completely ad-hoc. I'm looking at you, `FromIterator`. This is Very, Very Bad of course. Or so my Haskell instincts tell me. And yet, it makes code Really, Really Good. So now I'm just confused.

Basically: I think we need much more nuanced on this in the Haskell community. I'm leaning towards my _very_ original instincts having been spot on. So:

* Law abiding type classes: great
* Flippantly non-law-abiding type classes ala the original `classy-prelude`: bad
* "You know what I meant" typeclasses like `ToContent` in Yesod: also great

This isn't exactly in line with a "bad part" of Haskell. Up until now I've been giving a nuanced reflection on my journeys in Haskell. Let me try something better then. Ahem.

**DON'T LECTURE ME ON LAW ABIDING TYPE CLASSES AND FLAGRANTLY VIOLATE LAWS**

I'm staring at you, `Eq Double`. No, you cannot do equality on a `Double`. (And thanks again to Syd for this idea.) Rust, again, Got It Right. See `PartialEq` vs `Eq`. Floating point values do not allow for total equality. This makes things like `Map Double x` dangerous. Like, super dangerous. Though maybe not as dangerous as `HashMap Double x`, which deserves its own rant later.

So come down from your high horses. We don't have law abiding type classes. We have "if I close my eyes and pretend enough then maybe I have law abiding type classes."

## Unused import warnings

Haskell has a dumb set of default warnings enabled. ("I think you mean GHC, one implementation of Haskell, not Haskell the language itself." Uh-huh.) How can we _not_ generate a warning for a partial pattern match? Come on! ADTs and pattern matching is _the_ killer feature to first expose people to. And it's a total lie: the compiler by default doesn't protect us from ourselves.

So of course, we all turn on `-Wall`. Because we're good kids. We want to do the right thing. And this, of course, turns on unused import warnings. And because each new release of GHC and every library on Hackage likes to mess with us, we are _constantly_ exporting new identifiers from different modules.

The amount of time I have spent adding weird hacks to account for the fact that `<>` is suddenly exposed from `Prelude`, and therefore my `import Data.Monoid ((<>))` isn't necessary, is obscene. The introduction of fiddly CPP to work around this sucks. In fact, this all sucks. It's horrible.

I didn't realize how bad it was until I spent more time writing Rust. It reminded me that I never had these problems back as a Java developer. Or C++. This has been a Haskell-only problem for me. Maybe I'll get into this later.

Side note: I'm trying to avoid turning this series into "Rust is better than Haskell." But the fact is that many of the problems we face in Haskell _don't_ exist in Rust, for one reason or another. This specific issue is due to a better crate/module system, and object syntax. As long as we're getting our cards on the table, I also think Rust demonstrates what a good freeze/unfreeze story would look like (I'm looking at you `Map` and `HashMap`), what a good standard library would be, and what a regular release schedule with backwards compat should look like. Oh, and of course good community processes.

## The vector package

Did I just mention that Rust can show us what a good standard library is? Yes I did! I'm only going to begin to scratch the surface on how bad `vector` is here. It's really bad. `vector` is at the level of being _just bad enough_ that no one wants to use it, but being _just barely serviceable enough_ and _just wide spread enough_ that no one wants to replace it. There are two Haskellers I deeply trust who have taken on efforts to do just that, and even I haven't moved over.

Firstly, the `vector` package is a package. It shouldn't be. Packed arrays should be in the standard library. Rust got this right. `Vec` is completely core to the language. And it ties in nicely with built-in syntax for arrays and slices, plus the `vec!` macro. Haskell's got lists. You can turn on `OverloadedLists`, but I don't know if anyone does. And besides, you've gotta reach outside of `base` to get a `Vector`.

`vector` is slow to compile. Dog slow. Sloth-recovery-from-Thanksgiving-dinner slow. I'm peeved by this right now because I had to compile it recently.

`vector` seems to have a litany of runtime performance issues. I haven't tested these myself. But people regularly complain to me about them. Enough people, with enough Haskell experience, that I believe them. (And `text`: you're in this list too.)

Oh, right, `text`. `vector` is completely different from `bytestring` and `text`. And there's the pinned-vs-unpinned memory issue that screws things up. I'm unaware of any other language needing to make that distinction. (If there _are_ examples, please let me know. I'd love to read about it.)

Stream fusion is dumb. I mean, it's not dumb, I love it. "Stream fusion should be a default when performing array operations so that you magically mostly fuse away intermediate buffers but sometimes it doesn't work because rewrite rules are really fiddly and then my program consumes 8GB of memory whoopsie daisy" is dumb. Really, really dumb.

The API for `vector` is lacking. Sure, for what it does, it's fine. But every time I use `.push()` in Rust, I'm reminded that it could and should be better. I don't want to work with lists all the time. I want to have a mutable vector API that I'm happy to use. I want to sprinkle more `runST` throughout my code. I want phrases like "freeze" and "thaw" to be common place, much like `mut` is regularly used in Rust.

Oh, and there's no such thing as a strict, boxed vector. Boo.

So in sum:

* `vector` tries to do two things (streaming library and packed buffers)
* It does neither particularly well
* There are enough friction points that people don't use it where it should be used, making lists overused
* It's not core to the language and libraries, but it's "kinda core," preventing better options from succeeding

"Michael, you're not being very nice to the maintainers of this library." I mean these comments in complete respect. I've _advocated_ for many of the things I'm now saying are bad. We learn new stuff and move on. If I had a vision for how to make `vector` better, I'd propose it. I'm just airing my concerns. I have a vague idea on a nicer library, where you have a typeclass and associated type determining the preferred storage format for each type that can be stored, growable storage, an easy-to-use freeze/thaw, minimal dependencies, quick compile, and associated but separate stream fusion library. But I think working on it would be like [adding a new standard](https://xkcd.com/927/), so I'm not jumping into the fray.

## Next time...

I've dropped plenty of hints for future parts in this series. But I'd really love to hear ideas from others. Thanks to Syd for providing some of the fodder this round. And thanks to a number of people for mentioning partial functions.

I kind of think I owe some attacks on async exceptions. Stay tuned!

## Clarification

Someone's gonna get upset about my Turing comment above. No, I'm not challenging Turing on the halting problem. I only wanted to imply I was for poops and giggles.
