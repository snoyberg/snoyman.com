There's a popular book called "JavaScript: The Good Parts." And there's a common meme around the relative size of that book versus "JavaScript: The Definitive Guide."

<img src="https://i.imgur.com/wIf3EJh.jpg" style="max-width:80%">

Haskell is in my opinion a far more well designed and coherent language than JavaScript. However, it's also an old language with some historical baggage. And in many ways it's a bleeding edge research language that sometimes includes... half-baked features. And due to an inconsistent set of rules around backwards compatibility, it sometimes will break code every six months, and sometimes keep strange decisions around for decades.

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">True mastery of Haskell comes down to knowing which things in core libraries should be avoided like the plague.<br><br>* foldl<br>* sum/product<br>* Data.Text.IO<br>* Control.Exception.bracket (use unliftio instead, handles interruptible correctly)<br><br>Just as some examples</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/1321049221697544193?ref_src=twsrc%5Etfw">October 27, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

After a request and some tongue-in-cheek comments in that thread, I decided a longer form blog post was in order. I'm going to start off by expanding on the four examples I gave in that tweet. But there are many, many more examples out there. If there's more interest in seeing a continuation of this series, please let me know. And if you have pet peeves you'd like me to address, input will be very welcome.

## What is a "bad part"

Very rarely is there such a thing as a language feature, function, type, or library that is so egregiously bad that it should never, ever be used. Null is of course the billion dollar mistake, but it's still incredibly useful in some cases. So when I say that something is a "bad part" of Haskell, I mean something along these lines:

* A rarely-useful feature has been promoted to a position of prominence
* A function has major downsides that are not documented
* There's an unexpected performance implication

There's a large tendency in the Haskell community to be overly literal in responding to blog posts. Feel free to do that to your heart's content. But this caveat serves as a word of warning: I'm not going to caveat each one of these with an explanation of "yes, but there's this one corner case where it's actually useful."

## Why attack Haskell?

Since I'm a Haskeller and advocate of the language, you may be wondering: why am I attacking Haskell? I don't see this as an attack. I _do_ wish we could fix these issues, and I think it's a fair thing to say that the problems I'm listing are warts on the language. But every language has warts. I'm writing this because I've seen these kinds of things break real world projects. I've seen these failures manifest at runtime, defeating yet again the false claim that "if it compiles it works." I've seen these become nefarious time bombs that disincentivize people from ever working with Haskell in the future.

I hope by calling these out publicly, I can help raise awareness of these problems. And then, either we can fix the problems at their source or, more likely, get more widespread awareness of the issue.

Also, because it feels appropriate, I'm going to take a more jovial tone below. I personally find it easier to beat up on a language I love like that.

## foldl

Duncan Coutts [already did this one](https://www.well-typed.com/blog/2014/04/fixing-foldl/). `foldl` is broken. It's a bad function. Left folds are supposed to be strict, not lazy. End of story. Goodbye. Too many space leaks have been caused by this function. We should gut it out entirely.

But wait! A lazy left fold makes perfect sense for a `Vector`! Yeah, no one ever meant that. And the problem isn't the fact that this function exists. It's the __name__. It has taken the hallowed spot of the One True Left Fold. I'm sorry, the One True Left Fold is strict.

Also, side note: we can't raise linked lists to a position of supreme power within our ecosystem and then pretend like we actually care about vectors. We don't, we just pay lip service to them. Until we fix the wart which is overuse of lists, `foldl` is only ever used on lists.

OK, back to this bad left fold. This is all made worse by the fact that the true left fold, `foldl'`, is not even exported by the `Prelude`. We Haskellers are a lazy bunch. And if you make me type in `import Data.List (foldl')`, I just won't. I'd rather have a space leak than waste precious time typing in those characters.

Alright, so what should you do? Use an alternative prelude that doesn't export a bad function, and does export a good function. If you really, really want a lazy left fold: add a comment, or use a function named `foldlButLazyIReallyMeanIt`. Otherwise I'm going to fix your code during my code review.

## sum/product

The `sum` and `product` functions are implemented in terms of `foldr`. Well, actually `foldMap`, but list's `foldMap` is implemented in terms of `foldr`, and lists are the only data structure that exist in Haskell. "Oh, but `foldr` is the good function, right?" Only if you're folding a function which is lazy in its second argument. `+` and `*` are both strict in both of their arguments.

If you're not aware of that terminology: "strict in both arguments" means "in order to evaluate the result of this function/operator, I need to evaluate both of its arguments." I can't evaluate `x + y` without knowing what `x` and `y` are. On the other hand, `:` (list cons) is lazy in its second argument. Evaluating `x : y` doesn't require evaluating `y` (or, for that matter, `x`). (For more information, see [all about strictness](https://www.fpcomplete.com/haskell/tutorial/all-about-strictness/).)

"But wait!" you say. "What if I have a custom data type with a custom typeclass instance of `Num` that has a custom `+` and/or `*` that is in fact lazy in the second argument! Then `sum` and `product` are perfect as they are!"

That's true. Now go off and write your own `lazySum` and `lazyProduct`. 99 times out of 100, or more likely 999,999 times out of 1,000,000, we want the fully strict version.

"But it doesn't matter, GHC will optimize this away." Maybe. Maybe not. Stop relying on GHC's optimizer to convert horribly inefficient code into not efficient code. (But I digress, we'll talk about why the `vector` package is bad another time.)

## Data.Text.IO

I've already covered this one once before when I told everyone to [beware of `readFile`](https://www.snoyman.com/blog/2016/12/beware-of-readfile). In that blog post, I talk about a bunch of `String` based I/O functions, especially the titular `readFile`, which is obnoxiously exported by `Prelude`. Those are bad, and I'll reiterate why in a second. But `Data.Text.IO` is arguably far worse. The reason is that there's pretty good awareness in the community that `String`-based I/O is bad. Even though the `String` part is the least of our worries, it does a good job of scaring away the uninitiated.

But `Data.Text.IO` is a wolf in sheep's clothing. We're all told by people who think they can tell people how to write their Haskell code (*cough* me *cough*) that we should exorcise `String` from our codebases and replace it in all cases with `Text`. Attacking the `Text` type is a topic for another time. But the problem is that by cloaking itself in the warm embrace of `Text`, this module claims more legitimacy than it deserves.

The only module worse in this regards is `Data.Text.Lazy.IO`, which should be buried even deeper.

OK, what exactly am I on about? Locale sensitive file decoding. It's possible that this has been the number one example of a Haskell bug in the wild I've encountered in my entire career. Not the spooky memory leak. Partial functions like `head` randomly throwing exceptions are up there, but don't quite rise to prominence.

You see, when you are dealing with file formats, there is typically an actual, defined format. YAML, XML, JSON, and many others give a lot of information about how to serialize data, including character data, into raw bytes. We want to be consistent. We want to write a file in one run of the program, and have it read in a separate run. We want to write the file on a Windows machine and read it on a Linux machine. Or we want to interact with programs in other languages that read or write data in a consistent format.

Locale sensitive file encoding and decoding laughs in our face. When you use `Data.Text.IO.readFile`, it plays a mind reading game of trying to deduce from clues you don't care about which character encoding to use. These days, on the vast majority of systems used by native English speakers, this turns out to be UTF-8. So using `readFile` and `writeFile` typically "just works." Using functions from `Data.Text.IO` looks safe, and can easily get hidden in a large PR or a library dependency.

That's when all hell breaks loose. You ship this code. You run it in a Docker container. "Oops, you forgot to set the `LANG` env var, Imma crash." But it's worse than that. Typically things will work well for weeks or months, because it can often be a long time before someone tries to encode a non-ASCII character.

The same kind of thing happens regularly to Stack. Someone adds a new feature that writes and reads a file. The code passes all integration tests. And then someone in Russia with a weird Windows code page set and a Cyrillic character in their name files a bug report 2 years later about how they can't build anything, and we sheepishly tell them to run `chcp 65001` or build in `c:\`.

Friends don't let friends use `Data.Text.IO`.

"Oh, but `putStrLn` is fine!" Yeah, maybe. It's also potentially slow. And it will throw a runtime exception due to character encoding mismatches. Just use a good logging library. That's why we have one in `rio`.

__EDIT__ Since so many people have asked: instead of `readFile`, I recommend using [`readFileUtf8`](https://www.stackage.org/haddock/lts-16.20/rio-0.1.19.0/RIO.html#v:readFileUtf8), which is available from [`rio`](https://github.com/commercialhaskell/rio).

## Control.Exception.bracket

This is by far the least objectionable of the bad things in this list. I included it because the entire original tweet was inspired by a coworker telling me about a bug he ran into because of this function.

Async exceptions are subtle. Very, very subtle. Like, super duper subtle. I've devoted a large percentage to my Haskell teaching career towards them. Async exceptions are a concept that don't truly exist in most other languages. They require rewiring the way your brain works for proper handling. Helper functions help alleviate some of the pain. But the pain is there.

Then someone said, "You know what? Async exceptions aren't subtle enough. Let's invent two different ways of masking them!"

Wait, what does masking mean? Well, of course it means temporary blocking the ability to receive an async exception. Totally, 100% blocks it. It's like async exceptions don't exist at all. So you're totally 100% safe. Right?

Wrong. Masking isn't really masking. Masking is kinda-sorta masking. No, if you really want protection, you have to use `uninterruptibleMask`. You knew that, right? Of course you did, because it's so incredibly obvious. And of course it's painfully obvious to every single Haskeller on the planet just how important it is to choose normal `mask` versus `uninterruptibleMask`. And there'd never be a disagreement about these cases.

In case the tone isn't clear: this is sarcasm. Interruptible vs uninterruptible masking is confusing. Incredibly confusing. And nowhere is that more blatant than in the `Control.Exception.bracket` function. Interruptible masking means "hey, I don't want to receive any async exceptions, unless I'm doing a blocking call, then I totally don't want to be masked." And `Control.Exception.bracket` uses interruptible masking for it's cleanup handler. So if you need to perform some kind of blocking action in your cleanup, and you want to make sure that you don't get interrupted by an async exception, you have to remember to use `uninterruptibleMask` yourself. Otherwise, your cleanup action may not complete, which is Bad News Bears.

This is all too confusing. I get the allure of interruptible masking. It means that you get super-cool-looking deadlock detection. It's nifty. It's also misleading, since you can't rely on it. Really good Haskellers have released completely broken libraries based on the false idea that deadlock detection reliably works. It doesn't. This is a false sense of hope, much like rewrite rules for stream fusion.

I'm not putting `mask` on the "bad parts" list right now, but I'm tempted to do so, and claim that `uninterruptibleMask` should have been the default, and perhaps only, way of masking. (Reminder for later: `throw` is a horribly named function too.) But I _am_ saying that `bracket` defaulting to interruptible masking is a mistake. It's unexpected, and basically undocumented.

In `unliftio` (and therefore in `rio`) we provide an alternative `bracket` that uses uninterruptible masking. I debated this internally quite a bit, since I don't generally like throwing new behavior into an old function name. I eventually agreed with the idea that the current `bracket` implementation is just buggy and should be fixed. I still feel a bit uneasy about the decision though. (Note that I made the opposite decision regarding `sum` and `product` and included the broken versions, which I _also_ feel uneasy about.)

Credits for this one go to [Yuras](https://github.com/Yuras) and [Eyal](https://github.com/Peaker), check out [this Github issue for details](https://github.com/fpco/safe-exceptions/issues/3).

## Conclusion

I'll reiterate: Haskell is a great language with some warts. Ideally, we'd get rid of the warts. Second to that: let's be honest about the warts and warn people away from them.

If you'd like to see more posts in this series, or have other ideas of bad parts I should cover, please let me know in the comments below or [on Twitter](https://twitter.com/snoyberg).
