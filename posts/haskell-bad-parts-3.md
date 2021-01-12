If you didn't see it, please check out [part 1 of this series](/blog/2020/10/haskell-bad-parts-1) to understand the purpose of this. You can also check out [part 2](/blog/2020/11/haskell-bad-parts-2). Now, for more bad parts!

Note that this is the last blog post in this series where I had specific ideas queued up. So it's likely that this will be the last post for a while until I get annoyed by something again. So if you really want to see a continuation, find some bit of Haskell code that will annoy me and share it with me!

## Pattern matching

OK, this section title is a complete troll. I don't think pattern matching is a bad part of Haskell. I think pattern matching is one of the most powerful features in all of Haskell, especially when combined with sum types/ADTs. If someone asked me for a singular reason to consider using Haskell, this is what I'd point to.

Which is why the bad parts of pattern matching in Haskell are so bad. My "Haskell elevator pitch" is almost immediately undermined by terrible defaults.

My elevator pitch goes something like this: with sum types, you get to define your data types far more explicitly than in languages with only product types. And then you can rely on the compiler to tell you when you've made a change to a data constructor, or added a new data constructor. I usually try to understand first what someone's field of experience is and then build a real-ish example of sum types from that.

Unfortunately, that's all a lie, at least by default. Some specific parts of pattern matching _do_ in fact let the compiler tell you when you've screwed up. (And if it's not clear: my favorite part of GHC is when it tells me I've screwed up.) If I delete a data constructor, or if I add a new field to a data constructor and I was pattern matching on positionally, the compiler will error.

But this code compiles just fine, and I wish it didn't!

```haskell
data MySum = Foo | Bar | Baz

main :: IO()
main =
    case Baz of
        Foo -> putStrLn "Foo"
        Bar -> putStrLn "Bar"

```

Not only does it compile just fine, but *there are no warnings*! "OK Michael," you say. "Who cares? _Everyone_ knows that you should compile with `-Wall` turned on." To that, I say three things:

1. No, not everyone knows it
2. Because of spurious warnings in `-Wall`, like unused import warnings, important warnings like this often get drowned out. (Yes, it's better to be warning-free for this reason...)
3. Yeah, but it gets worse

Let's elaborate on (3). I'm a good programmer. So I turn on `-Wall`. Now I'm guaranteed a warning on incomplete pattern matches... right? Right?!? Right!!!!!!1111oneoneone

```haskell
{-# OPTIONS_GHC -Wall #-}
data MySum = Foo | Bar | Baz

main :: IO()
main = (\Foo -> putStrLn "Foo") Bar
```

Nope, denied! I get no warning here. Even though I have a _refutable pattern match_ in my lambda, the compiler provides no warnings. "Didn't you know you needed to turn on `incomplete-uni-patterns` for that?" Actually, no, for a long time I didn't know that.

```haskell
{-# OPTIONS_GHC -Wall -Wincomplete-uni-patterns #-}
data MySum = Foo | Bar | Baz

main :: IO()
main = (\Foo -> putStrLn "Foo") Bar
```

Fortunately, there's some [hope on the horizon](https://gitlab.haskell.org/ghc/ghc/-/issues/15656) for this one.

One more annoyance, only tangentially related. GHC: please stop generating partial functions on my behalf!

```haskell
data Staff
    = Principal
    | Teacher { subject :: String }

main :: IO()
main = putStrLn $ "Your subject is " ++ subject Principal
```

I just discovered that this can be detected with `-Wpartial-fields`, which is nice, but not part of `-Wall`. I've been giving advice for a long time to not include any field labels in sum types, and I'll continue to give that advice.

Note that some Haskellers won't like: this topic came up for me recently while writing a blog post comparing pattern matching in Haskell and Rust (probably going live next week). And, yet again, Rust does this better than Haskell. Yes, Rust is strict and Haskell is lazy. But as far as I'm concerned, in this case, Rust is simply more type safe than Haskell.

## Slow compilation

Let's get an elephant in the room out in the open. Compiling is slow. Some of this is because of GHC. Some of this is because our libraries are trying to do overly complicated things (like stream fusion in `vector` and `text`). This is a bane for Haskell, constantly.

As a recent example, I needed to add a flag to `http-conduit` to disable the `aeson` dependency:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">.<a href="https://twitter.com/snoyberg?ref_src=twsrc%5Etfw">@snoyberg</a> kindly provided &quot;-aeson&quot; flag to http-conduit which saves my time very much. Thanks.<br>BTW, why does building aeson take so much time?</p>&mdash; 山本和彦 (@kazu_yamamoto) <a href="https://twitter.com/kazu_yamamoto/status/1334988010434150401?ref_src=twsrc%5Etfw">December 4, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

I'm not going to beat up on this too much, since GHC is a complicated project and people are trying hard to both improve the existing support _and_ add new features. And library authors (myself included) are caught between providing features and speeding up compilation.

There is a part of me that wonders if part of the problem is that our standard library (`base`) doesn't provide enough functionality out of the box, and leaves a lot of external libraries to implement and reimplement similar functionality. As more time goes on, I think Vincent Hanquez was [completely correct about how to improve Haskell](https://github.com/haskell-foundation) and I wish I'd embraced it.

## Text

There's so much wrong here. And I can't even blame it all on the `text` package. But I can blame a lot of it on the `text` package. Consider this section incoherent rambling instead of well reasoned arguments. (Typically a fair assumption with anything I say to be honest.)

* Why oh why is it UTF-16 instead of UTF-8? This is terrible! Except...
* It's completely irrelevant what encoding `text` uses under the surface, because *you can't directly interact with its underlying representation*. And _that's_ because...
* `text` uses unpinned memory to avoid heap fragmentation, which is a Good Thing, but then `text` can't be directly used in FFI calls. Which is probably a good thing, since it uses the wrong character encoding under the surface.
* `text` defines its own `Builder` type. It shouldn't. We should be using the `ByteString` `Builder` everywhere, and have a `newtype` wrapper around it to encode the invariant "must be UTF-8 encoded." `Builder` is a _wonderful_ thing in Haskell, and it gets used far less than it should because it's such a PITA to deal with all of this. (I tried to address that in `rio`.) This is exactly the same as how we don't use `Vector` enough because of all the flaws with using it.
* And, of course, `text` has stream fusion, which is shouldn't. I'll touch on this even more in the next section.

Again, Vincent got this all right. We need a singular packed data type. It should underly a `ByteString` and a `Text`. A `Text` should be a simple newtype around a `ByteString`. Users should never have to make a decision around pinned vs unpinned memory, that should be an implementation detail. (And sure, why not, give users a knob somewhere to control this when they know better than the defaults.) Have a single `Builder` type that builds up `Vector`s, and have it work for `ByteString` and `Text` too.

Oh, and just a side note...

**MAKE TEXT UBIQUITOUS**

We are now more than 10 years since the first release of `text` to Hackage. And people are still regularly using `String`. **I** regularly use `String` in examples. We really should be able to move our ecosystem over to better types.

We've broken everyone's code for far less important things many times in the past. Just to rile you all up: Foldable/Traversable Proposal wasn't important or interesting, this should have taken precedence.

## Lazy data structures

There should be no lazy `ByteString`. There should be no lazy `Text`. There isn't a lazy `Vector`, and that's a Good Thing.

"But what if I want to read a 10GB file into memory lazily?" You're using the wrong abstraction.

"But what if I want to lazily generate a massive file?" You're using the wrong abstraction.

Here's the problem: we don't have the right abstraction in `base`. We ended up at a local minimum.

* Lazy lists get used for "generators" and for data storage, when they're only half good for the former
* We don't get a real generator type in `base`, because we have lazy lists
* We don't get a real data storage type in `base`, because `vector` exists and sucks

Here's what we need to do:

* Get rid of lazy lists from the language entirely
* Add a streaming data interface to `base` that properly handles side-effects without hacks like lazy I/O
* Provide great tooling around that streaming data interface, like stream fusion
* Add a `Vector`-like type to `base` that only handles storage, no fusion or generation, and have it work with the streaming data interface

Again, I'll say: Vincent got it right.

## Qualified imports

Since everyone loves to fixate on trivial syntactic issues, let me include one of those here at the end so everyone can ignore my other points. Qualified imports suck. Actually, modules suck. Actually, modules and packages suck. Actually, it all sucks.

The first problem? Our names are too verbose. `import qualified Control.Monad.Trans.Class as Trans`. I've actually typed that, many times. That's too much typing. Why is it `Control.Monad`? How does it help me?

What package does that module live in? You can't tell from the name. It's `transformers`. I think. Oh wait, no, now it's in `base`. My bad. Anyway, packages are able to just trample all over each others' namespaces like this. It's annoying.

And because Haskell doesn't have object syntax, importing identifiers directly, or qualified importing modules, is an absolute must for accessing most functionality on types. OOP kinda beat us here.

Yay, more incoherent rambling!

## Should we fix this?

OK, serious talk here. I think the warnings issue I mentioned above should absolutely be fixed. For the last section: I don't see a reality where a holistic change to Haskell syntax, module hierarchy, etc, could ever be possible. We're stuck with what we've got, possibly with minor ergonomic improvements.

I don't know what to say about the standard library stuff I've been raising. I really do believe that a combination of good streaming abstraction, built-in packed data, and more sensible `ByteString` and `Text` types in the standard library itself could be a huge improvement. It's also the kind of thing that takes a huge amount of buy in and effort, and I don't think it would happen easily.

The biggest questions that pop up for me would be:

1. What would the streaming abstraction be? Contrary to what I bet you're thinking, I don't think it should be based on `conduit` or anything like it. Instead, I'd base it on the stream fusion types directly. I think it's a simpler abstraction, and fits the goals better.
2. Where do these types live? I think long term, if we want this kind of thing to succeed, we need them to be part of `base`. But we'd need to have a community-wide effort around building a separate library first, get huge buy-in, and then consider adding them to `base`. And `foundation` is probably already pretty close to what we'd end up building.

My gut feeling is that no one has the stomach for this. I don't think I do. It's certainly tempting though.
