## FP is the new OOP

* Michael Snoyman
* Functional Conf 2022 (Virtual!)
* March 25, 2022

---

## I am not a functional programmer

* I've worked with Haskell for 12+ years professionally
* I use functionally-inspired code in other languages I use (Rust, TypeScript, even older Java)
* I don't consider myself a functional programmer
* Primary reason: I don't know what a functional programming language is!

---

## Let's talk psychology

* Credit to my good friend, Adam McCullough
* Freud pioneered much of the field of psychology
* These days, when you say "Freud" people think of Oedipus complex, Freudian slip, etc.
* That's because...

---

> The stuff he got right are, now, transparent to people -- because they've become ubiquitous to our understanding of psych.

---

## What is functional programming?

* I've been asking this question in talks for about 10 years!
* Higher order functions
* Immutability
* Declarative coding
* Lazy generators
* Rich type systems
* Type classes

Fair list? Maybe.

---

## Is C a good language?

* No, it's terrible!!!
* Pointers galore
* lol no generics
* Manual memory management
* Weak abstraction capabilities

Sounds fair, right? Wrong!

---

## Better question: why was C invented?

It's better than assembly!

* Portability
* Control structures
* Type safety
* Code reuse
* Easier memory management

Today, those points look like weaknesses of C. However, C was a huge step forward

---

## Object-oriented code sucks

This is an FP conference, so QED

* Big blob of global mutable state
* Class hierarchies make no sense
* `AbstractClassBeanFactoryFactory`
* Bad abstractions

---

## There's a lot more to OOP

* Combine data and code
* Name overloading
* Encapsulation
* Ideally _good_ abstractions
* Generics
* Programming to interfaces

Critics of OOP don't point to these concepts (usually). Why?

---

## Because those concepts won

```haskell
sum :: (Num a, Foldable t) => t a -> a
sum = getSum . foldMap' Sum
```

* This is great, idiomatic Haskell code
* We're programming to interfaces (`Num`, `Foldable`)
* We're dealing with abstractions
* We've encapsulated the complexity of `Sum` wrapping/unwrapping
    * OK, that's a bit of a stretch...
* Name overloading dominates, fully generic code

Good Haskell code is following OOP principles!

---

## Put down the pitchforks

* I'm being incredibly fast and loose with definitions
* People will object to my definitions of FP, OOP, etc.
* My points here is _not_ to argue for some specific definitions
* My real question is: why do so many of us despise OOP, while seemingly adhering to its principles to some level?

---

## Reviewing the history

* C made significant improvements on the programming status quo
* Most of those improvements were adopted by future languages
* C is now known for the "crappy" parts left behind
* OOP: rinse and repeat
* What's happening with FP?

---

## My favorite "FP" features

* Strong types, ADTs
* Cheap newtype wrappers
* Pattern matching
* Type classes

Rust and TypeScript both have these (to some extent)

Neither of those is an "FP" language

---

## Imperative languages are evil!

Look at how bad this C code is!

```c
#include <stdio.h>

int main() {
    int total = 0;
    for (int i = 1; i <= 10; ++i) {
        if (i % 2 != 0) continue;
        total += i * 3;
    }
    printf("total == %d\n", total);
    return 0;
}
```

* Mutable state
* Unnecessary short-circuiting
* Convoluted logic

---

## Beautiful FP in Haskell

```haskell
main =
    putStrLn $ "total == " ++ show total
  where
    total = sum $ map (* 3) $ filter even [1..10]
```

* Clean separation of logical components
* Fully immutable code
* Sensible combinators like `sum` and `map`
* Lazy generation of values

I've used variations on this code many times in the past

---

## Idiomatic Rust

```rust
fn main() {
    let total: u32 = (1..=10).filter(|x| x % 2 == 0).map(|x| x * 3).sum();
    println!("total == {}", total);
}
```

* Functionally identical to the Haskell code
* Provides the same advantages
* For now, Rustaceans will call this "functional style"
* But quickly this is simply becoming "idiomatic Rust"

---

## Crappy Rust code

You can still write bad code in Rust

```rust
fn main() {
    let mut total = 0;
    let mut i = 0;
    while i <= 10 {
        i += 1;
        if i % 2 != 0 {
            continue;
        }
        total += i * 3;
    }
    println!("total == {}", total);
}
```

* But people won't tell you "that's OOP" or "that's imperative"
* They'll tell you "that's not idiomatic"

---

## Crappy Haskell code

Just like you can write non-functional code in Haskell if you try hard enough

```haskell
import Data.IORef

main = do
    totalRef <- newIORef 0
    iRef <- newIORef 1
    let loop = do
          i <- readIORef iRef
          if i > 10
            then pure ()
            else do
              writeIORef iRef $ i + 1
              if even i
                then do
                  total <- readIORef totalRef
                  writeIORef totalRef $ total + i * 3
                else pure ()
              loop
    loop
    total <- readIORef totalRef
    putStrLn $ "total == " ++ show total
```

---

## What's my point?

* Functional programming has introduced amazing new concepts to the programming world
* We're all better off for FP's influence
* The best parts have _already_ been stolen by other languages
    * Lambdas have been introduced all over the place
    * Map/reduce is now an ubiquitous concept
    * Or my fun JavaScript/`Result` story...

---

## JavaScript and `Result`

* Team of JavaScript developers needed some Rust/WASM help
* Didn't know we were a Haskell company
* Needed help understanding the `Result` type in Rust
* Started explaining how it worked...
* "Oh, you mean it's a Monad?"

---

## The next generation

* New developers will learn languages with FP concepts built in
* JavaScript's `forEach`, immutable-by-default, pattern matching... all "normal" programming
* FP-focused languages will no longer stand out from the pack for these features
* Instead, like imperative and OOP before it, FP languages will stand out for what other languages _haven't_ adopted

---

## The cheese stands alone

What FP features are being ignored?

* Laziness (besides lazy generators)
* Purity (to some extent)
* Higher Kinded Types (to my chagrin)
* Category Theoretic nomenclature (though Monad surprisingly has caught on)

---

## What do we do about it?

* They've stolen all our great ideas!
* FP can no longer compete
* What do we do?

---

## Complain about it

* "Haskell did that first"
* "That's better in ML"
* "That's not as elegant as in Scala"

Outcome: ostracize developers. Don't do this.

---

## Admit defeat

* Oh well, FP languages are a waste of time
* Let's jump on the _other language_ bandwagon

It's missing the big picture!

---

## Declare victory

* FP concepts have won the day!
* This is what we actually cared about all along
* FP languages can quietly die while better languages take over

Much closer, but still not fully satisfying.

---

## Declare our niche

* Haskell's STM is IMO unrivaled in any other language
* Ability to write complex concurrent code with the `async` package is amazing
* Parser combinator libraries are still revolutionary
* Embedded DSLs via arbitrary operators still best-in-class

FP languages still excel beyond the features adopted by others, play to that advantage!

---

## Barriers to entry are reduced

* When I started learning and later teaching Haskell, massive barrier to entry
* The learning curve is no longer as high as it was
* Functional concepts are familiar
* Great sell to "normal" programmers: learning Haskell will help you write better FP code in Java/JavaScript/etc.

---

## Learn from the market

* FP has been the petri dish for new ideas
* Some of those ideas have flourished
* Others have been ignored
* We shouldn't dogmatically stick to our guns
* Maybe it's time to admit that some of our ideas aren't keepers

Which leads to my final point...

---

## Humility

* FP languages stand on the shoulders of giants
* It's worth going back and seeing what ideas _we_ should be stealing
* It's OK to adopt established best practices from industry
* Yes, FP improves on many *local maxima* situations in its predecessors
* But sometimes those ideas are just good, and we've rejected them

---

# Questions?

Thanks all!
