+++
title = "The Opposite of Partial Functions"
description = "Partial functions are often considered harmful, and we recommend developers use total functions instead. In this post, we'll explore what the true opposite of a partial function is."
path = "/blog/2023/04/opposite-of-partial-functions"
date = 2023-04-01

#[extra]
#ogimage = "double-cherry.jpeg"
+++
It would be fair, if cliche, to say "partial functions considered harmful." For those unfamiliar: partial functions are functions which are undefined for some valid inputs. A classic example would be Haskell's `head` function. It takes a list and gives you back the first item from the list. All well and good, but what should that function do in the case of an empty list?

Partial function enthusiasts would argue that `head` is fine as it is. It's the caller's responsibility to confirm that they don't pass in an empty list, e.g.:

```haskell
myFunction :: IO () -- lol not a function
myFunction = do
    someList <- getListFromEther
    if null someList
        then putStrLn "The list is empty!"
        else putStrLn $ "First item on list: " ++ show (head someList)
```

Opponents of partial functions will argue that this unnecessarily introduces possibilities for failure. One solution is to change the output type of `head` to use an explicitly optional value using `Maybe`. Then we're forced to deal with the possibility of an empty list:

```haskell
headMay :: [a] -> Maybe a -- implementation left as an exercise to the reader

myFunction = do
    someList <- getListFromEther
    case headMay someList of
        Nothing -> putStrLn "The list is empty!"
        Just x -> putStrLn $ "First item on list: " ++ show x
```

Another alternative is to change the input type to the function to make empty lists impossible. `getListFromEther` could return a `NonEmptyList` and we can provide a completely safe `head :: NonEmptyList a -> a`.

Both of these approaches are examples of converting a _partial function_ into a _total function_. We do that here by modifying either the input (or domain for math nerds) or the output (or range for math nerds, or codomain for younger math nerds) of the function. Total functions say that for every possible value in the domain, the function will return a non-bottom value in the codomain.

Side note to build up to a larger point: I just said "non-bottom value." You might _think_ that a "top value" would be the opposite of a "bottom value." But that's simply not true. You could have middle values, almost-bottom values, tippy-top values, pretty-high-but-not-quite-the-top, etc. So we need to talk about "bottom" and "non-bottom."

"But Michael," you say, "that's not at all what opposite means. You're talking about the _negation_ of bottom, not its opposite. Top is literally the opposite of bototm." That may be true, but I'm redefining words my way. My blog, my dictionary, my rules.

Anyway, coming back to the original point... people often get confused and think the opposite of partial functions is total functions. But partial and total functions are more similar than different. Consider `head`: for all three possible implementations we described above, it will return the same value for the vast, vast majority of possible lists. The _only_ time it returns a different result is in the one special case of empty lists.

Therefore, using exceptional<a id="footnotesrc1" href="#footnote1"><sup>1</sup></a> language skills and my patent-pending Snoyberg Dictionary, I would like to introduce a brand new concept that will revolutionize the programming world.

## Impartial functions

When you say "I'm partial to pistachio ice cream," you mean two things:

1. You have a tendency towards picking pistachio ice cream versus other flavors
2. You have very bad taste

Partial functions say the same thing. A partial `head` function is partial to non-bottom values, treating each of them separately and doing real work with them. The true opposite of a partial function is an _impartial function_. A function that makes no distinction on different input values. Let's take a properly written `headMay` function (yes, I said it was an exercise for you, but you were too lazy to write it, so I had to do it for you):

```haskell
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x
```

Look at how partial that function is, only returning `Nothing` for empty list! Here is a true impartial implementation:

```haskell
headMay :: [a] -> Maybe a
headMay _ = Nothing
```

We can do the same for the original `head` function by adding a typeclass constraint for the well regarded and highly recommended `Default` typeclass:

```haskell
head :: Default a => [a] -> a
head _ = def
```

## Constant functions

The astute reader--and I'm guessing you're astute--may have noticed that our current definition of impartial functions is actually the same as a constant function. One solution to this is simply to delete the dictionary definition for constant functions, which is actually my preference. But since the following code is so awesomely bad, let's go on.

I'm going to claim that constant functions are a _strict subset_ of impartial functions. Impartial functions cannot be partial to different values. But they _can_ be partial to non-values. Let's go back to the definition of total functions: for all well defined input (meaning non-bottom), the function must return a non-bottom value. A true impartial function can go in the same direction, and say that for all bottom input we provide non-bottom output, and for all non-bottom input, we provide bottom output.

What?

OK, that was a bit convoluted, but I'm sure this code will clear up any confusion immediately:

```haskell
import Control.DeepSeq (NFData, ($!!))
import Control.Exception (SomeException, try)
import Prelude hiding (head)
import System.IO.Unsafe (unsafePerformIO)

class Default a where
  def :: a
instance Default Int where
  def = 42 -- obviously

head :: forall a. (Default a, NFData a) => [a] -> a
head list =
  case unsafePerformIO $ try $ pure $!! list :: Either SomeException [a] of
    Left _ -> def
    Right _ -> error "Oops! Not bottom!"

main :: IO ()
main = do
  print $ head (undefined :: [Int])
  print $ head ([5] :: [Int])
```

This code is pretty straightforward and easy to follow, and hits all known Haskell best practices and code formatting recommendations, so we don't need to give it any more thought.

## Final observation

What we just explored demonstrates my true point here. We know, because words, that partial and impartial functions must be the opposite of each other. And we also just demonstrated that impartial functions are literally the opposite of total functions. That means we have a double negation: `!!partial == !impartial == total`. And since `!!x == x` in all languages (especially Javascript), we now know that partial and total functions are exactly the same thing!

We've been living a lie for decades. Don't let the haters stop you. Partial functions are just as safe as total functions. They're the same thing. And when you're ready to up your game, embrace impartial functions.

* * *

<a id="footnote1" href="#footnotesrc1"><sup>1</sup></a>I know no one will believe me, but that's actually a very clever pun.
