+++
title = "Kabbalah, Haskell, and the ST Monad"
description = "A surprising analogy between Haskell's immutability and ST monad, and the Kabbalistic idea that spiritual states are never erased."
path = "/blog/2026/06/kabbalah-haskell-st-monad"
date = 2026-06-26

[extra]
ogimage = "kabbalah-haskell-st-monad.png"
+++
For the past year, I’ve been learning Kabbalah for the first time in my life. It has been a fascinating journey. I haven’t really shared much about it publicly, and I’m not sure whether I’m going to continue discussing it on this blog or anywhere else.

However, I recently ran into an overlap between Kabbalistic teachings and purely functional programming which is, frankly, kind of hilarious. So I figured I would write something down for the three other people in the world who may be interested in this intersection.

To the other Kabbalistic religious Jews who know a little bit of Haskell: hey there.

This is a weird post to write, because I’m basically aiming at an audience that may know the functional programming side, may know the Kabbalistic side, may know both, or may know neither. So I’m going to introduce both. Let’s start with the part I’m much more comfortable with: functional programming.

## Immutability

Let’s say you have a list of numbers:

```haskell
[1, 2, 3, 4, 5]
```

And you want to add one to each number, ending up with:

```haskell
[2, 3, 4, 5, 6]
```

In many programming languages, the natural way to think about this is mutation. Somewhere in the computer’s memory, there are five numbers. We go to that memory and modify the values in place.

Purely functional programming languages like Haskell don’t really like this approach. One reason is that mutation makes code harder to reason about. Another is aliasing: if two different parts of your program are both referring to the same list, and one part changes the list, the other part is now holding on to something different from what it thought it had.

So one of the core ideas in Haskell, and in purely functional programming more generally, is immutability.

Once a value exists, you don’t change it.

But of course, we still need to “change” things. Programs that can’t produce new values aren’t very useful. So instead of modifying an existing value, we create a new value based on the old one.

In our example, we don’t mutate:

```haskell
[1, 2, 3, 4, 5]
```

into:

```haskell
[2, 3, 4, 5, 6]
```

Rather, we create a new list. The old list still exists. The new list exists too.

Now let’s make this a little more concrete. Imagine we had an operation that modifies one position in a list. You tell it which position you want to modify, how you want to modify it, and it gives you back a new list.

We could use that operation like this:

```haskell
[1, 2, 3, 4, 5]
[2, 2, 3, 4, 5]
[2, 3, 3, 4, 5]
[2, 3, 4, 4, 5]
[2, 3, 4, 5, 5]
[2, 3, 4, 5, 6]
```

Conceptually, this works. We never mutated any list. Every step produced a new list.

The problem is that this is terribly inefficient. We didn’t really want five intermediate lists. We wanted one final list.

Now, of course, we can define better operations for this specific case. We can use `map (+1)`, and everything is wonderful. That’s not the point I’m getting at here.

The question is: what happens when we want the efficiency of mutation, but we still want the safety and reasoning benefits of immutability?

## The ST monad

This is where the `ST` monad comes in.

I am not going to try to give a full explanation of monads here. That way lies madness, blog comments, and probably someone correcting me on category theory. For our purposes, a monad gives us a way to sequence operations. Do this, then do that, then do the next thing.

The `ST` monad is particularly interesting because it lets us perform local mutation inside an otherwise pure program.

A typical pattern looks something like this:

1. Start with an immutable vector.
2. Create a mutable copy of it inside `ST`.
3. Mutate that copy in place.
4. Freeze it back into an immutable vector.
5. Return the immutable result.

From the outside, nobody can observe the mutation. The mutable vector never escapes. The rest of the program just sees a pure function from one immutable value to another immutable value.

That means we can do something mutation-like internally while preserving the external semantics of pure functional programming.

Or, since I’m already writing a Jewish blog post: the mutation is kosher.

The key idea is containment. Haskell as a language does not generally embrace arbitrary mutation. But it gives us a controlled environment where local mutation is allowed, as long as the mutation cannot leak out and break the guarantees of the rest of the program.

Put differently: inside this little sealed-off world, mutation exists. From the outside, the whole thing is just one pure action.

Let’s put that to the side for a moment and switch to Kabbalah.

## אין העדר ברוחני

One of the core ideas I’ve encountered in Kabbalistic learning is:

אין העדר ברוחני

There is no absence in the spiritual.

Or said another way: in spirituality, nothing is ever erased, destroyed, or removed.

That is a very strange idea from the perspective of our physical world. In our world, things change all the time. A building stands, and then it is knocked down. A cup is whole, and then it breaks. A document exists, and then someone deletes it. A person is in one emotional state, and later they are in a different emotional state.

We naturally think in terms of mutation. Something was one way, and now it is another way.

But in the spiritual system, as I understand it, that is not what happens. When something “changes,” the original state is not destroyed. Instead, a new state comes into being.

Let’s take one example.

In Kabbalistic terminology, there is a concept called a מסך, a screen. The screen has some level of עביות, usually translated as thickness or coarseness. That thickness can be at different levels: Shoresh, Aleph, Bet, Gimel, Dalet.

Let’s say there is a Masach at level Dalet. It is exposed to light, and the light refines it, bringing it down to level Gimel.

In our normal physical intuition, we would say: the Masach used to be level Dalet, and now it has changed into level Gimel.

But spiritually, that’s not quite right. The level Dalet state is not erased. It remains. The refinement gives rise to a new state, a new Masach, now at level Gimel.

The old thing was not modified.

A new thing was created.

## This sounds suspiciously familiar

At this point, the overlap should be pretty obvious.

The Kabbalistic idea that nothing is ever removed or changed in spirituality sounds a lot like immutability in Haskell.

In Haskell, if I have a value, I don’t mutate that value. I create a new value based on the old one.

In the spiritual system, as I’m currently understanding it, a spiritual state is not mutated into a different spiritual state. The original state remains, and the new state emerges from it.

That’s already a neat overlap. But for me, the more interesting part is the connection to `ST`.

Because we clearly do not experience our world this way.

In this physical world, mutation seems very real. Things change. Things decay. Things are built, broken, refined, damaged, repaired, and transformed. We live in time and space. We experience before and after. We experience cause and effect. We experience becoming.

And that makes me wonder whether our physical world is, in some sense, like one giant `ST` action.

Inside the `ST` monad, mutation is real. You really can modify a mutable vector. You really can write to a mutable reference. Step one happens, then step two happens, then step three happens.

But from the outside, the entire `ST` computation is pure. No mutation leaked out. The outside world doesn’t see the intermediate states. It sees one pure transformation from input to output.

That feels like a surprisingly good analogy for our world.

Inside this world, mutation is real. Time is real. Our choices are real. Actions matter. Things can be damaged, repaired, elevated, or corrupted.

But from a higher spiritual perspective, perhaps the entirety of this world is one contained process. We begin from one state. We go through the entire history of this world, with all of its apparent mutations, choices, actions, and consequences. And at the end, there is a final corrected state.

Inside the process, mutation.

Outside the process, immutability.

To be clear: I am not claiming this is a rigorous Kabbalistic model. Please do not build theology on top of my Haskell analogy. That way lies even more madness than monad tutorials.

But as an analogy, it feels useful.

## Programming as spiritual training wheels

There is a broader point here that I keep noticing.

A lot of Kabbalistic ideas are incredibly abstract. That’s not a complaint. They are supposed to be abstract. We are dealing with realities that are not physical, not spatial, and not bound by the categories we normally use to understand the world.

But modern life gives us access to abstractions that earlier generations simply did not encounter in the same way.

Programming is one of the clearest examples.

In Kabbalah, distance is not necessarily physical distance. Spiritual distance is measured by similarity and difference of form. Two things are “close” if they are similar in form, and “far” if they are different in form.

That sounds strange if your only model of location is physical space.

But if you write software, this isn’t that strange.

In a computer game, an object may have an on-screen location: it is standing next to a tree, or inside a room, or ten meters away from another character. But where is that object “really”? It may be stored in memory somewhere completely unrelated to any of those visual relationships. Two objects that are “next to each other” in the game world may be nowhere near each other in memory. Two objects next to each other in memory may have nothing to do with each other in the game.

The represented space and the storage space are not the same thing.

As programmers, we deal with these kinds of distinctions all the time. We constantly move between levels of abstraction. We understand that something can be “located” in one structure conceptually while being represented somewhere entirely different physically. We understand that identity, equality, reference, state, and transformation are not simple ideas.

That doesn’t make Kabbalah easy. I’m not sure anything makes Kabbalah easy.

But I do think that spending a lifetime programming has given me some mental handles that make certain Kabbalistic ideas easier to approach. Or at least easier to misunderstand in interesting ways.

And this itself lines up with an idea I’ve heard in Kabbalistic learning: as we approach redemption, these teachings become more accessible and more widespread. I don’t know exactly what to do with that idea yet. But it is interesting to me that the modern world is filled with abstractions that make some of these concepts less foreign than they may have been centuries ago.

## No grand conclusion

I don’t have a big takeaway here.

I’m not announcing a new series. I’m not claiming to have discovered the secret Haskell encoding of the עולמות. I’m definitely not suggesting that the Ari was secretly using `runST`.

Mostly, I’m sharing a surprising overlap between two parts of my life that I never expected to meet.

On one side: Haskell, immutability, and the `ST` monad.

On the other side: אין העדר ברוחני, spiritual states that are not erased, and a physical world where change and mutation appear to be very real.

And sitting somewhere in the middle: one religious Jewish programmer, slightly amused that the mental model he used to explain mutable vectors may also help him think about Kabbalah.
