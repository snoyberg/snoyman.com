I'm assuming most people reading this post like static types, given my typical audience. I love both static types and strong typing. (I'll define the difference in a second.) I've been in the functional programming community for a long time via Haskell. But in all honesty, as much as I like many of the ideas of functional programming (immutability, declarative programming, higher order functions, etc.), all of them pale in comparison to just one feature of Haskell: strong, static typing. That's part of the reason I've been so happy to add Rust to my toolchain: it's not as functional as Haskell, and requires me to think at a lower level of abstraction than I'm accustomed to, but it gives me basically all of the type safety that I want.

Static typing is easy to understand. All languages, even Javascript or Python, have some level of notion of types. Static typing means whether the types are checked at compile time or runtime. There are some more nuanced points around gradual typing, optional typing, and add-on type systems, but the basic idea stands: static typing is a binary. Since I don't believe in it and never have, I won't try to make an argument for the dynamic typing side. But I can definitely make an argument for static typing:

> I want to find out, as early as possible, that my code is broken

If I'm in my editor working on a feature, and a red line pops up saying "hey, this argument is supposed to be a `Person`, not a `String`," I'm _happy_. Sure, it's annoying having to break my flow right then and fix that. But (1) I can batch up those errors to address at the end of my current workflow and (2) spending five minutes fixing that now can possibly save hours of frantic repairs to a production system.

"But wait!" you say. "Testing can address that too!" Absolutely, and you should write tests! I won't go quite this far, but a coworker of mine says that if the test doesn't exist, the feature doesn't exist. Tests are necessary, and they can and do prevent many bugs from going to production. Never, ever listen to someone who says "if it compiles it works." That's a lie. The point here is: use as many layers of protection as possible.

Strong typing is more nuanced than static typing. I don't think there's such a thing as a "strong typing language." I don't think there's an easy test for "this is strong typing." Strong typing is a spectrum of how a code base is set up. Stronger typing means that more things are enforced at the type level, and therefore caught by the type checker at compiler time. My go-to example for this would be replacing usage of a plain `String`, which could be _anything_, with appropriate newtype wrappers like `Name` or `EmailAddress`. This allows the type system to prevent you from accidentally using the wrong value in the wrong place.

In this world, no language is "strongly typed." Nothing in Haskell or Rust, for example, forces you to define such newtype wrappers. And on the other hand, languages like Java or C that most would think of as _not_ strongly typed still allow you to create newtype wrappers. I judge languages as being "strong typing friendly" by how easy they make it to do these things. Both Haskell and Rust make newtype wrapper definitions easy, and provide robust deriving mechanisms so that you can easily do things like create a `HashMap` with a `Name` newtype as a key. In my book, that's what makes these languages great.

That last paragraph buried an important concept, and really the heart of this blog post. My phrase was "how easy they make it to do these things." I have a philosophy around software, and the philosophy is simple. Software is written by humans. Humans make mistakes. And humans will take shortcuts, especially when under pressure of a deadline (like virtually all commercial software is).

Once when working on Java code, I mocked sum types via church encoding. If you don't know what that means, don't worry. The important point is that, what would have been 5 lines of Haskell became 5 20-line Java files. And it didn't work as elegantly as Haskell. I wrote that because I was stubborn, and honestly because I was a little curious if it would work. From a cost/benefit trade-off, it's unclear that this effort was worth the added stability in the code. In fact, if the me of today was my manager back then, I would have rejected that code as "too much effort to maintain, and not idiomatic Java."

Alright, to summarize my points up to here, the idea is:

* Catching mistakes early is a good thing
* Static types is one way to do that
* Tests are another way to do that
* The more strongly typed your code, the most mistakes you'll catch early on with static types
* Humans _will_ make mistakes, so we need tools that protect us
* Humans _will_ take shortcuts, so we should make it as easy as possible to do the right thing

One more simple example to throw in, and then I'll get into my final point. External analysis tools are _wonderful_. `cargo clippy` is so trivially easy to run on a Rust codebase that people actually do it. Linters, memory tools, and so on, are awesome. Include them in your CI process, and they'll have even more power, because everyone will pay attention to them. Humans may take shortcuts, but machines do not.

## Learning from Clojure

Funnily enough, while searching for the original quotes for this section, I landed on a [blog post by Edward Yang](http://blog.ezyang.com/2016/12/thoughts-about-spec-ulation-rich-hickey/), so apparently I'm not the only Haskeller to take these ideas to heart.

Clojure is not a statically typed language. Rich Hickey is perhaps famous to the rest of the FP world for being opposed to static typing. (I haven't heard comments from him directly in a while, so if I'm misrepresenting him, I apologize, and please correct me.) I disagree with this premise, which should be obvious from the comments above. However, this line of thinking is vital, because even in a dynamically typed world, no one wants bugs to appear at production. Rich has approaches to mitigating these in the absence of static typing. And I think it behooves us in the statically typed world to adopt these practices, adjusted accordingly for the power of static types.

Leveraging Edward's breakdown of Rich's points, there are two different ideas I want to focus on:

1. Never remove a function
2. Never mutate a function

Edward's post is great, and I agree with a lot of what he says there, so I won't repeat too much. I agree in principle that removing a function puts a burden onto users of a library. And taking such actions should be weighed carefully, and only undertaken if there's strong motivation. Many Haskellers, including myself, have a tendency too strongly in favor of "clean up the APIs" instead of "don't break people's code." Therefore, I think we can learn a lot from Rich's opposing point.

However, I strongly disagree with _never_ removing a function. I think the word "never" may apply correctly in a dynamically typed world, where removing a function may break things silently in a way that is only discovered in production. Yes, tests can help mitigate that. But as I said: humans will take shortcuts, humans will make mistakes, and I want multiple layers of protection. I see where Rich's approach comes from. I'm glad we don't need to take such an approach in statically typed languages. Instead, here's my viewpoint:

> When deciding whether to remove a function, weight the benefits of cleaning up the API/reducing maintenance burden/preventing usage of a bad function against the costs of all users of that function needing to update their codebase.

For economics afficiandos, you may be thinking about negative externalities now and the tragedy of the commons.

The second point is far more interesting. With static types, mutating a function to take an extra argument, but behave the same way, acts just like removing the function. Everyone using your API incurs a cost. You have to weigh that. That's why I've been moving many of my libraries gradually towards using settings types, since it's easier to extend parameters without causing any breakage.

On the other hand, mutating a function in a semantic way is really dangerous. If you change the API in some way, and you have static typing, the damage is _mostly_ mitigated, since a user of the function will be forced to do something. However, even in that case, they may blindly apply some changes until the code compiles, not noticing line 243 in the ChangeLog noting the important semantic change that goes along with it. Testing, again, can help. But (1) see my repeated points above about human behavior, and (2) unfortunately types give us a false sense of security, and statically typed code bases are often lacking in necessary testing. (Yes, point (2) is really an extension of point (1), this is all about lazy humans.)

So I'll pull in a _never_ from Rich, slightly modified: never semantically mutate functionality under the same name. If your new version of a function does something different, give it a new name. If you can, keep the old version under the original name as a compatibility shim. Add a `DEPRECATED` if you feel you must.

## Typeclass instances

The most painful example of all of this is typeclass instances. Let's say you have two law abiding versions of a typeclass instance. As a crazy example, consider `Semigroup` for `Int`. Both addition and multiplication form a semigroup. Let's say that in one version of base, we introduce an instance that uses addition. And then a consensus is reached that, for some reason, multiplication is clearly better.

You have basically four options:

1. Leave the original addition instance
2. Immediately move to the multiplication instance, noting it in the changelog
3. Remove the addition instance completely, never adding a new instance
4. Remove the addition instance completely, then later add a new multiplication instance

Of these options, using my framework of thinking above, (2) is by far the worst. It all but guarantees that code will break. And it will break in horribly surprising ways. I would place a large sum of money down that no one, in the entire history of software development, has ever gone through every single line of code in their codebase when upgrading major versions, and compared each of those lines of code against every comment in a changelog. Maybe in some regulated industry where it's contractually required. But I bet even there they took shortcuts.

The reason why (2) is so bad is that it's a complete betrayal of the ethos of static typing. We know we're breaking code. We know people are using a language like Haskell because they want things to fail at compile time. We know that this change will _never_ break code at compile time. We know this change will almost certain cause incorrect behavior at runtime. And we're including it anyway.

## Conclusion

So, to tie all of this together:

* No one wants errors in production
* Catching errors early is therefore a Good Thing
* Static typing helps catch some errors early, so it's also a Good Thing
* Following strong typing principles strengthens static typing, so it's also a Good Thing
* Adding more layers of protection via tests, linters, and static analysis is also a Good Thing
* Languages without static types need to find other ways to avoid errors in production, and we can learn from them
* Learning from them: avoid silent, semantic API changes like the plague
* Weigh the impact on your users of changes that will be caught by the compiler
* Do as much as you can to make the Good Things easy and the Bad Things awkward and difficult
