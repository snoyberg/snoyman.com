+++
title = "Let the API protect you"
description = "A small journey, in Rust, demonstrating the power of data driven APIs and strong types."
path = "/blog/2024/08/let-the-api-protect-you"
date = 2024-08-26
+++
Let's write a simple program to manage purchases at a small convenience store. The store only sells two items: eggs and apples. We know the price of each item, and we need to set aside 5% of every purchase for taxes. We should _really_ use a decimal type instead of floats for handling currency, but we'll simplify things a bit here for convenience.

```rust
fn main() {
    let mut accounts = Accounts::default();
    accounts.buy_eggs(6);
    accounts.buy_apples(10);
    println!("{accounts:#?}");
}

const TAX_RATE: f64 = 0.05;
const PRICE_PER_EGG: f64 = 0.75;
const PRICE_PER_APPLE: f64 = 0.5;

#[derive(Debug, Default)]
struct Accounts {
    company_balance: f64,
    taxes_paid: f64,
}

impl Accounts {
    fn log_purchase(&mut self, money: f64) {
        let taxes = money * TAX_RATE;
        self.taxes_paid += taxes;
        self.company_balance += money - taxes;
    }

    fn buy_eggs(&mut self, eggs: u64) {
        self.log_purchase(eggs as f64 * PRICE_PER_EGG);
    }

    fn buy_apples(&mut self, apples: u64) {
        self.log_purchase(apples as f64 * PRICE_PER_APPLE);
    }
}
```

We now have a highly sophisticated and bullet-proof accounting systems for our store, no tax auditor could ever object to such pristine book keeping! We continue to run our successful little business and soon make enough money to open a second location. Let's say our first business was in Arizona, and now we want to expand into the Nevada market.

All good... except that the tax rates in the two states are different! While Arizona is 5%, Nevada is 8%. How can we model this in our code?

One possibility would be to pass in the tax rate as a parameter to `log_purchase`. Let's give that a shot:

```rust
fn main() {
    let mut accounts = Accounts::default();
    accounts.buy_eggs(6, TAX_RATE_ARIZONA);
    accounts.buy_apples(10, TAX_RATE_NEVADA);
    println!("{accounts:#?}");
}

const TAX_RATE_ARIZONA: f64 = 0.05;
const TAX_RATE_NEVADA: f64 = 0.08;
const PRICE_PER_EGG: f64 = 0.75;
const PRICE_PER_APPLE: f64 = 0.5;

#[derive(Debug, Default)]
struct Accounts {
    company_balance: f64,
    taxes_paid: f64,
}

impl Accounts {
    fn log_purchase(&mut self, money: f64, tax_rate: f64) {
        let taxes = money * tax_rate;
        self.taxes_paid += taxes;
        self.company_balance += money - taxes;
    }

    fn buy_eggs(&mut self, eggs: u64, tax_rate: f64) {
        self.log_purchase(eggs as f64 * PRICE_PER_EGG, tax_rate);
    }

    fn buy_apples(&mut self, apples: u64, tax_rate: f64) {
        self.log_purchase(tax_rate, apples as f64 * PRICE_PER_APPLE);
    }
}
```

That's not too bad... until you realize that there's a bug in the code above. Look at the implementation of `buy_apples`. We've accidentally provided the `tax_rate` as the amount of money the apples cost! Easy mistake to make, and thankfully easy enough to fix:

```rust
fn buy_apples(&mut self, apples: u64, tax_rate: f64) {
    self.log_purchase(apples as f64 * PRICE_PER_APPLE, tax_rate);
}
```

"Huh," some vague part of my brain screams out. "It was way too easy to write buggy code. Can we fix that?" At this point, I think that proponents of dynamic typing can (rightfully) claim a small victory here. I've written some reasonable code in Rust, a statically typed language, and the compiler couldn't stop me from making a silly mistake. As a proponent of types, I begin to question the fabric of reality and my entire stance on programming. But no time for that, I'm too busy expanding my store to other states!

Soon enough, we're ready to expand further into Utah. Utah _also_ has a sales tax, but they exempt eggs from their sales tax because it's an essential good. (And if anyone's about to fact check me: I've completely made up all the tax rates and rules in this post.) Anyway, our existing `Accounts` struct and its API is totally up to the challenge here, and we can easily implement this correctly:

```rust
fn main() {
    let mut accounts = Accounts::default();
    accounts.buy_eggs(6, TAX_RATE_ARIZONA);
    accounts.buy_apples(10, TAX_RATE_NEVADA);
    accounts.buy_eggs(12, TAX_RATE_UTAH);
    accounts.buy_apples(2, 0.0); // essential goods have no taxes in Utah
    println!("{accounts:#?}");
}
```

Easy peasy... and broken! Once again, I've made a simple mistake, and the type system and my APIs have done nothing to protect me. I've set the tax rate in Utah at 0%... but for the purchase of apples, not eggs! Once again, it's an easy fix:

```rust
accounts.buy_eggs(12, 0.0); // essential goods have no taxes in Utah
accounts.buy_apples(2, TAX_RATE_UTAH);
```

But these recurring bugs are frustrating, and frankly the code structure is completely unsatisfactory. I've needed to put some of the logic for tax collection into our `main` function, while other parts live in `log_purchase`. And the types do nothing to protect us. Is there anything we can do about this?

## Strong types, local logic

I want to bash apart the code above using two principles:

1. Use strong types when possible. This isn't the same as static types. Static typing simply means that all variables have a known type. Strong typing is about making those types _meaningful_. In our `log_purchase` method, we currently have _weak typing_. We take two parameters, `money` and `tax_rate`. They're both `f64`s, and nothing prevents us from swapping the argument order by mistake.
2. Keep logic as local as possible. We're currently making decisions on the taxes in two places: determining the tax rate in `main`, and calculating the taxes incurred in `log_purchase`. We also need to pass that logic through the `buy_eggs` and `buy_apples` methods.

Let's start with trying to address the second point. I'd like to have all tax logic present in `log_purchase`. That means I need to know if the purchase is taxable or not. One possibility would be adding a new parameter to indicate if taxes should be collected:

```rust
fn log_purchase(&mut self, money: f64, tax_rate: f64, collect_taxes: bool) {
    let taxes = if collect_taxes { money * tax_rate } else { 0.0 };
    self.taxes_paid += taxes;
    self.company_balance += money - taxes;
}
```

But this fails both of our problems from above:

1. We've added in a new parameter, but it's just as weakly typed as an `f64`. (In this case, I'd call it *boolean blindness*.) While we don't have to worry about accidentally swapping parameters, who's to say if `true` means "collect taxes" versus "exempt from taxes?" Sure, you can look at the code or read the docs... but who's going to do that? I want my compiler to save me!
2. It still requires performing logic in the caller to determine if this particular purchase is required to pay taxes, which still keeps our logic split up.

Instead of this slapdash approach, let's try to think of it from the bottom up.

## Data driven

What information do we need to know to determine if taxes can be charged? Two things:

1. Which state the purchase took place in
2. What item was purchased

With that stated, it's easy enough to create some helper data types to begin modeling this more appropriately:

```rust
fn main() {
    let mut accounts = Accounts::default();
    accounts.buy_eggs(6, TAX_RATE_ARIZONA, State::Arizona);
    accounts.buy_apples(10, TAX_RATE_NEVADA, State::Nevada);
    accounts.buy_eggs(12, TAX_RATE_UTAH, State::Utah);
    accounts.buy_apples(2, TAX_RATE_UTAH, State::Utah);
    println!("{accounts:#?}");
}

const TAX_RATE_ARIZONA: f64 = 0.05;
const TAX_RATE_NEVADA: f64 = 0.08;
const TAX_RATE_UTAH: f64 = 0.09;
const PRICE_PER_EGG: f64 = 0.75;
const PRICE_PER_APPLE: f64 = 0.5;

#[derive(Debug, Default)]
struct Accounts {
    company_balance: f64,
    taxes_paid: f64,
}

enum State {
    Arizona,
    Nevada,
    Utah,
}

enum Item {
    Apples,
    Eggs,
}

impl Accounts {
    fn log_purchase(&mut self, money: f64, tax_rate: f64, state: State, item: Item) {
        let collect_taxes = match (state, item) {
            (State::Utah, Item::Eggs) => false,
            _ => true,
            // Or if, like me, you like to be really explicit:
            // (State::Arizona, Item::Apples)
            // | (State::Arizona, Item::Eggs)
            // | (State::Nevada, Item::Apples)
            // | (State::Nevada, Item::Eggs)
            // | (State::Utah, Item::Apples) => true,
        };
        let taxes = if collect_taxes { money * tax_rate } else { 0.0 };
        self.taxes_paid += taxes;
        self.company_balance += money - taxes;
    }

    fn buy_eggs(&mut self, eggs: u64, tax_rate: f64, state: State) {
        self.log_purchase(eggs as f64 * PRICE_PER_EGG, tax_rate, state, Item::Eggs);
    }

    fn buy_apples(&mut self, apples: u64, tax_rate: f64, state: State) {
        self.log_purchase(
            apples as f64 * PRICE_PER_APPLE,
            tax_rate,
            state,
            Item::Apples,
        );
    }
}
```

Now we're fully implementing our "essential goods" check within `log_purchase`, with none of the logic leaking out. And our new types are properly strong types; it's impossible to accidentally swap the `State` and `Item` with one of the `f64` parameters, since they have totally different types.

It's not like everything is perfect yet. We can still easily write this incorrect code:

```rust
accounts.buy_apples(2, TAX_RATE_UTAH, State::Nevada);
```

But this is _also_ easily rectified. Now that we're passing in a `State` parameter to `log_purchase`, we can determine the tax rate ourself within that function. And passing in a `State` value instead of an `f64` prevents us from accidentally providing the parameters in the wrong order.

But you may have noticed something else: the `tax_rate` parameter is now redundant! Thanks to providing more information to `log_purchase`, it can be more intelligent in its own functioning, reducing burden on callers and removing a potential mismatch such as this code:

```rust
#[derive(Clone, Copy)]
enum State {
    Arizona,
    Nevada,
    Utah,
}

impl State {
    fn tax_rate(self) -> f64 {
        match self {
            State::Arizona => 0.05,
            State::Nevada => 0.08,
            State::Utah => 0.09,
        }
    }
}

fn log_purchase(&mut self, money: f64, state: State, item: Item) {
    let collect_taxes = match (state, item) {
        (State::Utah, Item::Eggs) => false,
        _ => true,
    };
    let taxes = if collect_taxes {
        money * state.tax_rate()
    } else {
        0.0
    };
    self.taxes_paid += taxes;
    self.company_balance += money - taxes;
}
```

And just like that, `log_purchase` doesn't require _any_ outside logic to determine how to collect taxes. You simply, declaratively, and in a strongly-typed manner, provide it the information necessary for it to do its job, and the method carries out all the logic.

We could even go a step farther if we wanted, and have `log_purchase` handle the calculation of the cost of the goods too:

```rust
fn log_purchase(&mut self, quantity: u64, state: State, item: Item) {
    let collect_taxes = match (state, item) {
        (State::Utah, Item::Eggs) => false,
        _ => true,
    };
    let money = quantity as f64 * item.price();
    let taxes = if collect_taxes {
        money * state.tax_rate()
    } else {
        0.0
    };
    self.taxes_paid += taxes;
    self.company_balance += money - taxes;
}
```

And with that in place, you may even decide that helper methods like `buy_eggs` and `buy_apples` aren't worth it:

```rust
fn main() {
    let mut accounts = Accounts::default();
    accounts.buy(6, State::Arizona, Item::Eggs);
    accounts.buy(10, State::Nevada, Item::Apples);
    accounts.buy(12, State::Utah, Item::Eggs);
    accounts.buy(2, State::Utah, Item::Apples);
    accounts.buy(2, State::Nevada, Item::Apples);
    println!("{accounts:#?}");
}

#[derive(Debug, Default)]
struct Accounts {
    company_balance: f64,
    taxes_paid: f64,
}

#[derive(Clone, Copy)]
enum State {
    Arizona,
    Nevada,
    Utah,
}

impl State {
    fn tax_rate(self) -> f64 {
        match self {
            State::Arizona => 0.05,
            State::Nevada => 0.08,
            State::Utah => 0.09,
        }
    }
}

#[derive(Clone, Copy)]
enum Item {
    Apples,
    Eggs,
}

impl Item {
    fn price(self) -> f64 {
        match self {
            Item::Apples => 0.5,
            Item::Eggs => 0.75,
        }
    }
}

impl Accounts {
    fn buy(&mut self, quantity: u64, state: State, item: Item) {
        let collect_taxes = match (state, item) {
            (State::Utah, Item::Eggs) => false,
            _ => true,
        };
        let money = quantity as f64 * item.price();
        let taxes = if collect_taxes {
            money * state.tax_rate()
        } else {
            0.0
        };
        self.taxes_paid += taxes;
        self.company_balance += money - taxes;
    }
}
```

## Conclusion

OK, so we moved some code around, centralized some logic, and now everything is nicer. We have some type safety in place too. You may be looking at this as small gains for introducing a lot of type complexity. But here are my closing thoughts:

1. Sure, this silly example may not warrant the type machinery for protection. But it's very easy to scale up from such a simple example to real-world use cases where the type safety prevents far more complex and insidious bugs.
2. I'd argue that there's not really any complexity here. We introduced two new data types and a new method on each of them, but also removed two helper functions and five constants. I'd take that trade in complexity any day.
3. The next set of features we want to implement will become even easier to make. For example, take both the original weakly typed version and the new strongly typed version, and try implementing these changes:
    1. In Arizona only, reduce the cost of apples to 0.45 per apple when you purchase 12 or more.
    2. Allow the price of the goods to change during the course of execution. In other words, don't hard-code in all the prices.
   In my opinion, the strongly typed version makes both of these tasks much easier and safer.

So what's the overarching lesson to be learned here? I'd put it this way:

> Identify the inputs needed for your functions to perform all their logic, avoiding splitting up that logic into multiple parts of your code base. Use well defined, strong types to represent that input cleanly.

It may sound simple, and perhaps obvious. But the next time you feel yourself succumbing to writing yet-another-weird-hack to address an unexpected business requirement, see if reframing the question from "how can I quickly add this feature" to "what's the best way to model the requirements as inputs and outputs" helps you come up with a better design.
