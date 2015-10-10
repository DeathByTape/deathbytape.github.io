---
layout: post
title: Understanding the Practical Use of Types in Haskell
excerpt: The strong typing in Haskell is for more than the theorists!
categories: articles
tags: [haskell,types,type jiu jitsu,practical,programming]
comments: true
share: true
ads: true
---

Anyone who has heard of Haskell has no-doubt found _someone_ that claims, "Haskell's type system is incredibly powerful!" The discussion then typically turns into a Haskell-evangelism lecture filled with jargon and concepts which newcomers cannot easily follow. As Haskellers, we tend to get excited about the correctness properties and guarantees of pure code, the flexibility of GADTs and TypeFamilies, and we take pride in the ability to have all of this power and flexibility with great terseness and elegance (read: we don't need all the boilerplate bloat that comes along with writing _anything_ on the JVM). I'm excited just thinking about it. The sad truth is, however, **most** (read: every dev or architect you currently know who does not regularly use Haskell) people don't care about these things.

Now, maybe I'm not being completely fair. It isn't that they don't necessarily care, but it doesn't seem practical to obsess over the type signature for each function; this feels burdensome and unnecessarily time-consuming to the majority of developers. Until recently, however, I was a little perplexed by this response. I mean, how could you *possibly* not see the value in this fantastic typing system? Given my day-to-day sampling at my day job (which is big data and not at all Haskell-driven), I have begun to notice a common pattern: it's actually not initially obvious. Thinking back, when I started learning Haskell it wasn't obvious to me either. It has taken me time to drill-down and get some *real* work done in Haskell before I've started noticing the **practical** benefits of Haskell's type system.

This post will outline a few of these practical use cases for the strong type system in Haskell. I'm sure current Haskellers reading this post will recognize that they use these benefits all of the time (whether having explicitly thought about them or not). That being said, the target audience here is non-Haskell programmers; you don't even need to be a functional programmer. I am aiming to show the value of this type system to the working engineer who says, "well I need to produce a product by the end of the day: I don't have time for toy languages with toy benefits." I do expect some familiarity with what a type signature is if you want to follow closely, but if you believe everything I say without verification (note: I never advocate that position for trusting anyone), you will probably be fine to gloss over the fine details and come back to them. And now, without further ado, why "real" programmers should care about Haskell and its type system.

# The List

[There have been reports](http://www.theguardian.com/media-network/media-network-blog/2012/mar/19/attention-span-internet-consumer) showing that readers' attention spans are shrinking. As a result, if you've already made it this far, I'm probably about to lose you so I want to quickly show you the benefits up front (to the patient reader: don't worry, we'll dig into them in more detail below).

  1. **Quick understanding of a function and how to use it.** Knowing the types and a little bit about the expected behavior of a function provides users a lot of information about a function and its use. This is where the "if it compiles, it's probably correct"-mentality comes from. This does make the assumption that the underlying logic is also correct, of course.
  1. **Quick lookup reducing the amount of wheel reinventing.** If you are looking for a very specific function in Haskell, you can look it up by its type. For instance, if I am looking for a function which will `reverse` a list, I can perform a [Hoogle](https://www.haskell.org/hoogle/) search for `[a] -> [a]` and quickly find what I'm looking for.
  1. **Test for code correctness worry-free.** When done properly, *pure* code is very modular and easily testable. Your code won't even compile if you try to pass an incompatible type into a function. In short, null pointers don't exist and you won't be tricked into thinking you have one type of object and it's actually something else. In practice, this really cuts down on the test cases you need to write and when you add [QuickCheck](https://hackage.haskell.org/package/QuickCheck) to the mix, life gets even better.

As you can see, there are some pretty large productivity benefits here. However, I do not claim this to be an exhaustive list; instead, it is merely one that should appeal to the working software engineer.

# What does that function do?

When I started reading Haskell code, I saw many statements which appeared complex. However, it's important to realize that by its very nature, nothing is all that complex in Haskell *code* since you can always break things down (remember, [all functions are curried](https://wiki.haskell.org/Currying)). In practice, what this means is that if I see a scary looking statement, I break down into pieces and look at the types of each unit. Let's look at an example:

```haskell
scaryStatement = foldl (flip (:)) [] [1..20]
```

umm. What?

Calm down and keep reading! Let's review our situation: we're given a statement like this without a type signature and we want to know what the heck it's doing.

> NOTE: If you were to see a question about this on StackOverflow, you would certainly get a comment that looks like, "oh, the `foldl flip` thing is just a long way to say `reverse`". To a new Haskeller, this "obvious" statement is not so obvious. I would say that it's almost discouraging while trying to learn since you're unfamiliar with the process of gaining that intuition.

You can find all sorts information on the interwebz about all the common methods used here (i.e. `foldl`, `flip`, `(:)`). In particular, [Hoogle](https://www.haskell.org/hoogle/) is typically a great starting point but there is also [Hayoo](http://hayoo.fh-wedel.de/) and [FPComplete's Hoogle](http://www.stackage.org/). For simplicity, we'll stick to Haskell.org Hoogle for now.

While looking up these functions, we can quickly gather their type signatures and relevant English descriptions. I'll outline them below:

```haskell
-- foldl, applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the
-- list using the binary operator, from left to right: > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- The list must be finite.
foldl :: (a -> b -> a) -> a -> [b] -> a

-- flip f takes its (first) two arguments in the reverse order of f.
flip :: (a -> b -> c) -> b -> a -> c

-- Ok, so this one wasn't on Hoogle. I'll give this one to you, though
-- from the output of ghci (i.e. by running ":t (:)")
-- (:) prepends an element to a list
(:) :: a -> [a] -> [a]
```

If you apply the functions correctly, the English descriptions may be enough to see that this is taking each element of the list and reversing its position. However, you're still not entirely sure exactly what the function signature looks like for `scaryStatement` (i.e. how to use the function).

Now we have these type signatures, but it's probably still not all that obvious on how to use them. Alright my enterprise engineering friends, it's time to do something very uncomfortable for us: change (I blame you Java). We have to put on our functional hats for a moment to really appreciate the value of what we have here. Since [all functions are curried in Haskell](https://wiki.haskell.org/Currying) and [partial application is a beautiful thing](https://wiki.haskell.org/Partial_application) the types and descriptions tell us _precisely_ what this function is doing and how to use it. Let's reorganize this first:

```haskell
complexPart = foldl (flip (:)) []
```

We've just isolated the complex part of our statement. Now, trust me (or not: [try the statement yourself](https://tryhaskell.org/)) that this statement will compile. What are we left with here? Well, all we have is a partially applied `foldl` function. In particular, arguments 1 and 2 are already bound. With this knowledge alone, we see that our function `complexPart` is of type `[b] -> a`.

Though this is true, we can even get more specific on our type signature. Without taking you step-by-step through basic type-inference, we know the type of `(flip (:))` empirically: it matches `foldl`'s first argument type of `(a -> b -> a)`. Likewise, we notice `[]` is bound as `a` in our `foldl` application. If we now match the `a`'s and `b`'s with this new information, we can see that `a = [r]` and `b = r` (due to the type of `(:)`), so our final type signature for `complexPart` is `[r] -> [r]`.

> NOTE: Our choice of type `r` is arbitrary. It was an unused type variable and it was only chosen to distinguish it from `foldl`'s more generic `a` and `b`.

With this new information, we can now revisit the original problem:

```haskell
complexPart :: [r] -> [r]
complexPart = foldl (flip (:)) []

scaryStatement :: [Int]
scaryStatement = complexPart [1..20]
```

All of the sudden `scaryStatement` doesn't look so scary anymore, does it? In fact, if we claim that `[1..20]` is of type `[Int]`, then we know that the type signature of `scaryStatement` is simply `[Int]`.

This is great. It took us 20 minutes to figure out how to use an undocumented function where it would've taken us about 2 minutes to read some Java doc. Well, in this case we were quite thorough (more thorough than the average Haskeller probably needs to be). Believe it or not, the more you use Haskell, the greater number of type signatures you remember in your head. Consequently, you start performing these operations quickly without thinking too hard about it.

Even so, this skill still seems to be a bit arbitrary. However, I argue that this vastly improves **readability** and **maintainability** of code. Since (in general) state in Haskell is *immutable*, reading the code will give you no surprises (read: it will behave exactly how its written ignorant to external computations). It's not possible for this method to be affected by unintended side-effects somewhere else which makes it far easier to detect misleading and stale comments. I have heard arguments that keeping type information in your head actually makes the mental model of programming more difficult. On the contrary, reasoning about multiple threads possibly mutating multiple states is far more difficult than reasoning about the code directly in front of you.

# Did someone already write this code I need?

Great question. Why don't you [Hoogle](https://www.haskell.org/hoogle/) it? Due to its type system, the Haskell ecosystem contains tools for finding the code you're looking: simply search by type. Earlier we determined our `complexPart` function above is simply the `reverse` function. You should never just believe anyone, so go [see for yourself](https://www.haskell.org/hoogle/?hoogle=%5Br%5D+-%3E+%5Br%5D) that a search for `[r] -> [r]` yields a result for `reverse`. Obviously types are not completely unique so you will need to choose the function that performs the correct logic. However, this saves a lot of time from trying to Google "Java library that does 'x'" and searching around for functionality.

# Will my code break?

You can rest assured that "pure" (i.e. [staying out of the IO monad](http://deathbytape.com/articles/2015/07/03/staying-out-io-monad.html) and avoiding ["unsafe" functions](http://hackage.haskell.org/package/base-4.8.1.0/docs/System-IO-Unsafe.html#v:unsafePerformIO)) code will not break unexpectedly.

> Notable exceptions are logical fallacies such as `head` on empty list or `div` by 0. That said, there are [solutions for that](https://en.wikibooks.org/wiki/Haskell/GADT).

As a consequence, testing Haskell code tends to be far less arduous (though, I admit, no less important) than in other languages. That is to say, you'll never see any unexpected input to your functions: period. If I have a function `blackbox :: Int -> Int`, blackbox will always behave the same. There is no worry that the input is of type `Float` or that it could ever be the case that no input would be passed in since it won't compile. You know very explicitly that an `Int` _always_ goes in and an `Int` _always_ comes out (again, this is checked at compile time). Having these guarantees allows you to vastly reduce the number of erroneous test cases on your function (though you should still test all edge and common cases of your function's logic).

Moreover, such guarantees simply improve your life as a developer. Not only do you have to write fewer boilerplate test cases, you never have to worry about the following case:

```java
public class MyClass {
  public static Integer incPositive(int x) {
    return (x < 0) ? null : x + 1;
  }
}
```

The caller of this code has to know by __convention__ that the `incPositive` method may or may not return a valid Integer back. If this subtlety slips the mind of the user (or is undocumented) then an NPE will likely be thrown and bad things happen to the user's application (quite possibly a full crash).

This concept is still expressible in Haskell, but there are special types to encode this information (such as `Either` or `Maybe`). For instance, a Haskell version of this function would look like:

```haskell
incPositive :: Int -> Maybe Int
incPositive x = if x < 0 then Nothing else (Just (x + 1))
```

As you can see, the caller of this function will be **required** to handle the `Maybe` type. This allows the developer to think about all of these common edge cases as they occur at time of writing rather than fixing bugs blowing up in production later.

# Conclusion

What's the take away from all of this? The "burden" and "complexity" that comes with Haskell is actually one of its greatest assets. The most common excuse I hear people use when resisting Haskell is that it's "too hard" and that programming in a functional way like this simply "isn't practical." As you can see, I disagree with this sentiment. Though I have only chosen a few examples here, I have had many instances in my day job asking myself "I wish I could use Haskell for this" for the exact opposite reasons: it's often easier (for some definition of easy, I suppose-- this is quite relative) and more practical.

I know what you're thinking: if Haskell really is this great, it's an obvious choice for use on most projects. Why don't more people use it? The full answer is somewhat complex (dealing with people, emotions, politics, etc.). In my humble opinion, however, the biggest problem facing us is that the Haskell ecosystem is simply [not mature enough](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md) in many areas. [There are many people](https://github.com/commercialhaskell) trying to rectify this situation. The truth is, however, the reason I cannot convince the guys at work to make the switch is that there are simply no big-data equivalents to Hadoop, Spark, Storm, etc. in the Haskell world. Recreating these systems takes a lot of work and the Haskell community is still just getting the appropriate building blocks in place. Though a great language, sometimes to get your product up and running in a timely manner you have to make suboptimal decisions.

Above all, the idea here is that you should not simply overlook Haskell for whatever reason. If you're going to solve a new challenge, you should investigate whether or not it is mature in that area (there _are_ many areas in which it _is_ mature). If so, it is likely a very good choice for you and your organization assuming you have engineers who are excited and willing to learn. You can see in this post that there are some very strong benefits from only a _single_ feature of the language that I have discussed which already saves us from the many pitfalls and time sinks of using other languages.
