---
layout: post
title: An Introduction to Using Hoogle
excerpt: This was certainly not obvious to me when I was a beginner.
categories: articles
tags: [haskell,types,practical,programming,hoogle]
comments: true
share: true
ads: true
---

I am going to discuss a very brief introduction to using [Hoogle](https://www.haskell.org/hoogle). As a new Haskell programmer, this tool will *dramatically* improve your ability to quickly and easily write Haskell code while simultaneously improving the quality of your life. Let's get to it!

> **NOTE**: This is really intended as the TL;DR for the [official Hoogle guide](http://www.haskell.org/haskellwiki/Hoogle) so newbies can get started right away!

# What is Hoogle?

Hoogle is a search engine for Haskell types. Anything that is in [Hackage](http://hackage.haskell.org) (i.e. the de-facto Haskell package repository) is quickly and easily searchable. This sounds great, right? Well, it is. But first, you have to understand how to use this system effectively to put it to good use.

> **NOTE:** It is important to realize that this article-- though dealing directly with Hoogle-- also applies to Hoogle-like systems that use different package repositories. Most notably, [LTS Haskell](https://www.stackage.org/lts-3.10) and [Hayoo](http://hayoo.fh-wedel.de/).

# Searching By Name

The most straight-forward way of using Hoogle is to search by name. Perhaps you forgot the type constructors for the [Maybe](https://www.haskell.org/hoogle/?hoogle=Maybe) type. Then you would-- rather intuitively-- type "Maybe" into the search box. The first result yields `data Maybe` and if you were to follow this pointer, you would be immediately brought to the haddock of the data type which contains its constructors (i.e. `Just a` and `Nothing` if you were curious).

Similarly, this works well for functions. If you were trying to remember the type signature for `fmap`, again you would type "fmap" into Hoogle. The search would immediately yield the type signature of this function and you could happily be on your way to programming.

# Searching By Signature

If you just needed the type signature for fmap, you could just as easily Google "fmap haskell" right there in your browser and likely be presented with the correct resource; so what is the actual benefit of Hoogle? Well, a far more common use case for Hoogle (in my humble experience) is to search for what you don't already know.

Let's say we're happily chugging along with the code below:

```haskell
module Main where

ignoreSecond :: a -> b -> a
ignoreSecond x y = x

main :: IO ()
main = do
  print $ ignoreSecond 1 2
  print $ ignoreSecond 3 2
```

This example may seem a little contrived, but that function signature does look a little useful at least, right? `a -> b -> a` looks pretty generic. Well, if we were to go to Hoogle and search for it, we would instantly see a result for `const`. This is the exact function we're looking for. Everywhere you see `ignoreSecond` could be replaced with `const` which would result in a few benefits:

  1. You learned about a new function you won't have to write again.
  1. You can replace your custom code with maintained library code (this is simply good for reducing the amount of code you personally must maintain).

Yay! We found a new function and life is good. But, I must admit, the example is a little bit contrived. Many times you will be able to write the **exact** signature you're looking for (as we did here), but often times, your code may be more specific than the fully generalized case. Let's observe the following example:

```haskell
module Main where

-- NOTE: _ is the hole for the function we don't yet know.
printList :: (Show a) => [a] -> IO [()]
printList = _ . printList'
  where printList' :: (Show a) => [a] -> [IO ()]
        printList' = fmap (putStrLn . show)

main :: IO ()
main = do
  printList [1..10]
  printList ['a'..'c']
  return ()
```

Now, this is a common problem. We have a list of `Show`-able objects and we want to print the contents of each element (one per line in this case). In summary, we know we want to use `fmap` to apply our composed function over the list (i.e. `putStrLn . show`) and we know this will produce a type of `[IO ()]`. However, since we're using this in the `IO` monad, having the `IO` inside of the list doesn't quite help us use this function directly. At this point, we know that we need to somehow transform our list of `IO ()` to a list of `()` in the `IO` monad.

> **BONUS:** If you try to compile this program at this point, the `hole` I mentioned in the comment will actually show up in the compiler log. In particular, it will show you the type you're looking for (i.e. `[IO ()] -> IO [()]`).
> In this case it is quite simple to reason about the type we need, however, in more complex situations the compiler can help you figure out which functions to look for.
> For more information, see [hole-driven Haskell](http://matthew.brecknell.net/post/hole-driven-haskell/)

In short, we need one more function with the type signature `[IO ()] -> IO [()]` to complete our `printList` function. Realistically, we have two options: (a) write this function ourselves or (b) use someone else's function already written to do this. Naturally, you should almost always gravitate toward option **b** and if you find it insufficient (or it doesn't exist), then you fall-back on option **a**.

Anyway, if we [search for our type in Hoogle](https://www.haskell.org/hoogle/?hoogle=%5BIO+%28%29%5D+-%3E+IO+%5B%28%29%5D) we will certainly come up with a method called `sequence` with the signature `(Monad m) => [m a] -> m [a]`. If you think about this signature for a moment, you will realize that `IO` satisfies the constraint of `Monad m` and `()` is a valid substitution for `a`. That is, this is simply the generalized form of the method we were looking for. This finally brings us to the following:

```haskell
module Main where

printList :: (Show a) => [a] -> IO [()]
printList = sequence . printList'
  where printList' :: (Show a) => [a] -> [IO ()]
        printList' = fmap (putStrLn . show)

main :: IO ()
main = do
  printList [1..10]
  printList ['a'..'c']
  return ()
```

> **NOTE:** There is an analog to `sequence` known as `sequence_` that has the signature `(Monad m) => [m a] -> m ()` if you don't actually use the return type. For instance, if we did this in our case then we could also eliminate the final `return ()` in `main`.

If you were to run this code, you would quickly see that the result is properly printed strings for the values: one per line.

# Conclusion

[Hoogle](https://www.haskell.org/hoogle) is an incredibly useful tool to any Haskell programmer. Particularly, it speeds up programming by providing features for incredibly fast and efficient lookup of existing Haskell code. This allows programmers to reduce the number of redundant code they must write and allows them to focus on the problem they're trying to solve (rather than each individual sub-problem that comes with it). This vastly improves productivity over other languages where such a system does not exist.

Happy Hoogl'ing!
