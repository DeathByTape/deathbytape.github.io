---
layout: post
title: "Haskell through Stories: Functors"
excerpt: Learning Haskell should always be fun!
categories: articles
tags: [functors,haskell,functionalprogramming,fp,categorytheory]
comments: true
share: true
ads: true
redirect_from: ['/post/112411411944/haskell-through-stories-functors/', '/post/112411411944/']
---

<p>In my free time I have been playing with Haskell a lot recently. Seeing as this makes me an <a href="http://en.wikipedia.org/wiki/Imperative_programming" target="_blank">imperative programmer</a> by day in my day job and a <a href="http://en.wikipedia.org/wiki/Functional_programming" target="_blank">functional programmer</a> by night, I tend to rely on the interwebz and IRC for most of my questions. Upon stumbling around, I have noticed there is a lot of confusion on some fundamental concepts in Haskell. Even those who have read the <a href="http://learnyouahaskell.com/" target="_blank">best Haskell primer on the web</a> seem to have trouble distinguishing Functors, Monads, and Monoids and why each of them is useful. I remember approaching these for the first time and being similarly confused. Truth is, you must think of these concepts as well as your programs in a more mathematical way for any of this to really make sense (see: <a href="http://www.math.mcgill.ca/triples/Barr-Wells-ctcs.pdf" target="_blank">Category Theory for Computer Science</a>). Specifically, thinking of your program as bits of generic computation which you appropriately glue (read: compose) together seems to be a useful way of thinking. So first thing&rsquo;s first, put your math hat on (sorry to those of you who left it in your college dorm) and we&rsquo;ll try to dispel our confusion together.</p><h2>Functors</h2><p>In this post, we will start with <a href="http://en.wikipedia.org/wiki/Functor" target="_blank">functors</a>. To be honest, I don&rsquo;t see many people having much trouble with this one but since it is one of the most straightforward abstractions represented in Haskell, I figure we would work our way through it to setup a framework for attacking Monads and Monoids in future posts (stop cringing: you&rsquo;ll get it). Haskell defines the Functor type-class as follows:</p>

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

<p>Many people see this definition and say to themselves, &ldquo;oh, it&rsquo;s simply a generalization of map over some type <i>f</i>.&rdquo; If you clicked on the functors link and read the wikipedia page, you would notice that this justification is a little loaded. Though it must be verified by the implementor, a true <b>Functor </b>&ldquo;must preserve identity morphisms and composition of morphisms.&rdquo; What this statement means is that your functor <i>f</i> must support an identity operation (just like <em>X</em> x 1 is identity in multiplication over the reals) and it must also compose (i.e. f(g(x))). This is an awful lot of talking, so let&rsquo;s see it in action.</p><h2>Our Story</h2><p>Suppose you run your own software company and you employ three programmers and some other administrative staff. It&rsquo;s that time of year where you start thinking about raises, so you need a good metric to rank employees and give them raises. Times are tough, so you choose to ignore your administrative staff and only give raises to programmers which you feel have worked hard; you need to quantify this. After countless nights of thinking, you&rsquo;ve finally got it. Despite the advice you have read online about employee performance metrics, you think you have finally figured out how to determine employees line-of-code quality: <i>all programmers who wrote an even number lines of code will <b>not</b> receive a raise</i>. You are certain this is the right thing to do. After all, you did the math and any questions about it can wait until next year, right?</p><figure><img src="/images/raise-calc-fail.png" alt="image"/></figure><p>But time is money and to do this by yourself would be a real hardship, so we&rsquo;re going to show how we can use functors to our advantage and solve this problem using Haskell.</p><h2>How to do?</h2><p>Let&rsquo;s first show some code:</p>

```haskell
module Main where

data Employee a = Programmer String a | Other String deriving Show
type LoC = [String]
type Money = Int

instance Functor Employee where
  fmap f (Programmer n v) = Programmer n (f v)
  fmap _ (Other n) = Other n

-- Employees with lines of code
employees :: [Employee LoC]
employees =
  [Programmer "super1337" ["<?php"],
   Programmer "humbleServant" ["int bEndian()", "{", "union {", "int x;", "char c[4];", "} v = 0xdeadbeef;", "return c[0] == 0xde;", "}"],
   Programmer "n00blet" ["public static void main(String[] args) {", "System.out.println(\"woot\");", "}"],
   Other "reallyImportantSecrataryWhoTakesCareOfPrettyMuchEverythingNotSoftware",
   Other "Jim"
   ]

calculateRaises :: [Employee LoC] -> [Employee Money]
calculateRaises = fmap (fmap convert)
  where convert :: LoC -> Money
        convert xs = let len = length xs in
          if len `mod` 2 == 0 then 0 else len * 2
```

<p>The first thing we should do is see if our instance of Functor is <i>actually</i> a functor. To prove to you that it is, first try running:</p>

```haskell
let x = Programmer "test" ["a", "b"]
fmap id x
-- Returns: Programmer "test" ["a","b"]
</pre><p>This shows us that our identity property holds. Similarly, we should ensure that our functor composes. More precisely, we should show that fmap'ing a composed function or composing two partially applied fmap functions yields the same results (for the same functions, of course). For example,</p>
<pre class="brush: haskell">
let x = Programmer "test" ["a", "b"]
(fmap (++"!") . fmap head) x
fmap ((++"!") . head) x
-- Both return: Programmer "test" "a!"
```

<p>Great. It looks like we <i>actually</i> have an employee <b>functor</b>. Notice that we don&rsquo;t do anything special in our functor; it really simply acts as our <a href="https://wiki.haskell.org/Lifting#Lifting_in_general" target="_blank">lifting function</a> (i.e. take a typical function and apply it within the functor type). So our <b>calculateRaises</b> method actually works by applying the partially applied <b>fmap convert</b> to every element in the list (the outer fmap applies the function to each element in the list).</p><h2>What just happened?</h2><p>Let&rsquo;s take a look at this a little more closely.</p><figure><img src="/images/raise-calc-fail-result.png" alt="image"/></figure><p>Other than upsetting both your strongest programmer and your administrative staff, you quickly calculated the amount of money each person earns based on lines of code. Assuming you had some model setup, the only <i>real</i> work here was your business logic (i.e. the <b>convert</b> function) which converted lines of code into money according to the devised scheme.</p><p><b>fmap </b>(and, as a result, Functor instances) was the magic that really made this possible for us. We applied our <b>convert</b> function to a list of Employees by &ldquo;lifting&rdquo; our function to the correct type. This allowed us to use our simple and more general convert function instead of using a more specific <b>employeeConvert</b> function which strictly dealt with converting LoC per Employee into Employee&rsquo;s Money. The distinction seems subtle, but with large programs this makes your computations incredibly reusable.</p><p>Anyway, let&rsquo;s take a look at that substitution:</p>

```haskell
fmap :: (a -> b) -> [a] -> [b] -- List's fmap
fmap' :: (a -> b) -> Employee a -> Employee b -- Employee's fmap
convert :: LoC -> Money -- Our logic function

(fmap' convert) :: Employee LoC -> Employee Money -- First sub
(fmap (fmap' convert)) :: [Employee LoC] -> [Employee Money] -- Full sub
```

<p>If you follow the type substitutions (I disambiguated fmap calls with a &rsquo; symbol), you can see how we actually built up this function. The reason this is useful is that it makes it easy for us to think about particular logical units aside from decoration of the context in which its used. What I mean is that&ndash; as the boss&ndash; you can focus on turning lines of code into money rather than dealing with employee data types, etc.</p><p>So now that you have effectively upset your whole office dynamic, you can now share and reuse your <i>LoC -> Money</i> algorithm with the world (and people don&rsquo;t need to have or use your Employee data type!). Or, on the other hand, you can use it in other areas of your business. For instance, when you want to forecast your revenue for the year based on the value you&rsquo;re going to be producing from your software. With that in mind, I do suggest you revise this model before actually applying it to the anywhere.</p><h2>Recap</h2><p>That was a lot to look at, so what did we learn? First of all, we learned that when you arbitrarily apply mathematical definitions, you typically produce a model which has no correlation to the real world. Also, if you don&rsquo;t give raises to your staff members who really deserve them, they get angry.</p><p>On the other hand, we took a look at what a <i>functor</i> <b>is</b> as well as what it <b>does</b> for us in the context of Haskell. We saw that it provided us power to increase code reuse and ignore implementation details of irrelevant types. In this way we can write functions which describe our important business logic and then simply apply them to whatever relevant types make sense later through substitution. This is an incredibly useful tool to have in our functional tool belt, but it is only one of many. Later on we will continue to discuss monads, monoids, and arrows. Until then, keep working on your compensation models!</p>
