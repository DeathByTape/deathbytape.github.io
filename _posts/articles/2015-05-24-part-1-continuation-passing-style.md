---
layout: post
title: "Part 1: Introduction to Continuation Passing Style"
excerpt: First part of an explanation on CPS.
categories: articles
tags: [haskell,continuation passing style,cps,functional,programming]
comments: true
share: true
redirect_from: /post/119782045004/part-1-continuation-passing-style
---

<p>Continuation Passing Style (CPS) is a useful concept in functional programming. In summary, the goal is to <i>chain computation</i>. Rather than explicitly handling intermediate results at a higher-level, CPS functions should instead pass their result into the next function in the chain of computation. Well see more on how this works below.</p><h2>What are <i>continuations</i>?</h2><p>Practically speaking, continuations are <i>functions</i> which take the <b>return value</b> of another function. That is, rather than returning, a function using a continuation passes its result into the continuation instead of returning thus <i>continuing</i> the computation. Given the power of most functional languages, this result (or argument to the continuation) can contain its own state and be arbitrarily complex. However, from the perspective of <i>using</i> a continuation, it can be treated as a simple function.</p><h2>Why Continuation Passing Style?</h2><p>Continuation passing style provides several benefits to the programmer. Such benefits include graceful exiting from a computation (i.e. think transaction-like), asynchronous operations, and more. In general, like all other aspects in programming paradigms, its a way of thought meant to make your life easier in particular scenarios. Well try to outline some examples of where it could be useful to help you identify when you want to think in this way.</p><p>Note that while we will use Haskell here, this can be done in any language which supports closures and passing functions as arguments. You can see a <a href="http://matt.might.net/articles/by-example-continuation-passing-style/" target="_blank">nice primer using Javascript</a> which goes through many simpler examples.</p><h2>Continuation Passing Style Examples</h2><p><b>Babys first CPS</b>. This first example is trivial. It is to simply provide you with some foundation or notion of what CPS is and how it looks on a very simple level. The following examples will be increasingly more complex but also far more useful.</p>

```haskell
module Test where

factorial :: (Ord a, Num a) => a -> a
factorial n = factorial' n 1
    where factorial' n acc
            | n <= 0 = acc
            | otherwise = factorial' (n - 1) (n * acc)

factorialCPS :: (Ord a, Num a) => (a -> a) -> a -> a
factorialCPS k n = continuation n 1
    where continuation n acc
            | n <= 0 = k $ acc
            | otherwise = continuation (n - 1) (n * acc)

main :: IO ()
main = do
  -- Basic call
  print $ factorial 5
  print $ factorialCPS id 5

  -- Double result
  print $ ((factorial 5) * 2)
  print $ factorialCPS (*2) 5

  -- Over many values
  print $ fmap ((*2) . factorial) [1..10]
  print $ fmap (factorialCPS (*2)) [1..10]

  -- Composed continuation
  print $ fmap ((+10) . (*2) . factorial) [1..10]
  print $ fmap (factorialCPS ((+10) . (*2))) [1..10]
```

<p>As I mentioned, this example is contrived solely for demonstration purposes. The first thing you will notice is that we have implemented the factorial method in two different ways: (1) our standard factorial method and (2) a factorial method which passes each result into a continuation (the variable letter k is frequently used for continuations). You can run the program and the results will be identical for either way of performing factorial.</p><p>Some final thoughts about this example: you will notice that our factorialCPS method never returns a value. Instead, it returns the result of a computation (i.e. k $ acc). The final continuation k will transform its input and eventually return that value as a result. Similarly, you will notice that our factorialCPS is still recursive and calls itself (rather, its continuation method). It is important to remember that while all final calls should be the final value returned from the continuation, you still have to complete your operation before you can pass your result to the continuation. Consequently, for any algorithm which is multiple steps (i.e. most useful algorithms), it is likely that you will recurse and only use the continuation at the base cases.</p><h2>Exception Handling with Continuation Passing Style</h2><p>In this next example, Ill show an artificial example of using CPS for exception handling. In our example we assume an exception occurs anytime our computation reaches 0.</p>

```haskell
module TryCatch where

type Result = Either String String
type Except a = (a -> Result)
type Cont a = (a -> Result)

exceptionCatch :: (Show a) => Except a
exceptionCatch v = Left $ "Error: Found bad value (" ++ show v ++ ")"

tryIt :: (Num a, Show a) => a -> (Except a -> Cont a) -> Except a -> Result
tryIt v try catch = try catch v

predOp :: (Num a, Show a) => a -> a -> (a -> a -> a) -> (a -> Bool) -> Except a -> Cont a -> Result
predOp q r op p f k = let res = q `op` r in (if p res then k else f) res

addPred :: (Num a, Show a) => a -> a -> (a -> Bool) -> Except a -> Cont a -> Result
addPred q r p f k = predOp q r (+) p f k

mulPred :: (Num a, Show a) => a -> a -> (a -> Bool) -> Except a -> Cont a -> Result
mulPred q r p f k = predOp q r (*) p f k

add :: (Num a, Show a, Eq a) => Except a -> Cont a -> a -> a -> Result
add f k n m = addPred n m (/=0) f k

mul :: (Num a, Show a, Eq a) => Except a -> Cont a -> a -> a -> Result
mul f k n m = mulPred n m (/=0) f k

stringCont :: (Show a) => a -> Result
stringCont = Right . show

opSequence :: Int -> Int -> Except Int -> Int -> Result
opSequence a b f scale = add f (mul f stringCont scale) a b

main :: IO ()
main = do
  let res = [tryIt 1 (opSequence 1 2) exceptionCatch
            , tryIt 2 (opSequence 1 2) exceptionCatch
            , tryIt 0 (opSequence 1 2) exceptionCatch
            , tryIt 1 (opSequence 1 (-1)) exceptionCatch]
  putStrLn $ show res
```

<p>You will notice that we define all of our operations in continuation passing style. This is necessary for all operations. Our final operation (stringCont) is our terminal operation. That is, it is the final operation that executes upon a successful computation.</p><p>In summary, our tryIt function takes a single value to be passed into our function, our actual continuation, and, finally, our exception handler (i.e. the failure case). Focusing on the opSequence method, you will notice that results are computed from the outside-in (opposite of typical function composition in Haskell). That is because we compute a result and then pass it into the next function.</p><h2>Conclusion</h2><p>This ends the first part of our discussion on Continuation Passing Style. Now that you have seen it a bit, it provides us with a framework for discussing more interesting use cases in the future. That said, I admit that these cases&ndash; while useful for explanation of the technique&ndash; are relatively contrived. The examples provided are usually more easily handled in other ways. In any case, we will dive a little deeper next time. That is, we will discuss using CPS in a more useful fashion on different styles of problems.</p>
