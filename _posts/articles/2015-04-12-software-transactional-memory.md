---
layout: post
title: Software Transactional Memory
excerpt: STM is an incredibly powerful construct that changes the way you concurrency.
categories: articles
tags: [haskell,stm,software transactional memory,transactional memory,concurrency,parallel programming,software]
comments: true
share: true
ads: true
redirect_from: ['/post/116157542099/software-transactional-memory/', '/post/116157542099/']
---

<p>Software Transactional Memory (or STM) is a useful concept for constructing concurrent software. The <a href="http://groups.csail.mit.edu/tds/papers/Shavit/ShavitTouitou-podc95.pdf" target="_blank">original proposal</a> of STM by Shavit and Touitou was derived by adapting a <a href="http://www.cs.utexas.edu/~pingali/CS395T/2009fa/lectures/herlihy93transactional.pdf" target="_blank">hardware-based transactional memory system</a> which aimed to make for conceptually simpler and safer concurrent code. Since that time, STM has come a long way being used to describe <a href="http://research.microsoft.com/pubs/67418/2005-ppopp-composable.pdf" target="_blank">composable concurrent computations</a> and other useful concepts. As you can tell, this has been a heavily researched topic in the literature. However, it seems that for the vast majority of people, <i>locking</i> and raw concurrency are still the norm when developing a parallel system. Through some explanation and examples, Ill help you understand why STM is really useful and how it can make complex systems more robust and performant.</p><blockquote><div><b>NOTE</b>: Though there are several existing implementations of STM, we will focus on using the GHC Haskell compilers implementation of STM for our examples in this post. As a result, I assume some <i>familiarity</i> in working with monads though you dont need to be an expert with them.<br/><br/>Similarly, I expect familiarity with common locking techniques and problems associated with them.</div></blockquote><h2>Why Software Transactional Memory?</h2><p>The first question we always tend to ask is <i>why</i>? Particularly, <i>why</i> should you spend the time to learn a new concept when you already understand how to use mutexes and semaphores to control execution of your application? The short answer is that, in general, managing concurrency is <i>difficult</i>. Not only is it difficult, but its extremely <i>important</i> for any modern software system. If you notice the latest line of computers being manufactured, the speeds of the cores are staying the same but the number of them is growing. This means that you need to use many slower cores more effectively rather than a single fast core.</p><p>But just because something is difficult doesnt mean we dont try. If you look at the linux kernel or many other stable software solutions in existence today <i>many</i> of them use locks. That said, concurrency bugs are being fixed all of the time in these systems. Why is that? When I say concurrency is difficult, I mean its <i>really difficult</i>. Whats more, something like the linux kernel is a <b>large</b> system. Arguably some of the best developers in the world (i.e. linux kernel devs/maintainers) find issues with their subsystems on an almost daily basis. When dealing with locks you have problems such as <b>deadlock</b> (i.e. everything freezes waiting on resources) and what I will call <b>over-serialization</b> of your system (i.e. your threads start running serially due to overlocking to ensure data integrity). These problems ignore the fact that its also an error-prone process. That is, if you forget to lock a particular critical section, memory corruption can occur and will become incredibly difficult to track down and debug.</p><p>The root cause of this difficulty is in the size of the system. For small, trivial systems, it is possible to create a locking solution which is likely faster than STM. However, scaling this system into something more complex (as we see with the linux kernel) becomes exponentially more difficult and will likely result in a suboptimal implementation.</p><p>As you may have guessed, STM solves many of these issues. Though there is <a href="http://channel9.msdn.com/Shows/Going+Deep/Programming-in-the-Age-of-Concurrency-Software-Transactional-Memory" target="_blank">overhead associated with STM</a> (see around 11:00min), it can likely be ignored as we scale to larger systems. Similarly, this scaling of systems is likely inevitable.</p><h2>How does it work?</h2><p>Now that Ive convinced you that STM is a worthwhile concept in modern software engineering, how does it work? Well, if you know anything about databases and transactions, its probably going to feel very intuitive to you. If not, dont worry: pictures follow.</p><p>But first, a well go through a quick overview of STM. The first thing we must be aware of is that any <i>proper</i> implementation of STM has the following two properties:</p><ul><li><b>Atomic.</b> That is to say, grouped sets of operations executed on memory occur together or not at all. For more information on atomicity, see <a href="http://en.wikipedia.org/wiki/Atomicity_%28database_systems%29" target="_blank">this wikipedia article</a>.<br/></li><li><b>Serializable.</b> Practically, this means that the current state of our data structure can be reconstructed by simply replaying our transaction log in sequential order from start to end.</li></ul><p>These properties are general to all STM implementations. That said, we will be using the implementation presented in the <a href="http://groups.csail.mit.edu/tds/papers/Shavit/ShavitTouitou-podc95.pdf" target="_blank">original paper I linked to above</a> as the basis for our brief high-level sketch to explore how software transactional memory works:</p><ol><li>Initiate a <i>transaction</i> to occur on a dataset<br/></li><li>Acquire <i>ownership</i> of affected data <i>addresses </i>on specified dataset</li><li>If successful, log the old values, compute new values, then update dataset</li><li>Otherwise fail transaction and help existing owner of address complete</li></ol><p>For deeper details and insights, I suggest you read that paper. However, I think this provides us a foundation (albeit, a little hand-wavy) to begin our discussion of how this works (and whats so cool about it).</p><p>Lets first consider the diagram below:</p><p style="text-align:center"><img src="/images/multithread-disjoint-access.png" alt="image" data-orig-width="293" data-orig-height="210"/></p><p>This is the simplest use-case of STM. In summary, we have a single <b>shared</b> dataset between two threads. What abstraction we use to interact with our dataset (i.e. array, binary tree, hash map, etc.) is not very important. What is important is the fact that this shared dataset (like everything in computing) is composed of a set of unique addresses. In this case thread 1 and thread 2 access disjoint subsets of these addresses and can make their updates concurrently.</p><p>Though this is a simple example, its implications are quite profound. In a typical locking scenario, one would have to lock on the <i>granularity of the entire dataset</i> and, thus, only one of these updates could occur at a time (making our application serial). Subtle yet powerful! </p><p>The next diagram shows an example of contention between our two threads:</p><p style="text-align:center"><img src="/images/multithread-concurrent-access.png" alt="image" data-orig-width="295" data-orig-height="359"/></p><p>This situation is a little more interesting. What will happen is first T1 will acquire ownership on the addresses <i>A1</i>. Then, T2 will attempt to gain ownership of the same addresses. However, since T1 already owns those addresses, it will fail. When it fails, it will try to help T1 complete its transaction sooner so that it can start. After T1 finishes its transaction (thus, releasing its ownership on <i>A1</i>), T2 can retry its transaction on data at <i>A1</i>.</p><p>As you can see, STM is a pretty nifty little concept. It avoids deadlock and complicated locking issues while allowing your program to be highly concurrent. As I noted before, I am hiding some important details like the fact that the authors rely on hardware primitives such as <a href="http://en.wikipedia.org/wiki/Load-link/store-conditional" target="_blank">Load_Link/Store_Conditional</a>. Even so, these details are not as important as obtaining the general high-level understanding of STM so that you can use it in your own applications. However, if youre interested in those nitty-gritty details, I will again suggest you read the original paper.</p><h2>Enough Talk. Show me how it can make my life easier!</h2><p>Youre right. Its an interesting idea, but without any practical application it doesnt seem so useful. For this section (as I noted above), I will be using Haskell with <a href="https://hackage.haskell.org/package/stm" target="_blank">GHCs implementation of STM</a>. Though the previous section should give you an <i>idea</i> of how this is implemented, you should not assume an identical translation (i.e. these come from different sources). Instead, you should be aware of the overall benefits and general concepts which <i>do</i> carry over and are not artifacts of implementation details. Lets begin.</p><p><b>The Banking Bandit.</b> Assume youre setting up a new bank account with a local bank. Similarly, suppose there is a bandit on the loose. This bandit keeps taking money from peoples bank accounts right as they make withdrawals and is causing $100²s in overdraft fees. As a protection measure, banks have now <b>stopped</b> allowing anyone to withdraw more money than exists in the account. If you havent guessed yet, were the bank charged with this task. Lets get right to the code:</p>

```haskell
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.State.Lazy
import Control.Monad.STM
import Data.Typeable
import System.IO
import System.Random

data BankException = InsufficientFunds deriving (Show, Typeable)
instance Exception BankException

type Account = TVar Int

prompt :: String -> IO ()
prompt str = putStr str >> hFlush stdout

main :: IO ()
main = do
  prompt "Initial account balance: "
  initial <- getInt
  if initial < 0 then
    putStrLn "Must start with a balance >= 0."
  else do
    acct <- atomically $ openAccount initial
    forkIO $ banditThread acct
    forever $ do
      prompt "Enter adjustment (positive for deposit, negative for withdrawal): "
      adjust <- getInt
      failed <- atomically $ adjustAccount acct adjust `catchSTM` handleBankException
      when failed $ putStrLn "Could not complete transaction: insufficient funds."
      showBalance acct
  where getInt = getLine >>= (\x -> return (read x :: Int))
        showBalance acct = do
          balance <- atomically $ accountBalance acct
          putStrLn $ "Current balance: " ++ (show balance)

openAccount :: Int -> STM Account
openAccount = newTVar

accountBalance :: Account -> STM Int
accountBalance = readTVar

adjustAccount :: Account -> Int -> STM Bool
adjustAccount acct adjustment = do
  curVal <- readTVar acct
  let newVal = curVal + adjustment
  writeTVar acct newVal
  if newVal < 0 then
    throwSTM InsufficientFunds -- Could alternatively retry here
  else
    return False

handleBankException :: BankException -> STM Bool
handleBankException _ = return True

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds = threadDelay . (*1000000)

banditThread :: Account -> IO ()
banditThread acct = do
  delayGen <- newStdGen
  takeGen <- newStdGen
  evalStateT beABandit (delayGen, takeGen)
  where
    beABandit :: StateT (StdGen, StdGen) IO ()
    beABandit = forever $ do
      (delayGen, takeGen) <- get
      nextDelayGen <- liftIO $ randomDelay (1, 5) delayGen
      (tval, nextTakeGen) <- liftIO $ randomTake takeGen
      put (nextDelayGen, nextTakeGen)
      failed <- liftIO $ atomically $ adjustAccount acct ((-1) * tval) `catchSTM` handleBankException
      liftIO $ when failed $ putStrLn "\nBandit tried to steal more money than is in account!"
    randomTake = return . randomR (1, 20)
    randomDelay intval gen = do
      (val, gen) <- return $ randomR intval gen
      threadDelaySeconds val
      return gen
```

<p>The first thing you will notice about this code is that it doesn&rsquo;t look much different than normal Haskell code. Similarly, Haskell&rsquo;s type system makes it very clear where STM is used. For us, this is great news! It demonstrates that using STM turns out to be minimally invasive when using it in our Haskell code and feels very natural. Now, rather than explain every little usage detail of the STM library in Haskell (read: <a href="https://hackage.haskell.org/package/stm" target="_blank">the docs are here</a> and <a href="https://www.fpcomplete.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory" target="_blank">a good primer is here</a>), I&rsquo;ll give a brief overview of what&rsquo;s going on.</p><p>In short, I&rsquo;ve set out to simulate the situation I have described above. We have a main thread which simulates a primitive bank terminal and a <em>banditThread</em> which randomly decreases the value of our account over time. The bandit will continue to draw as much money as the system will let him (but we&rsquo;ll never overdraw!). In the case of an overdraft by either you or the bandit, we throw an &ldquo;InsufficientFunds&rdquo; exception and abort the transaction entirely (i.e. the transaction is rolled back and not committed). To show how this works, you&rsquo;ll notice I wrote the transaction in an interesting way. That is, I write the TVar <em>before</em> I throw the exception. However, since these actions are all part of the same transaction and the <em>entire</em> transaction is rolled back, none of the actions take effect on our final state. Pretty neat, huh?</p><p>As you can see, writing STM code in Haskell is pretty straightforward. In fact, there is very little surprise or difference in how we use STM as opposed to any other monad. Above all, we never have to concern ourselves with potentially dangerous locking techniques that make us worry about deadlock or other race conditions. Whats more, the implementation is practical and accessible. We can simply and easily use it in our everyday applications with little extra effort other than modifying some types. Best of all, we get the safety of STM which provides us the ability to more rapidly build concurrent applications correctly!</p>
