---
layout: post
title: "Writing Multithreaded Applications in C++ (the right way)"
excerpt: "Concurrency is already difficult. Read about C++'s modern facilities to help you!"
categories: articles
tags: [c++,cpp,multithreading,threads,c++11,c++14,programming,software,engineering,futures,promises,async]
comments: true
share: true
ads: true
redirect_from: ['/post/110008612055/cpp-threading/', '/post/110008612055/']
---

<p>If you&rsquo;ve been writing software for more than 5 years or so in C++, you&rsquo;re almost certainly familiar with Win32 threads or pthreads. GUI programming aside, one of the most painful problems when writing cross-platform software in C++ has been threading properly. Since many systems use different underlying mechanisms for multithreading, your pretty source code quickly becomes muddled with all sorts of <i>#ifdef</i> preprocessor conditions defining behavior for each platform. After complaining about this for years, <b>C++11</b> finally did something amazing for us C++ devs; they introduced the construct of <b>std::thread</b><i>.</i></p><p>At this point, any C++ compiler worth using should implement std::thread. If your boss is forcing you to use <a href="http://www.bloodshed.net/devcpp.html" target="_blank">Dev-C++</a>, then the only advice I have for you is to simply quit your job right now. Seriously. Anyway, assuming you have a compiler that helps you rather than cripples you, the correct way to now write threaded software in C++ is to now use this gem of an API. The <a href="http://en.cppreference.com/w/cpp/thread/thread" target="_blank">std::thread interface</a> is very clean and easy to use which makes writing this post easier. That said, it would be uninteresting and monotonous if I just threw up a few code examples and said, &ldquo;have a nice day,&rdquo; so I will, of course, go into some discussion about threading in general.</p>

<h2>Wait a minute. What are threads?</h2>
<p>If you&rsquo;re asking this question, then this particular article may be a little fast for you. There are <a href="http://lmgtfy.com/?q=what+are+software+threads&amp;l=1#" target="_blank">many great resources</a> describing what threads are on the web and you should read them. In short, threads allow your software to run on multiple processors in parallel (i.e. at the exact same time). This comes with a plethora of problems and often makes reasoning about performance and behavior difficult if your design is not first carefully thought out. </p><p>If it isn&rsquo;t crystal-clear after reading that paragraph what threads are and why they are useful (which, if you&rsquo;ve never seen threads before, it shouldn&rsquo;t be), then I encourage you to scour the internet first to learn more about threads. After that, please continue on with this article!</p>

<h2>I Consume, You Produce. </h2>

<p>An incredibly common use-case for multithreading is the producer-consumer scenario. In short, one of your threads produces some values for use in computation while the other thread consumes that value and uses it in performing the computation. This a typical scenario and should be reasonably intuitive, so I figured we would introduce the C++ std::thread API with this example. For sake of example, I have contrived our scenario such that the consumer (i.e. main thread) also modifies the data which provides a reason to show how the std::mutex works.</p>

```cpp
/**
 * ProducerConsumer.cpp
 *
 * C++ std::thread API example using a producer consumer model.
 * Lock-based solution.
 *
 * Author: Dennis J. McWherter, Jr.
 */

#include <chrono>
#include <iostream>
#include <mutex>
#include <thread>

using namespace std;

static mutex theLock;

void produce(int* data) {
  for (int i = 1 ; i <= 5 ; ++i) {
    {
      lock_guard<mutex> lock(theLock);
      *data = i;
    }
    this_thread::sleep_for(chrono::milliseconds(500));
  }
}

int main() {
  int data = 0;
  int val = 0;

  // Start thread
  thread t(produce, &amp;data);

  // Loop through the data until
  do {
    {
      lock_guard<mutex> lock(theLock);
      if (data != 5) {
        data += 12;
      }
      val = data;
    }
    cout<< "My current value " << val << endl;
  } while (val != 5);

  // Cleanup
  t.join();

  return 0;
}
```

<p>In this example you will notice that I have introduced 3 thread-related constructs. <b>std::thread, std::mutex,</b> and <b>std::lock_guard</b>. A quick description of each is as follows:</p><ul><li><b><a href="http://en.cppreference.com/w/cpp/thread/thread" target="_blank">std::thread</a>.</b> This object representations a single thread of computation. <a href="http://en.cppreference.com/w/cpp/thread/mutex" target="_blank"><br/></a></li><li><b><a href="http://en.cppreference.com/w/cpp/thread/mutex" target="_blank">std::mutex</a>.</b> This object represents a single mutex. In particular, it is an object which allows access by a single thread into a specific critical section.</li><li><b><a href="http://en.cppreference.com/w/cpp/thread/lock_guard" target="_blank">std::lock_guard</a>.</b> The lock_guard object locks a mutex while in scope. After the object is out of scope, the mutex lock is released.</li></ul><p>Our producer simply increments our shared data object to the value 5. The main thread, on the other hand, continually increments the data object until its value is set to 5 by the producer thread. Overall, the actual logic is not very exciting but that&rsquo;s because this is really an example made for learning.</p><p>A little bit more exciting, however, is seeing how easy it is to create a thread. In particular, when initializing a std::thread object we simply provide a function for the thread to execute as well as an arguments to that function (in our case, a pointer to the &ldquo;data&rdquo; int). Moreover, we use a global mutex* to protect our data from multiple writers. Finally, we use { &hellip; } to create explicit scopes and use std::lock_guard to actually control access to the mutex.</p><p>This is certainly a vast improvement over having to know the details of pthreads, Win32 threads, or any other threading library your system may be using.</p><p><i>*NOTE: In C++, you would probably want to encapsulate your locks in an object rather than using global locks. In this case, we did not add objects for simplicity though it is not particularly good style. </i></p><h2>Two Tasks. One process. (I&rsquo;ll give you something in the Future. I Promise.).</h2><p>Suppose you&rsquo;re writing software which is embarrassingly parallel (the best kind!). That is, assume you have no need for coordination between threads and no shared data. Our contrived example will be that we want to compute a single function in parallel for two different initial values. In this case, it&rsquo;s best for us to use what is known as a &ldquo;Future&rdquo; for asynchronous computation. A future is simply an object which guarantees that a valid value will eventually exist based on the computation you requested (assuming your computation completes). A future is&ndash; more or less&ndash; a placeholder for a result. It allows you to start a computation, perform another task, then check back later and see if a result has been produced.</p><p>More specifically, futures are created through an asynchronous result provider. In C++, <b>std::promise, std::packaged_task,</b> and<b> std::async</b> are the interfaces for such providers which all implement a get_future() method that returns the corresponding future. A short summary of the differences between these providers is below:</p><ul><li><b><a href="http://en.cppreference.com/w/cpp/thread/promise/promise" target="_blank">std::promise</a>.</b> The most flexible way to provide a value for a future. Computation is performed independently from the promise object and the result is simply passed through the object to the future using the <a href="http://en.cppreference.com/w/cpp/thread/promise/set_value" target="_blank">set_value() method</a>.<a href="http://en.cppreference.com/w/cpp/thread/packaged_task/packaged_task" target="_blank"><br/></a></li><li><b><a href="http://en.cppreference.com/w/cpp/thread/packaged_task/packaged_task" target="_blank">std::packaged_task</a>. </b>The second most flexible way to provide a value for a future. The constructor takes a function and uses the return value of that function to set the value for the future. Since packaged_tasks must be <i>explicitly</i> invoked, they can be created and then assigned to be run on particular threads.</li><li><b><a href="http://en.cppreference.com/w/cpp/thread/async" target="_blank">std::async</a>.</b> A high-level utility method to provide a value for a future providing the least flexibility, but also the simplest way to execute an asynchronous computation. The <i>method</i> (note: std::async is not an object like the others, it is a function) takes a function and uses the return value of that function to set the value for the future. The primary distinction between <b>std::async</b> and <b>std::packaged_task</b> is that std::async automatically begins execution upon calling it. Additionally, the caller has no control over where the task is scheduled to run (including on the current thread). Finally, there has been <a href="https://connect.microsoft.com/VisualStudio/feedback/details/761209/boost-thread-specific-ptr-leaks-on-vs2012-when-using-std-async" target="_blank">some discussion</a> about uncertainty as to when std::async will release its resources.</li></ul><p>In our case, we will use <b>std::packaged_task</b> to get a future and we&rsquo;ll schedule the task to run on a <i>new</i> thread. The code is below.</p>

```cpp
/**
 * ParallelCompute.cpp
 *
 * Compute a function result in parallel.
 *
 * Author: Dennis J. McWherter, Jr.
 */

#include <chrono>
#include <iostream>
#include <future>
#include <thread>

using namespace std;

int myComputation(int x) {
  for (unsigned i = 0 ; i < 999999999 ; ++i) {
    x++;
  }
  return x;
}

int main() {
  // Create promises
  packaged_task<int(int)> task1(&amp;myComputation);
  packaged_task<int(int)> task2(&amp;myComputation);

  // Get futures
  future<int> val1 = task1.get_future();
  future<int> val2 = task2.get_future();

  // Schedule promises
  thread t1(move(task1), 0);
  thread t2(move(task2), 5);

  // Print status while we wait
  bool s1 = false, s2 = false;
  do {
    s1 = val1.wait_for(chrono::milliseconds(50)) == future_status::ready;
    s2 = val2.wait_for(chrono::milliseconds(50)) == future_status::ready;
    cout<< "Value 1 is " << (s1 ? "" : "not ") << "ready" << endl;
    cout<< "Value 2 is " << (s2 ? "" : "not ") << "ready" << endl;
    this_thread::sleep_for(chrono::milliseconds(300));
  } while (!s1 || !s2);

  // Cleanup threads-- we could obviously block and wait for our threads to finish if we don't want to print status.
  t1.join();
  t2.join();

  // Final result
  cout<< "Value 1: " << val1.get() << endl;
  cout<< "Value 2: " << val2.get() << endl;
  return 0;
}
```

<p>In the example here we create our packaged_task objects and then get the future values. After that, we schedule our tasks for computation using the same std::thread API as before. While those execute, we update our console in the main thread which reports status. Finally, we clean up and print the final results (i.e. the return value of the functions).</p><p>Above all, threading in C++ is neither scary nor arduous any longer. In fact, with the mass implementation of the C++11 and C++14 standards among popular compilers, life has vastly improved for developers. Though the threading API is merely one of those many improvements, it is quite significant given that single-threaded software in the modern day does not take full advantage of that hardware on which it executes.</p><p>As usual, if you have any questions, comments, concerns, suggestions, etc. leave a comment below and we&rsquo;ll address the issues!</p>
