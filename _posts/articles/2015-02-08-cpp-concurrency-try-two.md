---
layout: post
title: "Take 2: Threading in C++"
excerpt: An expansion on my first multithreading in C++ post.
categories: articles
tags: [threads,c++,producer,consumer,concurrency]
comments: true
share: true
redirect_from: /post/110477442574/cpp-concurrency-try-two
---

<p>The last post I wrote about <a href="http://deathbytape.com/articles/2015/02/03/cpp-threading.html" target="_blank">threading in C++</a> received some sharp criticism in certain circles. Trolling aside, many of these criticisms were valid. That said, the title itself may have been slightly misleading in the first place. The real goal of that post was&ndash; more or less&ndash; to introduce awareness of the updated STL (particularly related to threads). In any case, I would like to address some of the concerns raised in the last article. The examples are <i>poor</i> but they are simply demonstrative of using particular STL features. But rather than defend that particular post, I have decided to simply discuss some important concepts in multithreading here.</p><h2>Threads</h2><p>The most primitive (intra-process) and often most error-prone method of concurrency is to use threads. In short, threads allow a user to spawn parallel execution within a single process. Since the thread is spawned within the same process, they are incredibly light-weight because they share their memory space with the existing process. However, this shared state is also what tends to make threads tricky.</p><p>For instance, consider a simple increment of an integer. This is the typical &ldquo;read-modify-update&rdquo; problem. To increment an integer, it is actually three non-atomic operations. This means that when this is done in parallel, either of the threads could be operating on stale data. For this particular scenario, locks are a common solution. In general, you would like to avoid locks though (i.e. mutexes/semaphores) anywhere in your code. If you <b>must</b> use them, it&rsquo;s best to keep those critical sections as short as possible.</p><p><b>Why are locks bad?</b> The fundamental property of locks is that they block other threads of execution from entering a critical section. What this means is that when your threads are blocked on a mutex, it&rsquo;s like they&rsquo;re standing in line to checkout at the grocery store. What I mean is that if they&rsquo;re waiting, they&rsquo;re not doing any computation and you&rsquo;ve just turned your multi-threaded process (i.e. supposedly <i>parallel</i>) into a <i>serial</i> process. This is in addition to all of the potential errors that can occur with locks such as <b>deadlock, priority inversion,</b> and even <b>resource starvation</b>.</p><p><b>Let&rsquo;s define a problem.</b> The last time I wrote about threads, I received flak for a poor producer-consumer example. Consequently, this seems like a perfect time to use the same example (since anything <a href="http://en.wikipedia.org/wiki/Embarrassingly_parallel" target="_blank">embarrassingly parallel</a> is not actually interesting). This time, however, I will give it context. Suppose we are authoring a real-time audio processing application. Basically our <i>producer</i> will be the thread capturing audio while our <i>consumer</i> will be performing some processing on the captured signal.</p><p><b>Let&rsquo;s analyze the problem.</b> We know what we want to do; we want to perform some computation in real-time on a captured audio signal. Fundamentally, the producer is simply passing data to the consumer. The most obvious implementation of such a process is a shared queue of sample data. Likewise, we also know that <i>locking</i> to protect our data is bad and will inhibit our ability to process data in real-time. Enter <a href="https://www.chrisstucchio.com/blog/2013/actors_vs_futures.html" target="_blank">lock- and wait-free algorithms</a>. In summary:</p><ul><li><b>Lock-free</b> &ndash; The parallel algorithm guarantees global process (i.e. no deadlock), but individual thread starvation could still occur.<br/></li><li><b>Wait-free</b> &ndash; The parallel algorithm is guaranteed to complete in a finite number of steps. (i.e. lock-free without starvation).</li></ul><p>Lock-free and wait-free algorithms are actually <a href="http://blog.memsql.com/common-pitfalls-in-writing-lock-free-algorithms/" target="_blank">very complex</a>, but some of the fundamental research about <a href="http://cs.brown.edu/~mph/Herlihy91/p124-herlihy.pdf" target="_blank">universality</a> is actually quite interesting. But rather than go into all of the details here, I will cut to the chase with how we are going to implement our solution (if you&rsquo;re interested in a more thorough introduction, see <a href="http://preshing.com/20120612/an-introduction-to-lock-free-programming/" target="_blank">this post</a>). We can use a <i><b><a href="http://en.wikipedia.org/wiki/Circular_buffer" target="_blank">wait-free (FIFO) ring buffer</a></b></i>. Ring buffers are simply queues implemented as arrays that &ldquo;wrap-around&rdquo; (hence, ring or <i>circular</i> buffers) to fill data. This works well for us for a few reasons:</p><ol><li><b>We have only 1 producer, 1 consumer.</b> This means we can use a <i>wait-free</i> implementation (remember, this is a stronger guarantee than simply <i>lock-free</i>). Particularly, we will be using boost&rsquo;s <a href="http://www.boost.org/doc/libs/1_56_0/doc/html/boost/lockfree/spsc_queue.html" target="_blank">spsc_queue</a> (single-producer, single-consumer queue) implementation.</li><li><b>We&rsquo;re dealing with audio samples.</b> In our application, we are just processing some audio for a fun application (i.e. not a mission-critical embedded system). That said, if overflow is detected, it is not much of a problem to drop that data on the floor. That is, audio sampling is already a lossy process (i.e. discretizing a continuous wave of sound) and we specify that some loss in our case can be tolerated.</li></ol><p>At this point I think I have linked to enough references for you to figure out <i>how</i> the wait-free buffer is <a href="http://www.boost.org/doc/libs/1_56_0/doc/html/atomic/usage_examples.html#boost_atomic.usage_examples.example_ringbuffer" target="_blank">implemented</a> and I will skip the details as this is one of the more straight-forward lock-/wait-free data structures to reason about. The quick summary is that you have a head and tail pointer which index into your array that both only move in a single direction. As a result, you can always determine when the queue is full or empty based on the relative positions of the pointers. The only other note I will make is that both boost and C++11 have capabilities for support controlling <a href="http://en.wikipedia.org/wiki/Memory_barrier" target="_blank">memory barriers</a> which adds language support for enforcing memory ordering on certain operations (this is still lock-free).</p><p>Last time I really wanted to make use of and show the capabilities of pure C++11 when it comes to threading. However, rather than reinvent the wheel, I will be using boost&rsquo;s spsc_queue implementation which I linked to above. To remain consistent, I will simply use boost&rsquo;s facilities whenever possible. So let&rsquo;s look at the code:</p>

```cpp
/**
 * audioprocessing.cpp
 */
#include <boost/atomic.hpp>
#include <boost/chrono.hpp>
#include <boost/lockfree/spsc_queue.hpp>
#include <boost/thread.hpp>

#include <iostream>

#include <cassert>
#include <cstdlib>

const size_t sample_rate = 44100;
const size_t process_rate = sample_rate / 10;
boost::atomic_bool quit(false);

// Our shared ringbuffer state
// 2s worth of samples to buffer
boost::lockfree::spsc_queue<float, boost::lockfree::capacity<sample_rate*2> > buffer;

// Audio capture thread
struct AudioCapture {
  void operator()() {
    while (!quit) {
      // Sample 44.1KHz (Hz is 1/s)
      boost::this_thread::sleep_for(boost::chrono::seconds(1));
      for (size_t i = 0 ; i < sample_rate ; ++i) {
        // We aren't really doing audio capture...
        // Generate a fake signal with no imaginary components
        if (!buffer.push(0.5f * static_cast<float>(rand()))) {
          std::cout << "Overflow detected." << std::endl;
          // Let consumer catch up (i.e. throw away our "current" samples)
          boost::this_thread::sleep_for(boost::chrono::milliseconds(300));
          break;
        }
      }
    }
  }
};

// Audio processing thread
struct AudioProcess {
  void operator()() {
    // Give our producer a chance to capture some audio
    boost::this_thread::sleep_for(boost::chrono::seconds(1));
    while (!quit) {
      size_t ctr = 0;
      // We only need to process a certain number of samples each time (i.e. process_rate in number)
      for (size_t i = 0 ; i < process_rate &amp;&amp; ctr < process_rate ;) {
        // Only increment if we grabbed a sample-- we're only processing on the full stream
        // We don't care about underrun since we can only process as fast as we have
        // data
        if (buffer.pop(samples[i])) {
          i++;
        } else {
          ctr++; // If we haven't seen any data recently, we want to check if our producer died (i.e. should we keep running)
        }
      }
      if (ctr == process_rate) {
        std::cout << "Could not retrieve any samples in previous loop (checking if we should exit)..." << std::endl;
      } else if (ctr > 0) {
        std::cout << "Buffer underrun detected." << std::endl;
      }
      // NOTE: In practice you would want to move the result to yet another thread
      //   i.e. either the main thread or a dedicated UI thread to render the result
      //   of the computation in some way.
      process();
      std::cout << "Fake processing... samples[0] == " << samples[0] << std::endl;
    }
  }

  void process() {
    // Should have an FFT library and probably add a filter or do
    // something interesting with the signal here
    // Simulate 100ms of processing by sleeping
    boost::this_thread::sleep_for(boost::chrono::milliseconds(98));
  }

private:
  float samples[process_rate];
};

int main() {
  // Assure ourselves that our impl is really lock-free
  assert (buffer.is_lock_free() &amp;&amp; quit.is_lock_free());

  // Random sampling to generate some bizarre signal
  srand(time(NULL));

  // Spawn our producer
  AudioCapture p;
  boost::thread producer(p);

  // Spawn our consumer
  AudioProcess c;
  boost::thread consumer(c);

  // Consume the data until we receive input from the user
  char input;
  std::cin >> input;

  quit = true;

  producer.join();
  consumer.join();

  return 0;
}
```

<p>This code is mostly straight-forward with the comments filling in some blanks. Since all of the really difficult wait-free stuff is hidden from us by boost, the only real thing to explain here is the overall design. Basically, we startup our two threads (audio capture and processing) and allow the capture thread to begin buffering some data before we start our processing. Our processing thread then begins processing data. Notice that I have tuned our production and consumption rates with our sleep calls (in lieu of performing and estimating time for a real computation); this enables us to process continuously. Assuming these rates are similar, this is the true benefit of <i>wait-free</i> algorithms. In a real system, however, you may occasionally hit buffer underrun or overflow depending on the state and load of your device. This brings us to one of the problems with our wait-free approach. Since wait-free is always running, you cannot block <i>on data</i> if you have overflow or underrun. As a result, if the producer and consumer get out of sync you have three apparent options:</p><ol><li><b>Busy wait.</b> This is often the wrong option, but only you know your system. Similarly, for certain high-performance applications this may be the best option if at any time you know the waiting period will be small. To do this you can simply operate as normal and ignore that any failure has occurred. For instance, in the case of overflow you will keep trying to push data onto the queue and you will continually be wasting effort since your request will keep being rejected until the consumer consumes a sufficient amount.<br/></li><li><b>Block without data dependence.</b> Another solution is to temporarily block for a specified amount of time relative to the other thread&rsquo;s rate (i.e. producer waits for consumer and vice versa) which is the solution we have implemented here. Again, consider we detect an overflow. We can drop the current set of samples on the floor and simply allow the consumer a few milliseconds to catch up. To select an appropriate amount of time, you will want to consider how large your queue size is and how fast your consumer typically pulls data from it.</li><li><b>Overwrite existing data.</b> Depending on what data you want to drop on the floor, you can actually just overwrite the old data in favor of processing the new data. See <a href="http://stackoverflow.com/a/4405911/388182" target="_blank">this StackOverflow answer</a> with an idea on how to do it.</li></ol><p>In going with option two, our producer algorithm now depends on the consumer to do anything interesting (i.e. it must process samples). That said, it does still have the property that it will always complete in a finite amount of time (i.e. exit when told) even if the consumer stops consuming. However, note that sleep is a blocking operation which makes this approach somewhat unclean. More or less, the sleep acts like a bounded-wait lock which is calibrated for code executing on a particular device (think embedded device programming) which diminishes the <i>lock-free</i> quality of the entire algorithm even though our data structure is still wait-free. I implemented it this way primarily because I found it more interesting than the busy waiting case which is the obvious solution to restoring lock-free guarantees to our algorithm. You can see a simple example of lock-free usage of the spsc_queue <a href="https://github.com/boostorg/lockfree/blob/master/examples/spsc_queue.cpp" target="_blank">here</a>.</p><p>As you can see, <i>wait-free</i> algorithms still have an inherent cost. If you want to avoid excessive busy waiting you need <i>some</i> form of synchronization between threads to correct any issues if the threads can ever become out-of-sync. After running this example on my dual-core MBP, underflow only occurs at startup and overflow very rarely occurs. That said, we obviously lose a minimum 1.3 seconds worth of audio samples each time we detect overflow (1 second for the currently captured samples, 300ms to allow the consumer to catch up).</p><p>Another possible option is a hybrid. It is possible that in some systems, the right thing to do would be to block the producer on a condition variable that is signaled by the consumer when the size of the buffer reaches a particular value. However, the algorithm itself is no longer <i>lock-free</i> at this point even though the buffer still is.</p>
<blockquote><div><b>NOTE:</b> If this feels like a lot of work to move your data around, you may be right. I must admit that most audio processing I have done is actually implemented via callbacks scheduled on independent threads. In any case, I wanted to provide a semi-interesting use-case example to give ourselves a common task to solve here. Likewise, it is conceivable that you would want to use a similar technique to communicate results to your UI/drawing thread (say, for a real-time plot).</div></blockquote>
<p>Some final thoughts are that lock-free and wait-free solutions may not always be feasible. That is, a wait-free solution may very well not yet have been discovered (if it exists) for your problem, and naive lock-free solutions can end-up being slower in practice than their lock-based counterparts. Similarly, lock-free algorithms are typically more complex and, depending on your system, can lead to detrimental starvation depending on data access patterns (see <a href="http://www.drdobbs.com/lock-free-data-structures/184401865" target="_blank">this post</a>).</p><h2>Where do I go from here?</h2><p>The example I have presented is a very specific case study on how you can take data and concurrently operate on it. As you could guess, there are plenty of problems that lend themselves well to using threads that are, in fact, quite different than the scenario which I have just described. If you are such a curious reader, I point your attention to explore <a href="http://en.wikipedia.org/wiki/Concurrency_pattern" target="_blank">concurrency patterns</a>. Just like there are software design patterns for construction, such patterns also exist for trying to use threads properly based on your problem.</p><p>I just want to close with drawing your attention to the fact that this post deals <b>solely</b> with threads. I plan to discuss other concepts more in-depth in the future such as Futures and Promises, Actors (i.e. everyone has separate state updated via message passing), and perhaps even Software Transactional Memory (STM). Since concurrency is such an important piece of computing nowadays, all developers should at least be <i>aware</i> of all the different techniques for concurrency available. Similarly, we all should understand the distinctions and trade-offs between them so we can decide which approach is best fit for our particular problem.</p>
