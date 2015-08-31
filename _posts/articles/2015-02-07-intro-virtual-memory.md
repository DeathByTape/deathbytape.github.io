---
layout: post
title: An Introduction to Virtual Memory
excerpt: Virtual memory is why you can be so wasteful with your buffers!
categories: articles
tags: [virtual,memory,model,computer,architecture,programming,c]
comments: true
share: true
ads: true
redirect_from: ['/post/110371790629/intro-virtual-memory/', '/post/110371790629/']
---

<p>I proposed a topic not-so-long-ago when I was <a href="http://deathbytape.com/2015/02/01/pointer-refresher.html" target="_blank">discussing pointers</a>: virtual memory. <a href="http://en.wikipedia.org/wiki/Virtual_memory" target="_blank">Virtual memory</a> is probably one of the most powerful advances we&rsquo;ve seen in computer architecture over the years which has really allowed computing to grow so quickly. Yes, they continue to pack transistors on chips even more tightly, but the truth is that if we only had 4GB of RAM to simultaneously share with all processes on our computer, we could certainly run into some trouble. You can certainly forget about playing World of Warcraft while you chat over you vent server and browse the web for that guide to lead you through the raid.</p>
<h2>What is memory/RAM?</h2>
<p>In short, RAM (Random Access Memory) is fast-access storage for your programs. In computer architecture, the hierarchy of storage units (from fastest to slowest) is: registers, L1 cache, L2 cache, L3 cache (if you have one), RAM/memory, and then disk. The trade-off is that with speed comes high production cost and, consequently, less available storage. Simply put, the following relationships hold for our components:</p>
<ul><li><p>Speed: registers > L1 cache > L2 cache > L3 cache > RAM > disk</p></li>
<li><p>Space: registers < L1 cache < L2 cache < L3 cache < RAM < disk</p></li>
</ul><p>Not to mention, all the storage units I described are actually <strong>volatile</strong> (i.e. they lose the integrity of their contents after being switched off) except for disk.</p>
<blockquote>
<p><em>NOTE:</em> This is true for the common case. However, if you try really hard, you can recover the contents of RAM using a <a href="http://en.wikipedia.org/wiki/Cold_boot_attack" target="_blank">Cold-Boot attack</a>.</p>
</blockquote>
<p>The reason we focus on RAM as programmers is that it is the most abundant of the faster resources that we have available to us. Similarly, accessing RAM is typically much easier than accessing registers directly. Besides, unless you <em>really</em> know what you&rsquo;re doing, your compiler will probably make better use of your registers than you will.</p>
<h2>What does RAM look like?</h2>
<p>Basically, when processes are running, a physical RAM chip looks like this:</p>
<p style="text-align:center"><img src="/images/memory-fragmentation.png" alt="image" style="align:center;"/></p>
<p>In the diagram, we show the allocation of memory to specific processes <strong>P1, P2,</strong> <strong>P3, P4,</strong> and <strong>P5</strong>. In particular, if a space is marked <strong>Free</strong>, that means no process is using that space. There are a few obvious problems with this model.</p>
<p><strong>Heavily fragmented</strong>. This first thing you will notice is that this chip is heavily fragmented. What that means is that there are <em>holes</em> of free memory stuck between reserved memory sections. This makes allocation more difficult. Specifically, if you don&rsquo;t request the exact amount of memory for a free block, you will end up fragmenting that section of free memory (if you can fit) which just contributes to the overall problem. Otherwise, if there is no hole large enough to fit the amount of memory you need (even if total available memory is sufficient), you will be unable to allocate the requested memory. <a href="https://www.cac.cornell.edu/vw/gpu/coalesced.aspx" target="_blank">Coalescing</a> <em>is a thing</em> which can help mitigate this problem, but good luck trying to get arbitrary processes to cooperate properly to do this.</p>
<p><strong>No process isolation.</strong> All the processes currently running are co-resident in memory. For instance, if P1 tries to access too much memory, it could end up writing in the process space of P2, P3, etc. Similarly, it could also read the contents of their memory which is an obvious security flaw!</p>
<p><strong>We can&rsquo;t use the full address space!</strong> One of the most apparent issues with this scheme is that our total physical RAM is divided among <em>all</em> running processes (including the OS kernel). So, if I bought a computer with 4GB of RAM, I certainly couldn&rsquo;t write an application that requires 4GB of memory. What&rsquo;s more, this limitation is made worse when you factor in memory fragmentation. Let&rsquo;s all agree that this is a severe limitation.</p>
<p>Clearly, from a programmer&rsquo;s perspective, this is an awful lot to worry about. In general, our problem is that we just have some application state which we want to be able to access quickly.</p>
<h2>How it used to be done.</h2>
<p>Way back (further back than many of us reading this would probably remember), there really was raw access to RAM. Your application used physical pointer addresses to access physical locations on RAM. We have already discussed some of the issues with this model above.</p>
<p>Enter <a href="http://duartes.org/gustavo/blog/post/memory-translation-and-segmentation/" target="_blank">segmentation</a>. Segmentation splits a pointer into a segment and offset (for specific details, see referenced article). Segments still must fit into contiguous physical RAM, but whole segments can be swapped out to disk, now. The primary advantage of memory segmentation is that you can more effectively manage your memory space by swapping these segments. The obvious problem here is that fragmentation is still a problem (if you can&rsquo;t find a large-enough hole, you can&rsquo;t put the segment in memory) and you still can&rsquo;t use the whole address space with co-resident segments.</p>
<h2>Paging and Virtual Memory</h2>
<p><a href="http://en.wikipedia.org/wiki/Paging" target="_blank">Paging</a> provides us with a more improved solution to managing our memory space. In particular, it provides us the ability to swap at the granularity of &ldquo;pages&rdquo; (i.e. blocks of memory typically 4KB in size) from physical RAM to disk instead of segments. Moreover, pages are &ldquo;per-process&rdquo; which gives rise to page tables for a process managed by the <a href="http://en.wikipedia.org/wiki/Memory_management_unit" target="_blank">Memory Management Unit (MMU)</a>.</p>
<blockquote>
<p><em>Aside:</em> Swapping at the granularity of 4KB chunks eliminates the issues caused by fragmentation on physical RAM (since all the chunks are the same size). Segments typically fit a whole program into the contiguous slot.</p>
</blockquote>
<p>A page table is basically a large array which indexes into (potentially) other arrays until it gets to a value which represents an address on the physical RAM. It allows someone to find the physical base address of a page. The index used to search page tables comes from dissecting a <em>virtual address</em> (i.e. the pointer held in a process&rsquo;s memory space). After finding a particular page, the low bits of that address are typically used as a direct offset to where your data begins within that page.</p>
<p>The improvement made here over segmentation is exactly the abstraction that <em>page tables</em> provide. Now our memory space can be composed of multiple non-contiguous pages (i.e. 4KB chunks) instead of a single contiguous segment. However, with that flexibility, we do incur some overhead in the complexity of <em>virtual pointer address translation</em> and the fact that we now need to use extra RAM for each process to store our page table.</p>
<blockquote>
<p><em>NOTE:</em> We will be using 32-bit pointers for simplicity. See your CPU architecture&rsquo;s manual for how it&rsquo;s specifically done for your particular CPU arch.</p>
</blockquote>
<p>One of the best ways to describe how this changes things is to see below:</p>
<p style="text-align:center">
<img src="/images/virtual-address-translation.png" alt="image" style="align:center;"/></p>
<p>This is example is known as a a <em>linear page table</em>. You can see that our virtual address is translated to a physical address using a lookup in our page table and combining that with the appropriate offset. The dotted box is our small slice of memory we were looking for (though, really, you&rsquo;re looking up a single byte). Similarly, the color-coding represents a process that owns that particular region of memory. You will notice that a fragmented physical RAM chip no longer appears any differently to an application (i.e. contiguous virtual memory addresses can now be fragmented across multiple pages). This is a huge win from the standpoint of RAM utilization.</p>
<p>This diagram does have some subtlety to it, however. That is, it hides the <em>valid</em> bit (which we assume to be true in this case). The <em>valid</em> bit represents whether or not the page is resident in memory or on disk (1 if in memory, 0 if on disk) and it is actually stored along-side our physical page. If a page is not in memory, it must fetch it from disk and likely swap a different (unused) page from memory into disk in its place to make room.</p>
<p><strong>Calculating overhead.</strong> Earlier I mentioned that we incur overhead since we need to store our page tables for each process; this is true. To perform an example calculation, we will claim our page size is 4KB (i.e. the smallest unit available for swapping/addressing) and we have already established that we will be using 32-bit addresses.</p>
<p>Our first goal is to make sure that if we ever reach a page, we can also access anything contained in the page. In particular, this means that our offset must span from 0 to 4K for 4KB page sizes. More specifically, since we will treat our offset as unsigned, we need to reserve 2^12 bits (2^12 = 4K) of our virtual address simply for offsetting our location within a page. These bits are typically reserved at the bottom of the virtual address.</p>
<p>This leaves us 20 bits (32 bits - 12 bits = 20 bits) remaining for our page table index. What this means is that our page table has 2^20 entries each containing 21 bits of information (20 bits for physical page, 1 bit for valid). This means that the page table for each process takes up 2^20 * 21 bits ~= 2.75 MB ~= 3MB. Therefore, the overhead for each process is approximately 3MB per process. However, now your process can access a 4GB address space (2^20 entries * 2^12 B/entry = 2^32 bytes = 4GB) with proper paging. Seems like a small sacrifice for such a large gain in usable space.</p>
<p><strong>Virtual to Physical Address Translation.</strong> The other incurred cost is simply address translation complexity. We have dealt with <em>linear page tables</em> in this post, but <em>multi-level page tables</em> also exit. Multi-level tables have page tables indexing into other page tables where the final level of page tables works the same way as we saw here. Each time you add a level of indirection, there is a time cost on the hardware design perspective, but we are software people, right? So the actual complexity of finding where your data lives in physical RAM has now increased. The steps now required to access RAM look as follows (according to our example above):</p>
<ol><li><p>Use top 20 bits as index into page table to find physical address to page</p></li>
<li><p>Check if page is in memory, if so go to step 4</p></li>
<li><p>Otherwise, swap an unused page (i.e. least recently used) from memory and store it on disk. Load the page we want to use off disk and put in memory where the evicted page used to be. Update physical address in page table.</p></li>
<li><p>Add offset to physical address</p></li>
<li><p>Read byte from physical memory location</p></li>
</ol><p>In code, this looks something like this (NOTE: this is only for <em>illustrative</em> purposes only):</p>

```cpp
// Assume this is a packed structure
typedef struct _page_table {
  uint8_t valid:1;
  uint8_t base_hi:7;
  uint8_t base_mid;
  uint8_t base_low:5;
  uint8_t unused:3;
} page_table;

// Page table with 2^20 entries
static page_table pt[1048576];

uint32_t virt_to_phys(uint32_t vaddr) {
  uint32_t idx = vaddr >> 12; // Top 20 bits
  uint32_t offset = vaddr &amp; 0xfff; // Low 12 bits
  uint32_t base_addr = (pt[idx].base_hi << 25) // 7 bits
                     | (pt[idx].base_mid << 17) // 8 bits
                     | (pt[idx].base_low << 12); // 5 bits
  uint32_t phys_addr = base_addr | offset; // base + offset = phys
  return phys_addr;
}
```

<p>Before, all we had to do is this:</p>
<ol><li>Read byte from physical memory location</li>
</ol><p>You can already see the logical complexity added for a single level of indirection. Once you start doing <em>multi-level page tables</em>, this complexity goes up (in a 64-bit address space, multi-level is a practical necessity).</p>
<h2>I&rsquo;m a programmer. Who cares about this stuff?</h2>
<p>While this is very low stuff, it&rsquo;s incredibly important for developers. In particular, you want to be aware of your memory access patterns. Almost all CPUs nowadays (read: I can&rsquo;t think of any that don&rsquo;t, but don&rsquo;t quote me) have <a href="http://en.wikipedia.org/wiki/Prefetching" target="_blank">prefetchers</a>. The job of the prefetcher is to anticipate your next memory access and try to guess what data you&rsquo;re going to need next. They then go and either swap pages preemptively (if necessary) or pull entries into your L1 or L2 cache for faster lookup if they&rsquo;re already in memory. These optimizations can have dramatic effect on your code&rsquo;s runtime.</p>
<p>Typically, to take advantage of prefetching as a programmer you don&rsquo;t need to worry about anything. Write your code and you will likely see its advantages. On the other hand, if you start to see a lot of page faults while profiling your application, you should certainly be aware that you can (likely accidentally) write code which performs poorly with the prefetcher.</p>
<p>In general, the hardware is pretty good at picking up common patterns; this is why most of the time we don&rsquo;t actually need to do anything special to take advantage of our prefetcher. However, it can be terrible for long strides, accessing column-major data on a row-major arch (or vice versa), etc. For more information, I will defer <a href="http://www.futurechips.org/chip-design-for-all/prefetching.html" target="_blank">to this post</a>.</p>
<p>Above all, knowing about how the hardware you&rsquo;re working on actually operates is never a bad thing. If for nothing else than some personal edification, all programmers/software engineers/devs/code monkeys/any other title we want to give ourselves, should learn about computer architecture and how their machine operates.</p>
