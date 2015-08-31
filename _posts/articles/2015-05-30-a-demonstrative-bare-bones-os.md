---
layout: post
title: A Demonstrative Bare-Bones OS
excerpt: PSA-style post.
categories: articles
tags: [kernel,c,programming,os,operating,operating system,from scratch]
comments: true
share: true
ads: true
redirect_from: ['/post/120288085689/a-demonstrative-bare-bones-os/', '/post/120288085689/']
---

<p>Today I found a piece of code I wrote (on my own time) for a presentation I gave while at my previous job. In summary, it is a quick (i.e. &lt;1 week worth of work) example of writing a bare-bones OS from scratch. You can see the <a href="https://github.com/DeathByTape/HelloWorldOS.git" target="_blank">source code here</a>.</p><p>Now, this isnt the most stellar piece of code anyone has written, but I think it may be useful to some people looking for more information on how to even start. In short, you will want to look at <i>boot.s</i> to see how we actually start the boot process (i.e. after the bootloader) and <i>kernel.c</i> to see how we initialize and what happens in the main kernel loop.</p><p>Though I believe it is a nice tool for learning and exploration, I do not suggest you build your next major OS from this codebase. Instead, I suggest you take the apparent concepts and apply them in a more organized fashion. In general, this is probably a better starting point than the full Linux kernel because&ndash; if for no other reason&ndash; it is a small piece of code. Everything should be reasonably understandable (depending on your knowledge of C and x86 assembly). The more important concept to realize is that when doing kernel programming, datasheets matter. You will notice that I have a few Intel manual references in there; we do indeed care at this level how to manipulate the processor directly and modify its state.</p><p>All that being said, this about wraps up this short PSA-style post. I just wanted to make people aware that I was releasing this source that I have previously used a a short teaching aid. Happy kernel hacking!</p>
