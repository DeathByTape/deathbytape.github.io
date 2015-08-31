---
layout: post
title: C is not (necessarily) Faster than C++
excerpt:
categories: articles
tags: [c,c++,performance,efficiency]
comments: true
share: true
ads: true
redirect_from: ['/post/109612092960/c-vs-c-performance/', '/post/109612092960/']
---

<p>I&rsquo;ve recently read a few things online (i.e. stackoverflow questions, random forum posts, and observations on IRC) asking whether or not C is faster than C++. What is more alarming to me is that there is a surprisingly large number of people who say, &ldquo;yes! C is faster than C++ since it&rsquo;s lower to the hardware.&rdquo; Sorry to say (read: sorry I&rsquo;m not sorry&hellip;), but this is not exactly correct.</p><p>We&rsquo;ll first describe why this assumption doesn&rsquo;t make sense and then we&rsquo;ll go through a few examples. To begin, both C and C++ are compiled languages which compile <b>directly to machine code</b>. What this means is that after you compile a C program and a C++ program, their binaries are in the same machine language. They may look slightly different (i.e. C doesn&rsquo;t have use for vptr&rsquo;s), but above all, the binary will be a sequence of add&rsquo;s, mul&rsquo;s, div&rsquo;s, sub&rsquo;s, and branches. More importantly is that&ndash; in theory&ndash; looking at a piece of compiled C or C++ code <i>could</i> produce <b>exactly</b> the same resulting binary. I do admit that this scenario is highly unlikely as it is all very compiler dependent and proper C++ should be written differently than proper C, but it is theoretically possible. This is an important point because it immediately shows us that C++ does not actually have any inherent flaws which naturally make it slower than C. That being said, the differences here in the final binaries will come from the respective compilers and not from the languages themselves.</p><p>At this point you may be considering other compiled languages such as Java and <i>everyone</i> knows Java is less performant than C. My first rebuttal is that <b>performant</b> C is faster than Java. Poorly written C code can also be slow and can certainly be slower than Java if the Java programmer knew what he or she was doing. More technically, however, Java runs in the Java Virtual Machine (JVM). That is, it runs inside of another program. Moreover, it uses garbage collection for memory management. Those these language decisions are useful in many ways (i.e. easy cross-platform code, no worrying about memory management), they incur a certain amount of overhead. As a result, a carefully written C or C++ program can certainly be faster than Java. But more importantly, this reiterates what I mentioned above. Neither C nor C++ have these overhead constraints and are fully capable of producing low (no?)-overhead binaries. As you ponder more languages (i.e. python) consider in what environments they run in. If they are not running natively, they are incurring overhead somewhere.</p><hr><p>Now that we&rsquo;ve discussed why C and C++ should be equally as powerful (barring the effectiveness of their respective compilers), let us go through a few examples. For all of my examples I will use <i><b>gcc version 4.8.2</b></i> where <i>gcc</i> will be used for compiling C code and <i>g++</i> for C++ code. I will keep optimizations turned off (i.e. -O0 switch in gcc) since we are trying to look at the languages and not the ability of the compilers. Moreover, I will also use a similar command to get the average results:<br/></p>

```bash
echo 'Profiling'; (for i in `seq 10`; do (/usr/bin/time -f "time; %E" ./a.out) 2>&amp;1 | grep 'time;' | cut -d':' -f2; done) | awk 'BEGIN{s=0}{s+=$1}END{printf("%fs\n",s/10.0)}'
```

<h2>Calculating PI</h2>

<p>We will first explore a simple example to calculate the digits of PI. The reason this is a good first example is that it&rsquo;s not using any of the language libraries (other than printf()) and can be compiled in both languages. This will give us an idea of the computational capacity of both languages on the <b>exact same piece of code</b>. And without further ado, the code:</p>

```cpp
/**
 * calcPi.c
 *
 * Small program which estimates the value of PI
 *
 * Author: Dennis J. McWherter, Jr.
 */

#include <stdio.h>

#define MAX_ITER 50000000

// Formula: 4 * sum((-1)^(k+1) / (2*k -1))
// See: <a href="http://mathworld.wolfram.com/PiFormulas.html" target="_blank">http://mathworld.wolfram.com/PiFormulas.html</a>
double calcPI() {
  size_t k;
  double res = 0.0;
  double val;
  for (k = 0 ; k < MAX_ITER ; ++k) {
    val = 1.0 / ((2 * k) - 1);
    res += (k % 2) ? val : -val;
  }
  return (4.0 * res);
}

int main() {
  double pi = calcPI();
  printf("PI Estimation: %lg\n", pi);
  return 0;
}
```

<p>In short, the results look like this:</p>

<table><thead>
<tr>
<th style="text-align:left">Language</th>
<th style="text-align:left">Average Runtime (seconds)</th>
</tr>
</thead><tbody>
<tr>
<td>C</td>
<td>C++</td>
</tr>
<tr>
<td>0.92</td>
<td>0.92</td>
</tr>
</tbody></table>


<p>As you can see, in this case the runtimes are pretty much identical for the same piece of code. But I know what you&rsquo;re thinking. This isn&rsquo;t exactly representative of the real world anyone. Again, <i>everyone</i> knows that C programmers and C++ programmers write code differently. So let&rsquo;s move on.</p>

<h2>Inserting to arrays and vectors</h2>

<p>A common thing to do in any language is to operate data in arrays. Typically vectors are preferred over array in C++ and arrays are used in C. So let&rsquo;s take a look at the performance impact of this decision.

</p>

```cpp
/**
 * mallocTest.c
 */

#include <stdlib.h>

#define ARRAY_SIZE (512*1024*1024) // 512MB of data

int main() {
  size_t i;
  unsigned* data = malloc(ARRAY_SIZE);
  for (i = 0 ; i < (ARRAY_SIZE / sizeof(unsigned)) ; ++i) {
    data[i] = 1;
  }
  for (i = 0 ; i < (ARRAY_SIZE / sizeof(unsigned)) ; ++i) {
    data[i];
  }
  free(data);
  return 0;
}
```

In this program we simply allocate 512MB of RAM and assign values to the memory. It&rsquo;s simple but we demonstrate a common usage pattern. Now, onto the C++ version of this:

```cpp
/**
 * vectorTest.cpp
 */

#include <vector>

#define ARRAY_SIZE (512*1024*1024) // 512MB of data

using namespace std;

int main() {
  vector<unsigned> data;
	data.resize(ARRAY_SIZE / sizeof(unsigned), 1);
	for (size_t i = 0 ; i < ARRAY_SIZE / sizeof(unsigned) ; ++i) {
		data[i];
	}
  return 0;
}
```

This is a similar problem except we don&rsquo;t deal with array allocation directly. Instead, we simply use a std::vector. But, for a fairer comparison (we know std::vector is for convenience and has a little overhead), we also should check a C++ version using &ldquo;new&rdquo; (i.e. the C++ analog to malloc()).

```cpp
/**
 * newTest.cpp
 */

#include <iostream>

#define ARRAY_SIZE (512*1024*1024) // 512MB of data

int main() {
	unsigned* data = new unsigned[ARRAY_SIZE / sizeof(unsigned)];
	for (size_t i = 0 ; i < ARRAY_SIZE / sizeof(unsigned) ; ++i) {
		data[i] = 1;
	}
	for (size_t i = 0 ; i < ARRAY_SIZE / sizeof(unsigned) ; ++i) {
		data[i];
	}
	delete [] data;
  return 0;
}
```

|  Language |  Average Runtime (seconds)  |
|:---------:|:---------------------------:|
|    C      |            1.71             |
|    C++    |            0.91             |
| C++ (new) |            0.92             |

<p>
The results are pretty clear here. The differences seem largely negligible depending on how much performance tuning you need to worry about in your system, but you can see that&ndash; in this case&ndash; C++ was faster at accessing raw arrays than C was. This difference is likely due to some variance in the system during the tests or perhaps even the compiler. In any case, this does show us that for the equivalent code in both languages, that C++ is not fundamentally any slower than C. We expected to see a slight slow-down using std::vector (even with .reserve()) because it is a nice container object that performs all the memory management for us.
</p>

<p>Now that we have discussed the fundamentals of how C and C++ are very similiar in their compilation processes and provided some real experimental results, it is hopefully clearer to see that C is not always faster than C++. That is not to say that someone could not write more performant C code than another in C++, but rather that the languages themselves do not exhibit any inherent performance differences. In fact, you should probably use the language best suited to your needs and worry about performance optimizations at the end. In any case, I am sure this battle will contain to rage in all corners of the internet, but suffice to say that&ndash; in general&ndash; C is not always more performant than C++ (and vice versa).</p>
