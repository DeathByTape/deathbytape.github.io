---
layout: post
title: The Slow (but noticeable) Shift to Functional
excerpt: "It's coming anyway, so you may as well embrace it."
categories: articles
tags: [functional,programming,haskell,java,c++,cpp,java8,c++14]
comments: true
share: true
redirect_from: /post/122385153704/slow-shift-to-functional
---

<p>Recently I have noticed a strong push toward the functional programming paradigm. While I do not think it has yet been fully embraced, its refreshing to see. Whether its because people are finally sick of writing boilerplate or that its simply becoming the new hotness, it is becoming undeniably prevalent in our daily lives as programmers. I suspect that in ten years from now, many programmers will fully embrace the functional paradigm and have an incredibly powerful tool in their arsenals.</p><p>If youve been following along, you probably know by now that I am a full supporter of functional programming. I believe it has many incredible benefits, but that discussion is for another time. Instead, today I plan to focus on the different ways functional programming is creeping into our daily lives; you probably use it already without recognizing it. As these features become more and more commonplace, the entire idea of functional programming will also become more intuitive to programmers (i.e. the number one reason I hear as to why people do not currently embrace it).</p><h2>Scala</h2><p><a href="http://scala-lang.org" target="_blank">Scala</a> is an ever-prevalent JVM-based language. It is used for everything from <a href="http://www.scala-lang.org/old/node/1403" target="_blank">small DSLs</a> to big data processing (see: <a href="https://spark.apache.org/" target="_blank">Spark</a>). The fact that this language has been gaining popularity bodes well for functional languages seeing as it boasts both functional and object-oriented characteristics. To a pure functional programmer, Scala is a bit of a nightmare. However, many imperative programmers find Scala a relatively easy starting point. It provides all the flexibility were familiar with as imperative programmers and allows us to use functional concepts where it is most intuitive and cuts down on code. Not to mention you get all the power associated with Java! Lets take a look at an example. Well take a brief look at Project Euler problem number 6. </p>

```scala
object Main {
	def main(args: Array[String]) {
		def square: (Int => Int) = (x) => x * x;
		val fstHundredNatural: List[Int] = (1 to 100).toList;
		val sumOfSquares: Int = fstHundredNatural.map(square).sum;
		val squareOfSum: Int = square(fstHundredNatural.sum);
		val difference: Int = squareOfSum - sumOfSquares;
		println(difference);
	}
}
```

<p>Though this solution is more verbose than it really needs to be, it is clear that we can use several functional concepts and this code is very straightforward to read. For instance, we use a lambda (anonymous function, etc.) to write our square function and we use a single map call to avoid a for loop. Similarly, you will see that our actions did not mutate fstHundredNatural: immutability is a useful concept that is often employed in many functional languages.</p><h2>Java</h2><p>The next major language I want to mention is Java. Everyone even remotely familiar with programming understands Java is an important language in today&rsquo;s software ecosystem. Anyone who has been working with it for any amount of time knows it&rsquo;s notorious role as an &ldquo;enterprise&rdquo; language. When I see this phrase, I cringe. Rather than see &ldquo;enterprise,&rdquo; I see <i>king of boilerplate</i>. Yuck, no one likes boilerplate. That being said, even Java 8 now has the all new <a href="https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html" target="_blank">stream api</a> to make functional programming a little more accessible and to cut down on some of that boilerplate. Along with stream api, Java 8 considers functions as first-class objects (i.e. Function<>, Supplier<>, Consumer<>, BiFunction<>, etc.) and introduces lambdas through use of their arrow notation. Let&rsquo;s take a quick look:</p>

```java
package example;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Main
{
	public static void main (String[] args)
	{
		Function<Integer, Integer> shift = (value) -> value + 1;
		ArrayList<Integer> myInts = new ArrayList();
		// Standard java imperative loop
		for (int i = 0 ; i < 100 ; ++i) {
			myInts.add(i);
		}
		// Let's add only the event numbers AFTER the shift
		List<Integer> modifiedList = myInts.stream()
			.map(shift)
			.filter((num) -> num % 2 == 0)
			.collect(Collectors.toList());
		System.out.println("Summing the following numbers:");
		modifiedList.forEach(System.out::println);
		Integer result = modifiedList.stream().reduce(0, (total, num) -> total + num);
		System.out.println("Result: " + result);
	}
}
```

<p>You will notice that Oracle has gone to great lengths to incorporate functional elements into the core Java library. This is useful for those of us looking for ways to eliminate boilerplate but aren&rsquo;t quite ready to give up the full power of Java, its libraries, and its mutability of all the things! Happy and powerful.</p>
<h2>C++</h2>
<p>The last imperative language I want to draw attention to is C++. C++11 and C++14 bring many great changes to the language. That being said, some limited functional elements have been brought into the core language. Many people hear C++ and think &ldquo;oh, that&rsquo;s the hard language to squeeze performance.&rdquo; It is true that C++ provides much flexibility and control down to the machine level, but it doesn&rsquo;t mean that it has to be difficult and montonous to write. Let&rsquo;s review:</p>

```cpp
#include <algorithm>
#include <functional>
#include <numeric>
#include <iostream>
#include <vector>

using namespace std;

int main() {
	vector<int> myInts;
	for (int i = 0 ; i < 100 ; ++i) {
		myInts.push_back(i);
	}
	auto shift = [](int&amp; value){ value + 1; };
	for_each(myInts.begin(), myInts.end(), shift);
	int sum = accumulate(myInts.begin(), myInts.end(), 0, plus<int>());
	cout << sum << endl;
	return 0;
}
```

<p>Anyone who is already familiar with C++ will notice that it looks pretty similar. However, you will see that they have introduced the <i>auto</i> keyword which infers a type for you. Similarly, you can notice that we have a lambda function (i.e. shift). It does look odd when compared to other languages, but it is a fully functional lambda nonetheless. That is, functions can now be first-class objects in C++ without the mere use of function pointers (i.e. observe the <i>plus<></i> method). This adaptation marks a significant shift in the thought of programmers. C++ is notoriously slow to change (as is Java) and both languages appear to be adapting these very useful and prevalent functional concepts. Moreover, these languages (especially C++) are very widely used across the industry today. Though it&rsquo;s not as elegant in C++, it is a strong start in the right direction.</p>
<h2>Haskell</h2>
<p>Haskell is a purely functional language. If you&rsquo;ve read other parts of my blog, you&rsquo;ve probably already been introduced to do this language. There will be no examples in this section, however, I would like to point out the growing adoption of this language. <a href="http://fpcomplete.com" target="_blank">FPComplete</a> has just released <a href="https://www.fpcomplete.com/blog/2015/06/stack-0-1-release" target="_blank">stack</a> which is aimed to be a true build system in the Haskell ecosystem. Similarly, it seems that the community continues to grow while the toolsets are maturing. Even if you&rsquo;re not excited about this language (or other functional languages for that matter), I do recommend that you follow this trend closely. I believe it is likely that these languages will be commonplace and you will encounter them at some point in your career within the next 10-20 years.</p>
<h2>Conclusion</h2>
<p>Although mass adoption of the functional paradigm is far from a reality, it appears the industry is making large strides in that direction. We can see by the increased adoption of functional languages and the introduction of functional features into imperative languages that developers are becoming aware of the power gained by using higher-order functions. While I do not believe that any single paradigm is the <i>best</i> for all cases, I do believe that many common problems are made easier by following a functional way of thought. With that in mind, I am excited about this industry-driven shift.</p>
