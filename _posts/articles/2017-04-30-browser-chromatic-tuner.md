---
layout: post
title: "Browser-Based Chromatic Tuner"
excerpt: ""
categories: articles
tags: [browser,tuner,chromatic,music,FFT,frequency,pitch]
comments: true
share: true
ads: true
---

Lately I have been looking for an excuse to use newer Javascript ES6 features. Since I am a full-time backend developer, I don't often have the opportunity to play with new frontend technology unless it's on my own personal time. Alas, I finally found something I wanted to build: a browser-based guitar tuner using the latest HTML5 and ES6. Now, I do believe I have seen such a thing elsewhere, but I was concerned about accuracy and other information. As a result, my tuner provides some more in-depth statistics about the entire conversion process. You can [go here to see the finished product](https://deathbytape.com/webTuner)! (NOTE: You'll get an SSL cert error due to github pages hosting, this is expected).

Eager to begin, I began looking around at the state of the Javascript world since I last left it. I decided to build my application using [EmberJS](https://www.emberjs.com/), [Plotly](https://plot.ly/), and [Materialize CSS](http://materializecss.com/). It seemed as though everything else I wanted to do could be accessed out of box via the [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) and [HTML5 canvas](https://www.w3schools.com/html/html5_canvas.asp).

After about an 8 hour coding binge (mostly learning these new API's), I had come to the product shown above. However, I did notice a few issues. Namely, I was using the Web Audio API's [AnalyserNode](https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode) to perform my [Fast Fourier Transform](https://en.wikipedia.org/wiki/Fast_Fourier_transform) (in short, the thing that takes my signal and breaks it into frequencies). This was super convenient since I neither (a) had to implement my own or (b) find a performant implementation. However, the resolution produced by the FFT seems to be too low to accurately estimate my low frequencies. Let me explain why this is.

## Quick FFT Run-Down

When you apply a frequency-domain FFT to a discretized signal, it will produce a digital approximation of values as an array. Typically, each index in this array represents a _frequency_ and each value represents the _magnitude_. Then, if you had a _continuous_ frequency-domain FFT, you could see how much of your signal was made up of frequency `123.34 Hz` by using the value as the index:

```
const magnitude = fdFFT[123.34]
```

This looks intuitive; however, we do not have a continuous FFT. Instead we have a _discrete_ FFT. In a discrete FFT, we have evenly spaced bins representing a particular frequency which are a function of two things: the sample rate and the size of our FFT. As a result, if you wanted to see frequency 123.34, you would have to do something like this:

```
const stepSize = sampleRate / FFT_SIZE
const idx = Math.round(123.34 / stepSize) # Some approximation is required here. It could be distance, etc.
const magnitude = fdFFT[idx];
```

The sample rate is determined by the audio input device and cannot be adjusted by us. The FFT size, however, is within our control. That being said, the Web Audio API limits us to a size no larger than 16384. Practically speaking, what does all of this mean? Well, we'll consider the system I've been developing on:

```
sample rate = 48000Hz
FFT size = 16384
(sample rate) / (FFT size) = 2.92Hz
```

If you look at the calculation above, you'll notice that the step between each bucket is 2.92Hz with this configuration. In other words, if you had a 1Hz signal, it would fall between the 0Hz and 2.92Hz buckets and likely be assigned to the closest one (i.e. 0Hz). As a result, you have now lost the ability to perform frequency analysis of signals at 1Hz.

## Problem

If you use this [non floating-point pitch-to-frequency reference](http://peabody.sapp.org/class/st2/lab/notehz/), you will notice that higher frequencies and notes are more widely spaced. Consider the difference between C#7 and C7; this is a difference of **125 Hz**. Now, comparing C#2 against C2 is merely **4 Hz**! Herein lies the problem. As we get lower, the difference between notes becomes miniscule. As a result, our FFT resolution must be high enough to detect the differences.

This notion of "high enough" seems a bit wishy washy, but for our purposes here, I think a minimum resolution of less than 1Hz is required. The distance between pitch is typically measured in [cents](https://en.wikipedia.org/wiki/Cent_(music)). In summary, the definition is:

$$
n = 1200 \cdot \log_2{b\over{a}}
$$

where $$b$$ is your measured frequency and $$a$$ is the expected frequency. If you were to plot this function, you would see that a 1Hz difference can make a huge difference for low frequencies, but be pretty miniscule for high frequencies. As an example, let's take our example of C7 and C2 again with a 1Hz difference.

$$
C7 = 2093\textbf{ Hz} \\
n = 1200 \cdot \log_2{2092\over{2093}} = -0.827
$$

$$
C2 = 65\textbf{ Hz} \\
n = 1200 \cdot \log_2{64\over{65}} = -26.84
$$

Relatively speaking, a 1Hz change in a C7 would likely be audibly indistinguishable, however, the same change on a C2 would make the note sound very off pitch. As you can see, to accurately measure the differences in low frequencies, our FFT has to be capable of providing us a fine resolution of frequency data.

## Future Solutions

In the future I plan to look around for pre-existing FFT Javascript libraries. Ideally, I could use something such as a [Chirp DFFT](https://en.wikipedia.org/wiki/Chirp_spectrum) or similar to reduce computational overhead on the parts of the frequency spectrum of which we don't care. If there are no existing libraries that can help me with this, then it looks like I'll have a fun project waiting for me in the future!
