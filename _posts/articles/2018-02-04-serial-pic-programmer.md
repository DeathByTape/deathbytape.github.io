---
layout: post
title: "Creating a Serial PIC Programmer from an Arduino (Part 1)"
excerpt: ""
categories: articles
tags: [pic,pic16,serial,arduino]
comments: true
share: true
ads: true
---
A while back I bought a couple of [PIC16F57](http://www.microchip.com/wwwproducts/en/PIC16F57) ([DIP](https://en.wikipedia.org/wiki/Dual_in-line_package)) chips because they were dirt cheap. I figured someday I could use these in *something*. Yes, I know, this is a horrible way to actually build something and a great way to accumulate junk. However, this time the bet paid off! Only about a year or two too late; but that\'s beside the point. The problem I now had was that I didn\'t have a PIC programmer. When I bought these chips I figured I could easily rig a board up to the chip via USB. Lo and behold, I didn\'t read the docs properly; this chipset doesn\'t have a USB to serial interface. Instead, it only supports Microchip\'s In-Circuit Serial Programming (ICSP) protocol via direct serial communication. Rather than spend the $40 to buy a PIC programmer (thus, accumulating even more junk I don\'t need), I decided to think about how I could make this happen. Glancing at some of my extra devices lying around, I noticed an unused Arduino. This is how the idea for this project came to life. Believe me, the irony of programming a PIC chip with an ATMega is not lost on me. So for all of you asking, why would anyone do this? the answer is two-fold. First, I didn\'t want to accumulate even more electronics I would not use often. Second, these exercises are just fun from time to time!



## Hardware Design



My prototype\'s hardware design is targeted to using an Arduino Uno (rev 3) and a PIC16F57. Assuming the protocol looks the same for other ICSP devices, a more reusable platform could emerge from a common connector interface. Likewise, for other one-offs it could easily be adapted for different pinouts. Today, however, I just have the direct design for interfacing these two devices: [![PIC](/assets/images/pic-programmer-v1-sch.png)](/assets/images/pic-programmer-v1-sch.png)Overall, the design can\'t get much simpler. For power I have two voltage sources. The Arduino is USB-powered and the 5V output powers the PIC chip. Similarly, I have a separate +12V source for entering/exiting PIC programming mode. For communication, I have tied the serial communication pins from the Arduino directly to the PIC device. The most complicated portion of this design is the transistor configuration; though even this is straightforward. I use the transistor to switch the 12V supply to the PIC chip. If I drive the Arduino pin 13 high, the 12V source shunts to ground. Otherwise, 12V is supplied to the MCLR pin on the PIC chip. I make no claims that this is the most efficient design (either via layout or power consumption), but it\'s my first working prototype. ## Serial Communication with an Arduino



Arduino has made [serial communication pretty trivial](https://www.arduino.cc/reference/en/language/functions/communication/serial/). The only problem is that the Arduino\'s serial communication ports are [UART](https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter). That is to say, the serial communication is *asynchronous*. The specification for programming a PIC chip with ICSP clearly states a need for a highly controlled clock for *synchronous* serial communication. This means that the Arduino\'s Serial interface won\'t work for us. As a result, we will go on to use the Arduino to generate our own serial clock and also signal the data bits accordingly.

### Setting the Clock Speed

The first task to managing our own serial communication with the Arduino is to select an appropriate clock speed. The key to choosing this speed was finding a suitable trade-off between programming speed (i.e. fast baud rate) vs. computation speed on the Arduino (i.e. cycles of computation between each clock tick). Remember, the Arduino is ultimately running an infinite loop and isn\'t actually doing any parallel computation. This means that the amount of time it takes to perform all of your logic for switching data bits must be **negligible** between clock ticks. If your computation time is longer than or close to the clock ticking frequency, the computation will actually impact the clock\'s ability to tick steadily. As a rule of thumb, you can always set your clock rate to have a period that is roughly 1 to 2 orders of magnitude than your total computation. Taking these factors into account, I chose 9600 baud (or a clock at 9.6KHz). To perform all the logic required for sending the appropriate programming data bits, I estimated somewhere in the 100\'s of nanoseconds to 1\'s of microseconds for computation. Giving myself some headroom, I selected a standard baud rate that was *roughly* two orders of magnitude larger than my computation estimate. Namely, a period of 104 microseconds corresponds to a 9.6KHz clock. After completing the project I could have optimized my clock speed. However, that was unnecessary for this project. The clock rate I had selected worked well. The 9600 baud rate is fast enough for timely programming the device because we don\'t have much data to transmit. Similarly, it provides us a lot of headroom to experiment with different types of computation.

### Generating the Clock Signal

While this discussion has primarily focused on the *design* *decisions* involved in choosing a clock signal rate, how did we generate it? The process really comes down to toggling a GPIO pin on the Arduino. In our specific implementation, I chose pin 2 on the Arduino. While you can [refer to the code](https://github.com/DennisMcWherter/arduino-pic16-programmer/blob/master/arduino/PIC16_Programmer/PIC16_Programmer.ino) for more specific details, an outline of this process follows: 

```c
inline bool clock_tick() {

  if (PORTD & _BV(SERIAL_CLOCK_PORT)) {

    PORTD &= ~_BV(SERIAL_CLOCK_PORT);

    // If clock is currently high, toggle to low

    return false;

  }

  PORTD |= _BV(SERIAL_CLOCK_PORT);

  // Return true if we have turned clock high

  return true;

}



void loop() {

  if (clock_tick()) {

    // ... compute and control data signals

  }

  // delay for 52us (half clock period)

  waitForHalfClockPeriod();

}

```



As you can see, ticking the clock basically consists of toggling it and then making sure each loop iteration waits for half the clock period. The omitted section for data control is where most of the logic for the controller goes. However, it runs in a time that is far less than 52 microseconds. As a result, the duration of each loop iteration can be considered as:

$$
\begin{equation} 52\mu s \gg \delta \\
52\mu s + \delta \simeq 52\mu s
\end{equation}
$$

where $$\delta$$ is the time required to perform computation for data control. Consequently, the clock ticks at an appropriate rate. I have included an image taken from my oscilloscope below. ![Oscilloscope](/assets/images/NewFile1clk.png)This image provides some empirical evidence that what we\'re doing should work. While there is no data being sent on this image (we\'ll show more of that below), we can generate a nice clock signal (notice the $$\frac{1}{\|dX\|}$$ and BX-AX lines on the image) at 9.6KHz by toggling the pin and waiting.

### Controlling the Data Line

Now that we have a steady clock, we need to control the data line. Writing this section of code felt like I was back in my [VHDL](https://en.wikipedia.org/wiki/VHDL)/[Verilog](https://en.wikipedia.org/wiki/Verilog) days. The basic principal--- from a signal generation perspective--- was to only change the data lines on a positive clock edge. There were minor complications for the **read data** command (since the pin has to go from output to input), but this was an isolated case with a straightforward solution. To actually control the signal, we manually turn the serial data pin (in our case, pin 4) high or low depending on the command and data each clock cycle. This ICSP programming protocol starts with a **6 bit command sequence**. If the command requires data, then a *framed* 14-bit word (total of 16 bits with the start and stop bits) is sent or received. Command and data bits are sent **least significant** **bit first**. In the case of my PIC16F57, the commands are only **4 bits** where the upper 2 bits are ignored by the PIC. Likewise, since the PIC16F57 has a **12 bit word**, the upper 2 bits of the data word are also ignored while sending and receiving data.

#### The Load Data Command


Let\'s first investigate the **load data** command. This command queues up data to write to the chip. A series of additional commands and delays are executed to flush this data to the chip. The bits for the load command are **0bXX0010** (where X is a don\'t care value). However, let\'s take a look at it under the oscilloscope:

<figure class="image">
    <img src="/assets/images/NewFile1ldcmd.png" alt="Clock on oscilloscope"/>
    <figcaption><strong>NOTE:</strong> The clock in this image is halved (i.e. ~4.8KHz) due to a programming error. This has been fixed in the actual code and doesn't affect results other than the timescale for this plot.</figcaption>
</figure>
The yellow curve is the clock and the blue curve is our data line. Starting from the left (and reading the blue curve under the yellow high marks) we can read our command exactly as intended: **0b0100XX**. Notice that it is inverted since our *least* significant bits are sent first. If you follow along a little bit further on the top, you\'ll notice a clock-low delay. This delay allows the PIC chip to prepare for data. The data for the command immediately follows the delay. #### Implementation Overview



Without going too deeply into the details (again, I [refer to the code](https://github.com/DennisMcWherter/arduino-pic16-programmer/blob/master/arduino/PIC16_Programmer/PIC16_Programmer.ino)), the command sequences are modeled as a state machine. Generally, when executing a command, we keep track of the number of steps taken for a particular command already. Since each command consists of sending a finite number of bits, we can now precisely what to do at each step. The other detail I mentioned earlier was about the **read command**. This command is *sent* over pin 4 in output mode, but during the delay this pin must switch to input mode. When in input mode, the PIC chip will proceed to send data at the given memory address. To accommodate this, each command starts by setting the pin as *output* mode. In the case of the read command, it sets the pin as input when appropriate. ## Conclusion



I\'ve enjoyed building out this project. When initially building, I really wanted to discover whether or not I could build a PIC programmer with an Arduino. This post reviews my initial prototype and a high-level description of the Arduino code. Unfortunately, the story doesn\'t end here. Due to a variety of limitations, I had to introduce a PC-based controller to stream data to the Arduino. My finished product also removes extra elements (i.e. a second 12.5V power supply) and moves from a breadboard to a more permanent fixture. Even so, I leave these details to a part 2 of this post. In any case, you can checkout my code [from this repo](https://github.com/DennisMcWherter/arduino-pic16-programmer) and run it today. While I work on the second part of the write up, you can always read through what I\'ve done. For now though, I will leave you with a picture of some messy breadboarding.

<figure class="image">
    <img src="/assets/images/IMG_1169.jpg" alt="Some messy breadboarding" style="transform: rotate(-90deg);margin-left:30px;"/>
    <figcaption>An LED powered by the PIC chip after programming it with the Arduino Programmer!</figcaption>
</figure>
