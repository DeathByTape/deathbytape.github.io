---
layout: post
title: Remote Desktop for Linux
excerpt: A step-by-step guide for setting up the free version of NoMachine.
categories: articles
tags: [nomachine,nx,remotedesktop,remote,linux,software,mac,osx,computers,nxclient]
comments: true
share: true
redirect_from: /post/125840933854/remote-desktop-linux
---

<p>If you have a business and you want to allow customers or clients remote desktop into your systems or if your beefy personal machine is simply a Linux box you wish to use through your laptop, then youre probably looking for a remote desktop client/server. Maybe you tried X11 Forwarding for a while, but that doesnt quite fit the bill after a while. Sometimes you really want to access a real desktop remotely.</p><p>One possible method for doing this is VNC. I was using VNC for some time but I noticed that performance was sometimes shotty and this made it painful for any real-time rendering that I wanted to view remotely (note: I only use this at home). Naturally, I set out to find an alternative. Consequently, I stumbled upon <a href="http://nomachine.com/" target="_blank">NoMachine (NX)</a>. Im going to go through the basics of setting up an NX server and connecting with an NX client (<a href="https://www.nomachine.com/download/download&amp;id=61" target="_blank">NoMachine Terminal Server v4</a>). If you want to follow along, download that now.</p><h2>Basic Installation</h2><p>Of course the first thing youre going to want to do is install the server. As I linked above, youre going to want to download the <a href="https://www.nomachine.com/download/download&amp;id=61" target="_blank">Linux NoMachine Terminal Server</a>. If youre using a non-Debian based Linux distribution, that link wont be exactly the one youre looking for. Instead, you will likely need the RPM or similar. In any case, pick your favorite package manager that comes with your distro and I will assume you already know how to use it. For sake of completeness, however, I will go through the Debian-based configuration. </p>

```bash
sudo dpkg -i <DOWNLOADED_PACAKGE_NAME>.deb
```

<p>That command will require root access (you will be prompted for a password). However, it will install the entire NX software package for you. By default, NX is installed in the <b>/usr/NX</b> directory.</p><p>The server will automatically start after installation and should also startup automatically on boot. The default port for the server to listen to is 4000. As a result, ensure that port 4000 is open for any hosts you wish to provide NX access to.</p><h2>Adding a User</h2><p>Next you will want to setup a user. For our example, we will assume the user already exists on the system and you only care the local users can access this machine over NX (i.e. no ldap or similar). In such a case, you will want to run the following command:</p>

```bash
sudo /usr/NX/bin/nxserver --useradd <USER_NAME>
```

<p>This command will add the user to the NX user database on the server and will fallback on the local system users to bootstrap the user environment. Additionally, this process will prompt you for a password that authenticates the user via NX.</p><p>To verify that the user is added to the server, run:</p>

```bash
sudo /usr/NX/bin/nxserver --userlist
```

<p>You should be able to identify your newly added user in this list. If this is the first user youve added, you should also verify that it is the only user in the list.</p><h2>Connecting to the Server</h2><p>Now were ready to connect to the server! You can download the <a href="https://www.nomachine.com/download" target="_blank">NX Client</a> for various operating systems. In my case, I will be using Mac OSX. After installing the client, you should select the option to create a new connection. From here, select the <b>NX</b> protocol and proceed to the next screen. From here you should enter the hostname or IP running your NX server. If you did not modify the port yourself, then 4000 should als already be correct. Finally, proceed through the next screens with the appropriate information (i.e. enter credentials and setup proxy if necessary).</p><blockquote><div>NOTE: You probably <b>dont </b>need a proxy in your internal network. Especially if you have not already set one up yourself.</div></blockquote><p>At this point, youre ready to make your first connection!</p><h2>Ugh. No session available.</h2><p>If youve logged in successfully, youve probably seen some message to the effect of, <No available sessions on this server>. Well, thats a little annoying. This is a limitation of NoMachine 4 (for it to work out-of-box you now need the paid version). But no worries for&ndash; as usual&ndash; there is a workaround. In our case, we will want to have an <a href="https://www.nomachine.com/AR10K00710" target="_blank">Xserver running</a> alongside our NX server. Rather than bore you with the details of this (of which you can read that post and other Xserver references), I will provide a quick and dirty script below to make this happen. But first, make sure you have Xvfb installed on the box running your NX server and then execute the following script on the same box (must be executed a <b>single time per restart</b>):</p>

```bash
#!/bin/bash
# Script to startup xserver on :1
# Author: Dennis J. McWherter, Jr. (dennis@deathbytape.com)
AUTHFILE="$HOME/Xvfb-1.auth"
COOKIE=`ps -ef | md5sum | cut -f1 -d" "`

rm $AUTHFILE
rm $HOME/.Xauthority

# Create auth file
xauth -f $AUTHFILE add :1 MIT-MAGIC-COOKIE-1 $COOKIE

# Always add to ~/.Xauthority
xauth add :1 MIT-MAGIC-COOKIE-1 $COOKIE

echo "Killing old Xsessions if they exist..."
ps aux | grep Xvfb-1 | grep -v grep | awk '{print $2}' | head -n1 | xargs kill -9

echo "Starting Xsession..."

Xvfb :1 -auth $AUTHFILE -screen 0 1024x768x24+render &amp;
DISPLAY=:1 nohup /etc/X11/Xsession startx -- :1 &amp;
```

<blockquote><div>NOTE: I havent fully debugged the problem, but if you receive an error about unix-default-session or something similar, simply restart the script a few times until it works.</div></blockquote><p>If you find that youre having issues finding this session, check your <b>~/.xsession-errors </b>file. Additionally, if youre seeing a User not allowed message, be sure to update your <b>/etc/X11/Xwrapper.config</b> and set the value to <i>anybody</i>.</p><p>Before youre ready to connect, however, you will also need some additional configuration. In <b>/usr/NX/etc/server.cfg</b> you will need to add the following line:</p>

```bash
AvailableSessionTypes "physical-desktop"
CommandSessreg = "/usr/bin/X11/sessreg"
```

<p>and to <b>/usr/NX/etc/node.cfg</b> you should add</p>

```bash
DisplayDefault :1
```

<p>Now your server is setup. Open your NX client and connect!</p><h2>Conclusion</h2><p>This step-by-step guide went through the process of setting up an official NX server (free version) on a Linux host and connect to it from Mac OSX (and, presumably, any other NX client OS). In doing so, we crafted a script which allows us to run all of this remotely (i.e. no need for physical desktop access). As a result, this technique can be used for any server that has X installed. Good luck!</p>
