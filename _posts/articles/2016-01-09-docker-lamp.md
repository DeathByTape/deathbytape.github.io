---
layout: post
title: "Using Docker for Quick Web Replication"
excerpt:
categories: articles
tags: [docker,LAMP,Linux,Apache,MySQL,PHP,web]
comments: true
share: true
ads: true
---

Recently I encountered an issue where I wanted to produce a clone of a site that I was to modify for a friend. Naturally, I didn't want to make modifications directly to his live site so I moved forward with replicating the site on my own private server. By doing this, I enabled him to interact with a real copy of the site containing my modifications while leaving his live site unscathed by our development iterations. Of course, back in the day, setting up a replica meant reproducing the server environment from scratch and likely acquiring hardware of the following varieties: (a) a clean box or (b) an existing web box where you have room to share alongside the new site. In my case, I had neither of these readily available and I didn't exactly want to spin up a whole new box for this. I did, however, have a private box running my BNC sitting around that could be temporarily provisioned for this purpose. That being said, I still didn't want to install a [LAMP](https://en.wikipedia.org/wiki/LAMP_(software_bundle)) (**L**inux, **A**pache, **M**ySQL, **P**HP) environment on this box simply to have to clean up later (I have no intention of leaving this box as a web server long-term). This is where [Docker](http://docker.com) comes in.

For those who aren't familiar, Docker is a Linux container platform. In short, it allows you to quickly create lightweight virtualization that can be shared. What's more, its architecture allows you to take other people's pre-built images and start directly with them! This would allow me to setup the entire LAMP environment in the matter of minutes, make my customizations, and happily be on my way.

# Where did I start?

I have a strong preference to working locally whenever possible, so I spun up docker on my local box first. I then reminded myself the overall goal: replication of a LAMP-based website. Logically, the first step is to then setup a proper LAMP environment. Naturally, the first thing I did was Google, "docker lamp image" and used the [very first result](https://github.com/tutumcloud/lamp) that popped up. It appears that Tutum had already created a base image for LAMP installations; this is the perfect starting place.

After cloning the repo to the directory called _lamp_, I then built their image:

```bash
$ docker build -t tutum/lamp lamp/
```

This command builds an image called "tutum/lamp" using the files located in the "./lamp" directory. After a couple of minutes of downloading and building, all was well and I had a ready-to-go LAMP image.

```bash
$ docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
tutum/lamp          latest              059c1db2f260        About a minute ago  426.9 MB
```

With this image in place, the obvious next step was to boot it up. While I could have made modifications to the image build scripts, I decided instead to work on the container directly since this was a one-off project. Consequently, I immediately started it as follows:

```bash
$ docker run -it tutum/lamp /bin/bash
```

The result of this command (as expected) dropped me directly into a shell created with the lamp image. I then ran `/run.sh` in the container and verified that the installation was working correctly. As I anticipated, all seemed to be in working order. I would now move on to investigate the installation a little further.

# What did I modify and how did I keep those changes?

Now that I had an isolated, working LAMP installation I needed to get my friend's web content on there. That is, I needed a way to upload files to the server! The first thing I noticed was that the image neither included an FTP or SSH daemon. I then proceeded to install an FTPd on my own container and added it on a script for the [supervisord](http://supervisord.org/) configuration. Now when I ran the `/run.sh` script, I would have the FTP server running alongside my Apache and MySQL processes. Obviously, I proceeded to install his site content and made the appropriate script setting modifications to comply with the settings on the container.

After having everything in place, I needed to somehow retain my settings. Docker actually makes this pretty easy. I simply exited the container and ran the folowing command:

```bash
$ docker commit mycontainer replica
```

Now when I ran `docker images`, the output would look more like this

```bash
$ docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
hccreplica          latest              965a2383b9ae        About an hour ago   1.024 GB
tutum/lamp          latest              059c1db2f260        2 hours ago         426.9 MB
```

This shows that I have rolled all of my changes up into an image that I can use to create a new container later.

# How did I get this stuff to my server?

Now that I was ready to put this stuff on my server, how did I get it there? Well, docker provides a mechanism to save and load **images**. Recall that I just created an image containing all of my files and configuration.

```bash
$ docker save -o replica.tar replica
```

This saves the `replica` image in a local file known as `replica.tar`. After compressing this file and uploading it to my server, I was able to perform the inverse operation to recover the image:

```bash
# NOTE: This was done on the remote server
$ sudo docker load -i replica.tar
```

The image is now successfully on my remote host!

# How did I get things running?

As I mentioned, this box was/is not really intended to run a web server. Consequently, port 80 was actually not in-use and so a little bit of port forwarding was all the magic required to get this working.

```bash
# NOTE: This was done on the remote server
$ sudo docker run --name my_replica -d -p 80:80 replica /run.sh
```

After visiting the domain I had this running under, I was able to see the changes immediately.

# What about developing on this box now?

Now the box is running as a daemon and has no direct input out to the user. It may at first seem odd (since there is no SSH server on the box) and you may be curious as to how I could continue to modify the files directly. Well, fortunately, docker allows you to execute additional processes in a running container. As a result, I would just run a command like this to drop myself back into a shell:

```bash
$ sudo docker exec -it my_replica /bin/bash
```

Running this command provides me with the expected bash prompt and does not interrupt my existing service in anyway.

# Take Away

Docker is an incredibly useful tool. At this moment it allowed me to standup a full one-off server environment (from scratch) on the order of minutes. Similarly, I was able to build my image on one machine and have it execute flawlessy on another one. Having such power enables us as developers to do more at a much faster rate. Viva la containers!
