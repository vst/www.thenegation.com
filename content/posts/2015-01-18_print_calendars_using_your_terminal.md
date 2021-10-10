---
title: Print Calendars Using Your Terminal
date: 2015-01-18
tags: lifehack
layout: post
---

Planning my schedule and highlighting important deadlines using a software proved to be less effective for me compared to using pen and paper. I started a short time ago to print a month calendar which is very handy for this task.

If you want to print your own calendar in a nice format as a postscript file, you can try the following on your terminal with [pcal](http://pcal.sourceforge.net/):

```bash
brew install pcal ## Or "sudo apt-get install pcal" on Ubuntu/Debian
pcal -w -P a4 -F 1 -t Garamond -d Garamond > calendar.ps
ps2pdf calendar.ps ## To convert it into PDF format if you need
```

Now you have a nicely formatted [yearly calendar](https://dl.dropboxusercontent.com/u/14420846/blog/posts/calendar2015.pdf).
