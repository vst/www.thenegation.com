---
title: R with MOEA Framework using Renjin Script Engine
date: 2015-11-03
tags: programming
layout: post
---

Using R with Java is painful, ugly.

While I was recently revisiting [REpochX](https://github.com/vst/REpochX) which I quickly wrote long time ago to evolve R grammars using [Grammatical Evolution](https://en.wikipedia.org/wiki/Grammatical_evolution), I decided to do the same with [MOEA Framework](http://moeaframework.org/) instead of [EpochX](http://www.epochx.org/) as the latter does not look actively maintained.

Firstly, MOEA Framework is very comprehensive and the coding practice is a lot moderner compared to similar evolutionary computing libraries.

But, again, R/Java bindings... That's killing me.

<!-- more -->

There are quite a few different ways which we used in the past: JRI (from rJava), Rserve, OpenCPU and so on. All these require the programmer to manage an R installation and instances/daemons.

Enabled by the [Java scripting interface](https://docs.oracle.com/javase/7/docs/technotes/guides/scripting/programmer_guide/), [Renjin](http://www.renjin.org/) solves this mess nicely. Similar to jpython, jruby etc..., you can program in R in Java by passing R statements to the respective R scripting engine and consume results: R Language implemented in Java.

Renjin is not complete. The packaging system mimics the original R packaging, but there are many (now mainstream) libraries which are not available to Renjin users.

Nevermind... Below is a simple Java program which demonstrates the idea of using R with MOEA Framework through the Renjin Script Engine.

Don't forget to add the following to your `pom.xml`:

    <dependency>
      <groupId>org.moeaframework</groupId>
      <artifactId>moeaframework</artifactId>
      <version>2.6</version>
    </dependency>

    <dependency>
      <groupId>org.renjin</groupId>
      <artifactId>renjin-script-engine</artifactId>
      <version>0.7.0-RC7</version>
    </dependency>

<script src="https://gist.github.com/vst/a2c6d4599ad0f965ef03.js"></script>
