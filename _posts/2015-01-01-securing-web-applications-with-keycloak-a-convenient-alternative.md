---
title: "About Securing Web Applications with Keycloak"
date: 2015-01-01
tags: security programming
layout: post
---

Securing a web application can be easier than you think. Keycloak is a
convenient and powerful alternative to implementing your own security
mechanism by providing an SSO and IDM implementation.

<!-- more -->

Programmers like to think about the problem in-hand and hate repeating
themselves to do non-problem related tasks such as securing the
application, implementing a mail subsystem etc. For this reason, there
exist Web development frameworks.

I prefer using decent programming languages such as Java (*sic*), Scala
or Lisp. Sometimes, I have to use specialized programming languages
like R. But just to get rid of boring tasks like user management,
going back to Django or using Play or choose-a-famous-name-framework
really sucks.

My latest attempt was to maintain a
[Maven archetype](https://github.com/vst/jee7-sandbox-archetype) to
bootstrap a Java EE-backed, REST-enabled application, develop a
powerful API (both internal and external) and complete the UI
development in pure HTML/CSS/Javascript, preferably in Ember.js or
Angular.js. To secure the application, I have been using
[PicketLink](http://www.picketlink.org/), Red Hat's security and
identity management subsystem for Java applications.

Now, I decided to give it a go for
[Keycloak](http://keycloak.jboss.org/), which gives an out-of-box
solution for browser applications and RESTful web services. Keycloak
is under the hood of the PicketLink family. It is very easy to setup
and even deploy a production grade SSO system. Fun part is that you
don't even need to know Java to use Keycloak. It will integrate well
into your existing Web project regardless of the programming language
which you are using. You can use Google Authenticator for TOTP, too!

Here is how you can start and see yourself:

1. Watch the [introductory videos](http://keycloak.jboss.org/docs) and take a glance to the [Keycloak documentation](http://keycloak.jboss.org/docs).
2. Get yourself a [Wildfly](http://wildfly.org/).
3. [Setup Keycloak](http://docs.jboss.org/keycloak/docs/1.1.0.Beta2/userguide/html/server-installation.html#WAR_distribution_installation) in your Wildfly.
4. Play with the examples which come along with the Keycloak distribution.

If you are not an absolute beginner, you will get the `js-console`
example up-and-running in about 2 hours after reading these lines.
