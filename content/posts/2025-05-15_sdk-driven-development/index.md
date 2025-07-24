---
title: "SDK-Driven Development: A Litmus Test for Good Software Design"
date: 2025-05-15 18:23:55
description: >
  How thinking of your code as an SDK can help validate your development
  practices and improve code quality

slug: sdk-driven-development
tags:
  - Technical Notes
  - Thinking
---

This-driven development, that-driven development, and now you should bother with
this other thing?

<!--more-->

I am only half-serious. You should not. Yet you might still want to consider it
as a litmus test for your projects.

I am not saying that you should use Software Development Kits (SDKs) for
development. We all do. Rather, I am asking you to think of your codebase and
product as an SDK or part of it.

But first, what is all this XYZ-driven development?

## What is XYZ-Driven Development?

_XYZ_ simply refers to the guiding principle or artifact that drives the
development process. Mind you, this is not necessarily a methodology like
"Agile" or "Waterfall", which drive the _organization_ of the development
process.

I like to think of this distinction as follows: You gather all the developers in
a room and organize them to work on a project using an Agile methodology, for
example. Then, you ask them to return to their catacombs and attend to their
work with a specific ruleset -- let us say, the infamous Test-Driven
Development.

There are many such development techniques. I promised myself not to list them
here, but I can tell you what the common objectives are for all of them: They
help us build the _right thing_ _correctly_ and _efficiently_.

## Which XYZ-Driven Development?

It depends, of course.

The important thing to understand is that they are not mutually exclusive. To
me, choosing one over the other and sticking to it as if it were a religion is a
mistake.

A programmer should study and understand the principles behind each of them and,
unless mandated otherwise, choose the ones that fit the bill. Mix and match
_Domain-Driven Design_ with _Property-Based Development_ to streamline
definitions of invariants and testing them, for example.

Knowing and appreciating these principles is not enough. Practicing them
consistently is what makes the difference.

I learned how to convert such principles to my own inner voice (or one of them).
It makes it easier to internalize them for practicing. For example, I noticed
how my inner voice was motivating me to use the [HtDP Design Recipe] in my
career even today, after teaching it more than 15 years ago. I have been
actively using [doctest][doctest-python] in Python and
[doctest][doctest-haskell] in Haskell, which are analogous to "Examples" and
"Tests" in the HtDP Design Recipe. I even changed my go-to language to Haskell
to paint HtDP Design Recipe's "Contract" more clearly, using strongly typed data
definitions and type constraints.

## The Litmus Test

All these principles and practices are great, but I had to find a way to ensure
they make sense in the grand scheme of things. I needed a litmus test.

That is when I started thinking of my work as a collection of SDKs. I am not
talking about the SDKs you find in the wild, but rather interfaces and toolkits
I create for myself and my team.

REST APIs in the form of an OpenAPI specification are a great example of SDKs
for starters. For a well-designed and well-documented REST API, you do not need
an SDK library that does not come with additional value such as error handling,
caching or rate limiting. I noticed this clearly when I was looking into the
[Zendesk API] documentation. Zendesk maintains a single library in Ruby and
lists another 20 community-maintained libraries written in different languages.
Honestly, I do not need any of them.

I design and document most of my REST APIs for myself and my team. Then, I take
the extra step to imagine third-party developers using them. This comes
naturally with REST APIs. Implementing an SDK library is then a matter of
convenience: If the library will create further value, I will write it.
Otherwise, I will not.

Also for systems administration and DevOps, I first used [Ansible] to streamline
the management of our servers. Writing playbooks is OK, but going beyond that to
convert them to roles is a good practice from collaboration perspective. This
SDK approach worked quite well for me and my team. Now, I am developing [NixOS]
modules for various services we deploy. In both cases, the goal is to compose
well-defined and documented modules (SDK) into a complete system in a few lines
of code (application).

But how about Web-based frontend applications?

Certain aspects of frontend applications are completely boilerplate:
authentication, layout, navigation, managing secrets, etc. Developing a complete
application in the form of a library will not work, but how about factoring the
boilerplate code into one or more libraries?

This is what I envisioned and implemented with [@alioguzhan] at work. He ended
up creating a complete SDK for our frontend applications, consisting of a set of
libraries and an `npx create-frontend-app` command.

What is the practical outcome of this?

1. The application layer becomes thinner and easier to maintain.
2. We have a consistent way of building frontend applications.
3. The boring but important parts became standardized and easy to refactor.
4. Instead of a single, monolithic application, we now have a collection of
   loosely coupled applications developed independently, without interfering
   with or blocking each other.
5. We addressed testing, previewing, and production deployment issues during the
   development of the SDK, and they come for free for the applications built on
   top of it.
6. We are now exploring options with our clients' tech teams to use our SDK to
   build their own auxiliary applications.

## Conclusion

This is what I mean by SDK-driven development. It is nothing but a litmus test
for the principles and practices we should be using: Make sure that complete
strangers can find their way around your codebase or product, start developing
and deploying their own applications, and do so with only the knowledge of the
SDK-like documentation, libraries or modules you provide.

<!-- REFERENCES -->

[HtDP Design Recipe]:
  https://htdp.org/2003-09-26/Book/curriculum-Z-H-5.html#node_sec_2.5
[doctest-python]: https://docs.python.org/3/library/doctest.html
[doctest-haskell]: https://hackage.haskell.org/package/doctest
[Zendesk API]:
  https://developer.zendesk.com/api-reference/ticketing/introduction/
[@alioguzhan]: https://yildiz.dev/
[Ansible]: https://www.ansible.com/
[NixOS]: https://nixos.org/
