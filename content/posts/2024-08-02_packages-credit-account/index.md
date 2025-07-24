---
title: Your package.json as a Credit Account
date: 2024-08-02 21:54:10
description:
  Every dependency in your (JavaScript) project is like an expense on your
  credit account.
slug: packages-credit-account
tags:
  - Technical Opinion
  - JavaScript
  - TypeScript
  - Computing
---

It is quite easy to install a new `npm` package into your project. I go one step
further, and insist on that actually it is _too_ easy.

Every dependency in your project is like an expense on your credit account. And
that is [not good] if this credit account is not managed properly.

<!--more-->

## Economic Analogy

Every codebase is an economic artifact, whether it has some monetary or
non-monetary value. We invest our resources in a codebase with an expectation of
return on this investment.

Sometimes, we do not have all the resources to build everything from scratch. We
use libraries, frameworks, and tools to speed up the development process and
deliver some value.

It is like running business on credit: If you do not have enough capital, you
simple borrow it to launch or grow your business. If things go well, you will
have enough revenue to pay back the borrowed capital. If things go wrong, you
will keep on borrowing until you are bankrupt.

The outcome usually depends on the quality of the investment you make with the
borrowed capital.

In the same way, every dependency in your project is like borrowing some capital
and making an investment with it.

## Pick Your Dependencies Wisely

1.5GB of `node_modules` folder with over 100K JavaScript and TypeScript files
for a relatively small SPA? Patch version update that breaks the whole app on a
colleague's machine? 10 minutes of CI/CD pipeline just to install dependencies?
Still managing dependencies which were only installed as peer dependencies of a
package we dropped 1 year ago? Being stuck at a certain version of a package for
over 9 months because the upgrade causes a cascade of breaking changes or
Node.js "heap out of memory error" which is only experienced by a few random
developers around the globe who can not help reproducing it at all? I have seen
them all and more.

No, thanks...

I will not rant more about the JavaScript ecosystem. At the danger of being
confused with their grumpy uncle by the younger developers, I will just say that
most of the problems are inherent to the language and ecosystem, and no tooling
can solve it.

It is what it is. After being badly hurt every other dependency upgrade, I have
decided that I am going to treat `package.json` as a credit account.

For example, I used to add `day.js` to dependencies the moment when I need to
work with dates, which is almost in every project. I know, `Date` is a pain to
work with, but nowadays I prefer to use `Date` instead of `day.js` if the
use-case is simple or infrequent.

I used to add `react-icons` to dependencies to have access to the universe of
all icons ever imagined and created. But now, I prefer to use SVGs or inline
SVGs if I do not need many icons.

Recently, I have dropped `next-auth` (now Auth.js) in favour of a handcrafted
authentication solution. Indeed, I dropped the adapter for Auth.js as well. The
outcome was quite funny (not): I have dropped 2 libraries from my dependencies
and ended up _removing_ more code than I added in that PR. Note that this is not
always the case. It is just that Auth.js does not have a decent abstraction and
lacks a good implementation (and yes, even the upcoming `v5`).

The opposite is also true. I have decided to add [purify-ts] to my dependencies
on all of my TypeScript projects. It is a great library with zero dependencies
that helps me to write better TypeScript code. It single-handedly solves many
control-flow problems and decreases the complexity (a.k.a. entropy) of my
codebase.

## Embracing Idiosyncrasies

I enjoy having skin in the game. For smaller projects, I set a limit of 10
dependencies. For each additional dependency beyond this threshold, I plan to
make a donation to a charity, and maybe, whose cause I fundamentally oppose.

I understand and embrace the idea that every project is different and has its
own idiosyncrasies. I am not saying that you should drop all your dependencies
and write everything from scratch. I am just saying that you should be aware of
the cost of every dependency you add to your project.

<!-- REFERENCES -->

[not good]: https://youtube.com/shorts/eIKqfujuxIQ?si=nEiLZG3siiZXKlQ6
[purify-ts]: https://gigobyte.github.io/purify/
