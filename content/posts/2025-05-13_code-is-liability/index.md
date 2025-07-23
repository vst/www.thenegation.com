---
title: "Code is Liability"
date: 2025-05-13 23:55:27
description: >
  Code is a liability, not an asset. This post explores practical ways to write
  less code, reduce complexity and improve maintainability.

slug: code-is-liability
tags:
  - Technical Notes
  - Programming
  - Principles
---

While programming, we often aim to avoid repetition. Repetition is boring, and
ironically, much of programming exists to automate boring tasks in the first
place. But repetition is just one symptom of a much deeper problem: Code itself
is a liability.

<!--more-->

Repetition reveals itself in different forms. Off the top of my head, I can
think of:

1. Boilerplate code
1. Duplicating logic in different places
1. Failing to abstract structural similarities
1. Ignoring existing high-quality standard or third-party libraries

I like programming because it is creative. But creativity is not measured by how
much code you write. To me, it is about correctness, scalability, performance,
readability and maintainability.

We can easily argue that every additional line of code is antithetical to
achieving the above goals. On the other hand, obsessing over line count should
not be a goal in itself. The principle should be to justify the marginal cost of
code by the marginal value it brings: Like businesses must justify each
liability on their balance sheet, developers should justify every line of code.

I think of code as a liability.

Delegating code to libraries does not eliminate liability; it just shifts it. If
a library becomes unmaintained, insecure or incompatible, your codebase pays the
price. In other words, assuming the liability of others is not a good idea. It
is still in your books, just under a different account.

How can we reduce such liabilities? Here are some ideas:

- Know your standard library and its idioms. It is the best library you can rely
  on. It is well-tested, well-documented and usually well-understood.
- Use existing libraries that are ubiquitous and well-known. We call them "the
  missing standard library". Every programming language has a few of them:
  `requests` for Python, `lodash` for JavaScript, `dplyr` for R, `text` for
  Haskell, etc.
- Use software design patterns to structure your code while eliminating or
  reducing excessive code. You can implement patterns in any programming
  language, such as Inversion of Control (IoC), Dependency Injection (DI) and
  Model-View-Controller (MVC). For example, without IoC, your functions may end
  up handling tasks outside their responsibility. Would you like to perform SMTP
  library configuration in your business logic, in every function you want to
  send an email from?
- Use metaprogramming to reduce boilerplate code. It is a powerful tool, but it
  can be overkill. Use it wisely. Some languages have great metaprogramming
  capabilities, like Lisp. Python has decorators and metaclasses, Rust has
  macros, Haskell has generic programming and Template Haskell, etc.

Failing to spot the above opportunities will lead to code bloat. Worse, you may
find yourself maintaining code that is reinventing the wheel, but half-baked.
This is a recipe for disaster.

Finally, and most importantly, building and using _Embedded Domain Specific
Languages_ (eDSLs) will not only help you reduce code, but also make your code
more readable and maintainable. For example, building a small eDSL to describe
routing rules or business workflows can reduce boilerplate and clarify your
intentions. In this regard, any declarative approach is better than an
imperative one. I compare it to literary writing: poems vs prose.

Code is a liability, but it is also your leverage. Use it carefully, manage it
wisely.
