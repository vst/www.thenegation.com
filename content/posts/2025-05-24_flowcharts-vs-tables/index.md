---
title: "Data Definitions, Not Flowcharts"
date: 2025-05-24 23:43:58
description: >-
  Computer programs are best understood in terms of the data they consume,
  process and produce. Flowcharts are useful for visualizing control flow, but
  not for understanding complexity.
taxonomies:
  tags:
    - Technical Notes
    - Programming
---

Computer programs are best understood in terms of the data they consume, process
and produce. Flowcharts are useful for visualizing control flow, but not for
understanding complexity.

<!-- more -->

One of the most famous quotes of [Frederick Brooks] is:

> Show me your flowcharts and conceal your tables, and I shall continue to be
> mystified. Show me your tables, and I won't usually need your flowcharts;
> they'll be obvious.
>
> -- _The Mythical Man-Month_, 1975

This quote is my constant reminder that computer programs are best understood in
terms of the data they consume, process and produce. The information that a
flowchart provides is not only too abstract, but also vague and underspecified
even as a conceptual model.

Defining what a computer program is deserves its own blog post, but my point
here is that it is NOT primarily about the procedures and control flows, but
rather the data structures and their relationships.

I keep emphasizing that data definitions are not only important for the
implementation of a program, but also for its design and documentation. It is
the only connection to the phase of problem analysis: The more precisely the
data is defined during analysis, the more precise the implementation will be.

Although I enjoy doodling diagrams, I never bring them to the fight. Instead, I
sit down and write down the data types and their relationships in a programming
language.

One benefit of using high-level, strongly typed functional programming languages
is that _functions_ and _effects_ can be expressed in terms of type definitions:
Function signatures and effect constraints are type definitions, and so on. You
can even use defunctionalization to express _functions_ as data definitions,
which is a common technique in functional programming.

In Haskell, I usually create a module named `Algebra` and tinker with the
definitions there. I still like to refer to my prototype as an _algebra_, a
habit from when I was studying algebraic data types and free monads. How it is
implemented (interpreted) is not important at this point.

What is wrong with _flowcharts_? Nothing, but they are not the right tool for
analyzing and designing a program. They are useful for visualizing the control
flow of a program, but not for understanding its complexity. When it comes to
communicating the high-level view of a program or a system to a complete
non-technical audience, I still use _flowcharts_.

Lastly, I have been thinking about how [arrows] can be used to describe a
high-level view of a program. Inspired by category theory, they offer a way to
represent computation (that is, that transforms input of type `A` into output of
type `B`), abstracting away internal implementation details while still focusing
on inputs and outputs.

This focus on data transformations is why I am exploring arrows as a design tool
-- they force you to think in terms of data type transformations rather than
procedural steps.

<!-- REFERENCE -->

[Frederick Brooks]: https://en.wikipedia.org/wiki/Frederick_P._Brooks_Jr.
[arrows]: https://en.wikipedia.org/wiki/Arrow_(functional_programming)
