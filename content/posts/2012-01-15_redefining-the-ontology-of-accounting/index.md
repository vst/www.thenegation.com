---
title: Redefining the Ontology of Accounting
date: 2012-01-15 08:03:01
updated: 2023-10-18 13:53:28
slug: redefining-the-ontology-of-accounting
description:
  A brief opinion on the current state of accounting and the Resources - Events
  - Agents (REA) ontology.
taxonomies:
  tags:
    - Computing
---

_Accounting_ is more than 7,000 years old. This discipline helps us to plan,
execute and assess business transactions. It is also the universal language of
business.

In particular, _double-entry accounting_ (also known as _double-entry
bookkeeping_) system has been used for about the last five centuries as a
framework to record business transactions. We can then consolidate these
transactions and report summaries through standardized documents such as
ledgers, balance sheets, income statements, etc.

However, we are living in the age of information technology. And the way we
perform accounting still looks like it is pre-computer age.

Can we do any better now?

In the following, I will briefly explain my concerns. Then, a relatively recent
and exciting ontological approach to accounting follows.

<!-- more -->

Before proceeding, please note that I am a programmer, not an accountant.
However, I do hands-on accounting and develop accounting systems.

## Problem 1: What happened to my data?

Firstly, the information resulting from the accounting process is quite opaque.
The typical accounting process tends to concentrate mostly on the preparation of
_summary of transactional data_ for statutory filing which presents a top-level
view of business operations.

Furthermore, there is not much emphasis on the very continuum of business
execution; there are significant cut-off times such as end of month, quarter or
year. In conclusion, there is some information loss both in quantity and quality
due to the primary focus on the summary of the data.

There is nothing wrong with summarizing _per se_. What I am trying to say is
that the target audience of such summaries is merely external.

## Problem 2: Computers to Replace Processes, Not Tools

Secondly, I am more concerned with the technical aspects of accounting. Let's
look at the [etymology of accountant] (from Wikipedia as of 2012-01-15):

> The word _"Accountant"_ is derived from the French word _Compter_, which took
> its origin from the Latin word _Computare_.

If it is about computation, we have a better understanding of computational
models and methodologies compared to 500 years ago. If this sounds like a bold
claim, check this out: [Algebraic Models For Accounting Systems]. It even talks
about _monoids_ which can characterize certain accounting systems with special
properties. The term _monoid_ can be dated back to the early 1930s, but not any
earlier.

As far as I understand, there is not much of a difference between the
pre-computer age and now in how we perform accounting except that we are using
portable calculators instead of pen and paper, and personal computers instead of
Facit.

## A Better Way

Why am I concerned with accounting?

I have been involved in designing and implementing e-commerce and financial
portfolio management systems. I think that accounting in general could have
served as a more solid basis for our data modeling. Every time we prototyped
around this idea, we failed and ended up having two separate systems: one based
on an operational data model and another one based on an accounting data model.

However, around 2010, we met a different approach: Resources - Events - Agents
(REA). We used some concepts from REA in our e-commerce application. Later in
2014, we used a watered-down version of it to build our portfolio and fund
management system.

I can tell that it was a wise choice, and it still seems very promising.

[REA Wikipedia article] gives a relatively good overview of REA. However, I
would recommend [Model-Driven Design Using Business Patterns] by Pavel Hruby.

In essence, REA defines an ontology of business _events_ occurring among
_agents_ as _resources_ being exchanged during a business transaction or
converted during a manufacturing process.

![REA Entity Diagram](./REA_entity_diagram.png)

Besides these entities, the core of the ontology also includes the commitments
of agents to events. Furthermore, "Model-Driven Design Using Business Patterns"
shows how REA ontology can be extended via policies (and various other patterns)
to incorporate complex business processes, capture more granular information
about business transactions, and derive reports directly from first-hand data.

As for the last point above: One surprising result of using REA is that
conventional accounting is a natural consequence of working with business
transactions: You fold them and get your insights, and some of these insights
are already what we use the _double-entry accounting system_ for.

REA comes with one more surprise egg: Using REA, accounting systems can _very
easily_ be implemented from an independent, third-party perspective, i.e. if you
have access to business transactions for multiple entities, one system can
handle accounting for all these entities in full integrity.

Is REA ready? From an academic perspective, probably yes. Yet, we cannot find
practices, let alone the best ones, publicly. There is no reference
implementation, either. Yet, do not be discouraged: learn more about REA and
play with it to assess its merits.

<!-- REFERENCES -->

[etymology of accountant]: http://en.wikipedia.org/wiki/Accounting#Etymology
[Algebraic Models For Accounting Systems]:
  https://www.researchgate.net/publication/275209178_Algebraic_Models_for_Accounting_Systems
[REA Wikipedia Article]: http://en.wikipedia.org/wiki/Resources,_Events,_Agents
[Model-Driven Design Using Business Patterns]:
  https://www.researchgate.net/publication/220689230_Model-Driven_Design_Using_Business_Patterns
