---
title: Redefining the Ontology of Accounting
date: 2012-01-15 08:03:01
tags: computing
layout: post
---

The human-kind has been performing <em>accounting</em> for more than 7,000 years. This discipline does not only give us the arithmetic means to plan, execute and assess business transactions, but also the universal language of business.

In particular, we have been using the <em>double-entry bookkeeping</em> system for about the last 5 centuries as a framework to record transactions. Furthermore, with the help of the concept of <em>ledger</em>s, we have been consolidating transactions and reporting summaries of them through standardized documents such as balance sheets, income statements etc.

So far, so good... But is it all good? I have some doubts. In the following, I will briefly explain two concerns of mine. By the end, a recent ontological movement follows which really excites me.

<!-- more -->

Before proceeding, please note that the person who expresses his ideas here is a <em>poor</em> programmer, not an accountant.

## Problem 1: What happened to my data?

Firstly, I am not comfortable with the highly opaque nature of the information resulting from the accounting process. All the accountants which I've been working with (both juniors and seniors, regardless of their nationality being Turkish, Singaporean or Indian) concentrate mostly on the preparation of a <em>summary of transactional data</em> (such as balance sheets) which presents the top-level view of the business operations. Furthermore, there is not much emphasis on the very continuum of the business execution; there are significant cut-off times such as end of month, quarter or year. In conclusion, there is a critical mass of information loss (both in quantity and quality) due to the excessive focus on the summary of the data.

There is nothing wrong with summarizing <em>per se</em>. However, we should ask ourselves: "How relevant are these summaries to our ongoing business processes?"

I have always been impressed by <em>accountants</em> and their presence in office premises. What they do seems always to be a sort of sacred work. Nobody sees them around, knows what they do, how they do. Now, I understand better "Why?". Instead of that the accountant who collects and keeps probably the most valuable information on the business is an integral actor in decision making and business processes, she is simply isolated from the ongoing business to focus more on the fulfillment of this <em>degrading</em> work of summarizing data, calculating ratios, filling in forms, filing applications etc.

## Problem 2: Computers are meant to replace us, not our tools

Secondly, I am more concerned with the technical aspect of accounting. Let's look at the <a href="http://en.wikipedia.org/wiki/Accounting#Etymology">etymology of accountant</a> (from Wikipedia as of 2012-01-15):

> The word <em>"Accountant"</em> is derived from the French > word <em>Compter</em>, which took its origin from the Latin > word <em>Computare</em>.

 Is it all about computation? If so, we have a better understanding of computational models compared to 500 years ago. How did this affect the accounting throughout the time at all? As far as I understand, there is not much difference except that we are using portable calculators instead of pen and paper, personal computers instead of Facit.

For example, although relational database model could have deprecated the use of T-accounts entirely, it has been used only to replace our solid books to store the T-accounts in the electronic form. What a shame...

## There should be a way out!

Why am I concerned with accounting at all? During the last 5 years, I've been involved in designing and implementing e-commerce and financial portfolio management systems. I have been thinking that accounting in general could have served a solid basis for our data modeling. Everytime we prototyped around this idea, we failed and ended up having two seperate systems: one based on an operational data model and another one based on an accounting data model.

However, for about one and a half year ago, we met a different approach: REA (Resources - Events - Agents). Since then, a part of our e-commerce systems sits on top of REA. We are not using REA fully, but so far, I can tell that it was a wise choice and it seems very promising.

<a href="http://en.wikipedia.org/wiki/Resources,_Events,_Agents">Wikipedia article</a> gives a relatively good overview on REA. However, I'd suggest to get a copy of <a href="http://reatechnology.com/">"Model-Driven Design Using Business Patterns"</a> by Hruby et. all.

![REA Entity Diagram](/assets/posts/REA_entity_diagram.png "REA Entity Diagram")

Basically, REA defines an ontology of business <em>events</em> occuring among <em>agents</em> as <em>resources</em> are being exchanged during a business transaction or converted during a manufacturing process. Besides these entities, the core of the ontology also includes commitments of agents to events. Furthermore, "Model-Driven Design Using Business Patterns" shows how the REA ontology can be extended through policies and other patterns to incorporate various complex business processes. In short, REA is relatively more able to capture the micro-economics of business transactions, and has the capacity of incorporating business processes into accounting process.

One surprising side-effect of REA is that accounting (as we know of) is <em>consequential</em>, whereas double-entry bookkeeping system needs accounting information <em>a priori</em> (such as the account code), ie. before recording the debit/credit transaction. In other words, REA forces the user to define the business process and record the <em>business transaction</em> properly (<em>focus on relations as values</em>), whereas double-entry bookkeeping system forces the user to record <em>the debit/credit transaction</em> and rely on what has already been defined in the chart of accounts (<em>focus on nominal amounts as values</em>).

This means that times are ahead that we do no longer need to restructure our business just because we have purchased a new off-the-shelf accounting system.

REA comes with one more surprise egg: Using REA, accounting systems can <em>very easily</em> be implemented from an independent third-party perspective, ie. if you have multiple companies, one system can handle all accounting for your individual companies in full integrity.

What would be the value added of using REA in different domains? I personally believe that it is highly possible that any economic transaction can be modeled through REA. For example, portfolio management systems and automated trading systems can benefit a lot from REA.

Is REA complete? It seems that it is not. There is no standardization, no reference implementation and lack of consensus on some patterns. However, it should not demotivate the <em>hacker</em> from attempting to read more on REA and implementing a simple REA-based application to assess its usability.

Looking forward to writing more on this as we acquire more knowledge on REA.
