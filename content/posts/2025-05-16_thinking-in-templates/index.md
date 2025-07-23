---
title: "Thinking in Templates"
date: 2025-05-16 22:30:13
description: >
  On the conceptual and practical value of templates in programming.

slug: thinking-in-templates
tags:
  - Technical Notes
  - Programming
  - Thoughts
---

I believe that there is value in thinking in templates when it comes to
programming. I will try to explore the concept of templating in general, how it
reveals important patterns in programming, and how it appears in the wild often
in the form of _template engines_.

<!--more-->

## Templating

Templating is underrated. It is not only a programming tool but also an
essential way to think about defining and solving problems, and an aid in
designing systems.

We find templates almost at any level of the technology stack, even on the
command line. Do you need an `awk`-able output for your Git history?

```sh
git log --pretty="format:%h %ai %ae %s"
```

When we talk about templating, we refer to the _concept_ of defining a _fixed
structure_ and inserting _variable content_ into that structure. This may sound
too abstract, but you get the idea. For practical purposes, we will talk about
_template engines_ as a common practice for tooling. However, in the next
section, I will sidetrack to emphasize the importance of templating in an
extreme way.

## Programs as Copies of Their Input

Following the [HtDP] tradition, here is a function template in Scheme for the
so-called _structural recursion_ over a list of elements:

```lisp
(define (template lst)
  (cond
    [(empty? lst) ...]
    [else ... (first lst) ... (template (rest lst)) ...]))
```

This function template is derived from the structure of the list it consumes, as
per the definition of a list:

> A list is either empty or a pair of an element and a list.

Note that `template` is not a function here, but a _template_ for a function.
The variable content is **not** the list, but what we want to do with this
function structure. The idea of _structural recursion_ is that the function
structure reflects the structure of the self-referential data that it consumes.

Sum of the numbers in the list?

```lisp
(define (sum lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst) (sum (rest lst)))]))
```

Product of the numbers in the list?

```lisp
(define (product lst)
  (cond
    [(empty? lst) 1]
    [else (* (first lst) (product (rest lst)))]))
```

The template revealed a pattern here. We are programmers, we do not want
repetitions. How about we mechanize the template into a more general function,
`reduce`:

```lisp
(define (reduce base combine lst)
  (cond
    [(empty? lst) base]
    [else (combine (first lst) (reduce base combine (rest lst)))]))
```

Using `reduce`, we can now define `sum` and `product` more concisely:

```lisp
(define (sum lst)
  (reduce 0 + lst))

(define (product lst)
  (reduce 1 * lst))
```

**Quick takeaway:** Problems are analyzed in terms of data definitions. Programs
consume problems and produce solutions, both in terms of data. Most of the time
(unless we are designing an algorithm, the so-called _generative recursion_),
data structure defines the program structure, like a template, hence our
programs usually mimic the structure of the data they consume.

In other words, our programs are usually copies of the problem they intend to
solve.

## Template Engines

A _template engine_ is a program or a library that takes a structure in the form
of a _template_ and variable content in the form of _data_ of some kind, and
renders a final output in the form of a _document_.

We usually reach for a template engine when we do Web development, document
generation, emailing, reporting, etc. But what we actually do, intentionally or
not, is to separate the content and the presentation for attending them
independently. I learned the importance and power of this separation when I was
writing documents in [LaTeX]. Over the years, I started enjoying the same
principle with HTML and CSS.

When I first started using the Model-View-Controller (MVC) pattern, I had pretty
much the same feeling: You define a model, manage the model over the controller,
and render the model over the view, mostly using a template engine.

[Django], for example, has a template engine that allows you to define a
template in HTML and render it with a context -- data usually sourced from the
database via the Django _view_. However, with its filters and helpers, it is
almost too powerful -- undermining the core idea of templating. The same goes
for [Ember.js], as well.

This is an important point, because template engines are usually characterized
by their involvement in the rendering process. Consider the following
classification:

1. **Logic-less**: The template engine does not allow any logic in the template.
   All the logic is pre-applied to the variable content, our data, before it
   even arrives at the template engine. A typical example is [Mustache].
2. **Logic-capable**: The template engine allows you to define some logic in the
   template, such as not only what, but also when and how to display it. A
   typical example is [Jinja2].
3. **Logic-full**: Such template engines allow you to define any arbitrary logic
   in the template, by allowing you to even execute code in the template. One
   example is [Sweave] in R, which allows embedding R code in LaTeX templates.

I made up the name of the last category, but I think it is good enough to
describe the other extreme end of the spectrum.

What is there to learn from this classification, and why does it matter? That is
for my next post.

## Conclusion

In this post, I tried to emphasize the importance and ubiquity of templating in
programming. I also argued that templating is not only a practical tool but also
a way we design our programs.

I introduced the _template engines_ as we find them in the wild, and I tried to
classify them in terms of their involvement in the rendering process.

In my next post, I will show why and how such classification is important, and
present a case study on choosing the right template engine in a polyglot,
loosely coupled team of designers, frontend programmers and data analysts.

<!-- REFERENCES -->

[HtDP]: https://htdp.org/
[LaTeX]: https://www.latex-project.org/
[Mustache]: https://mustache.github.io/
[Jinja2]: https://jinja.palletsprojects.com/
[Sweave]: https://cran.r-project.org/web/packages/Sweave/index.html
[Django]: https://www.djangoproject.com/
[Ember.js]: https://emberjs.com/
