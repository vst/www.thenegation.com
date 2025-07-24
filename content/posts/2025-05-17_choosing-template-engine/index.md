---
title: "Choosing A Template Engine: The More Powerful Problem"
date: 2025-05-17 21:49:05
description: >
  A reflection on template engine choices through the lens of abstractions in
  terms of their power.

slug: choosing-template-engine
tags:
  - Technical Notes
  - Programming
---

Continuing on the topic of template engines, I would like to share my thoughts
on the _power_ of template engines, and how it relates to the _power_ of
abstractions in programming.

<!--more-->

First, let us look at the classification of template engines I have listed in my
[previous post]:

> 1. **Logic-less**: The template engine does not allow any logic in the
>    template. All the logic is pre-applied to the variable content, our data,
>    before it even arrives at the template engine. A typical example is
>    [Mustache].
> 2. **Logic-capable**: The template engine allows you to define some logic in
>    the template, such as not only what, but also when and how to display it. A
>    typical example is [Jinja2].
> 3. **Logic-full**: Such template engines allow you to define any arbitrary
>    logic in the template, by allowing you to even execute code in the
>    template. One example is [Sweave] in R, which allows embedding R code in
>    LaTeX templates.

Which template engine should you choose? _Logic-less_ seems, well, less.
_Logic-full_ gives us more value for the money -- it is more powerful.

But do we really want the _more powerful_?

## Show Me The Power

Let's consider the following two function signatures, both defined over
TypeScript arrays:

```ts
type MapFn<T, U> = (arr: T[], fn: (x: T) => U) => U[];

type ReduceFn<T, U> = (arr: T[], fn: (acc: U, x: T) => U, base: U) => U;
```

The first function, analogous to `Array.prototype.map`, takes an array and a
function, and applies the function to each element of the array, returning a new
array with the results of function applications:

```ts
mapFn([1, 2, 3], (x) => x * 2); // [2, 4, 6]
```

The second function, analogous to `Array.prototype.reduce`, takes an array, a
function and a base value, and applies the function to each element of the
array, accumulating the result in the base value:

```ts
reduceFn([1, 2, 3], (acc, x) => acc + x, 0); // 6
```

Which one is a more powerful function? Practically speaking, `reduceFn` is
significantly more powerful for at least two reasons:

1. It can transform the _structure_ of the data into a different structure. In
   the example above, we transformed an array of numbers into a single number.
   We can transform an array of numbers to an object, too:

   ```ts
   reduceFn([1, 2, 3], (acc, x) => ({ ...acc, [x]: x * 2 }), {}); // { 1: 2, 2: 4, 3: 6 }
   ```

2. We can define the `mapFn` function in terms of `reduceFn`:

   ```ts
   const mapFn: MapFn<T, U> = (arr: T[], fn: (x: T) => U) =>
     reduceFn(arr, (acc, x) => [...acc, fn(x)], []);
   ```

If we are sent to an island with only one of these two functions, we would
probably choose `reduceFn`, because it is more of a Swiss Army knife.

You want more power? Beware of what you wish for, though. Let us take a closer
look at what it means in terms of programming.

## The Toothpick vs The Knife

Luckily (or not), we are not the Robinsons, but programmers. We want to make
sure that we are not only solving a problem, but also doing it in a way that the
solution is maintainable, readable and understandable.

Consider the `Functor` and `Monad` type classes in Haskell. We are given two
abstractions:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b

bind :: Monad m => (a -> m b) -> m a -> m b
```

The first one consumes a function and a `Functor`, and applies the function to
the _inside_ of the `Functor`, returning the same `Functor` with a new value
inside. The structure is preserved **without** the function even knowing
anything about the structure.

In the second one, however, the function not only knows about the structure, but
must also preserve it. Here is a more concrete example for the above
abstractions:

```haskell
toothpick :: (a -> b) -> IO a -> IO b

knife :: (a -> IO b) -> IO a -> IO b
```

Firstly, in both cases we are given an `IO` action. `IO` is a `Monad`.

As a fellow programmer performing a review for your PR, I would rather see
`toothpick` in your code. `knife` is obviously more powerful, but it is also
more complex.

The only reassurance needed for the `toothpick` to perform well is to know that
the function passed to `toothpick` is a _total function_, i.e. it will not error
out. Do not let the `IO` fool you: Every `Monad` is a `Functor` and this
`toothpick` is just like or even is `fmap`.

But the `knife` allows arbitrary `IO` operations, and its signature does not
constrain what those operations are. In practice, this means the door is open to
many things that can go wrong. Even if the code is safe today, there is no
guarantee that someone will not cut their finger tomorrow.

That is one of the reasons why the common advice in Haskell is to pick _the
least powerful abstraction that does the job_. I find this advice sound first
due to wisdom, and then for technical reasons.

So, our takeaway is:

> If you do not have a good reason (_and necessary insurance policies_) to pick
> a more powerful abstraction, go with the less powerful one.

## Back to Template Engines

Now, let us go back to our template engines. We have seen that the _logic-less_
is not necessarily bad, and _logic-full_ is not necessarily good. It all depends
on the context.

In most cases, however, we can assume that the _logic-less_ template engine is
going to work well enough for us. Considering a team of programmers, designers
and data analysts, it can still be quite practical:

1. The designer and frontend programmer can work together over the template
   without worrying about the logic. They will assume a certain structure for
   the data, and focus on the presentation.
2. The data analyst will completely ignore the template and even the
   presentation to a certain extent, but only come up with a certain structure
   for the data definition that can represent the report.
3. The backend programmer will only care about sourcing the report data and the
   template, converting the report data to the template data, and rendering the
   template with the data.

What is good about it?

1. The interaction between individuals is more about concept and less about
   implementation.
2. The communication is materialized over clear contracts, i.e. the template and
   the data definitions, like in the form of JSON schemas, for example.
3. A simpler, _logic-less_ template engine can be deployed much easier than a
   more powerful one, such as a single-page Web application for the designer, a
   CLI tool for the frontend programmer and data analyst, which in return will
   result in faster iterations and feedback loops.
4. Simpler, _logic-less_ template engines are usually ported to multiple
   programming languages, giving the backend programmer the extra flexibility to
   pick the language, framework, deployment and hosting platform of their
   choice.
5. It is much easier to reproduce and refactor the template, as well as the
   entire team structure.

## Conclusion

If anyone asked me to choose a template engine, I would probably go with a
_logic-less_ one, if there is no particular reason and a VERY good maintenance
plan that justifies the need for a _logic-capable_ one. And for a _logic-full_
one, I can not think of a single reason to use it over a _logic-capable_ one,
except for fast prototyping, and maybe for domain-specific literate programming
such as RMarkdown, Jupyter Notebooks, etc.

Therefore, if I have to choose one right now, I would probably go for
[Mustache], and a JSON processor such as [jq] as a glue if needed.

This would give me _peace of mind_, if not joy, in choosing regular expressions
over a Turing-complete language.

I think another blog post is in order to discuss the trade-offs of using
_declarative, domain specific programming languages_ vs _imperative, general
purpose programming languages_.

<!-- REFERENCES -->

[Jinja2]: https://jinja.palletsprojects.com/
[Mustache]: https://mustache.github.io/
[Sweave]: https://cran.r-project.org/web/packages/Sweave/index.html
[jq]: https://stedolan.github.io/jq/
[previous post]: ../thinking-in-templates
