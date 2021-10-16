---
title: Literate Haskell with Markdown
date: 2021-10-16
---

This is a short guide to write [Literate
Haskell](https://wiki.haskell.org/Literate_programming) programs using Markdown.

<!-- more -->

[The source code](Main.lhs) of this very Webpage is a Markdown file with a
preamble. At this same time, the source code is also a Literate Haskell program,
ie. you can compile and run it. Therefore, we first define a Haskell module:

```haskell
module Main where
```

... and then, define a `main` function:

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

By now, we have a valid Haskell program embedded in our Markdown document (the
source code). But we will define two more functions to demonstrate
[doctest](https://hackage.haskell.org/package/doctest).

```haskell
-- | Adds 7 to the given 'Int'.
--
-- >>> add7 35
-- 42
add7 :: Int -> Int
add7 = (+) 7

-- | Divides 42 by the given 'Int'.
--
-- >>> div42 1
-- 42
-- >>> div42 2
-- 21
-- >>> div42 3
-- 14
-- >>> div42 6
-- 7
-- >>> div42 7
-- 6
-- >>> div42 0
-- 0
-- >>> div42 42
-- 1
div42 :: Int -> Int
div42 = div 42
```

We need to install [markdown-unlit](https://github.com/sol/markdown-unlit) that
is a custom unlit program to extract Haskell code from Markdown files. Once
installed, we can compile our program:

```console
$ ghc -pgmLmarkdown-unlit Main.lhs
[1 of 1] Compiling Main             ( Main.lhs, Main.o )
Linking Main ...
$
```

This will produce your executable (`Main`) along with `Main.o` and `Main.hi`
files. You can run your program:

```console
$ ./Main
Hello World!
$
```

We could have ran the program directly using `runhaskell`, too:

```console
$ runhaskell -pgmLmarkdown-unlit Main.lhs
Hello World!
$
```

Also, we can produce the Haskell code of interest:

```console
$ markdown-unlit -h label Main.lhs Main.hs
$
```

We can study `Main.hs` or run `doctest` on it (do not forget to re-generate
`Main.hs` after you change the source code):

```console
$ doctest Main.hs
label:51: failure in expression `div42 0'
expected: 0
 but got: *** Exception: divide by zero
          ^

Examples: 8  Tried: 7  Errors: 0  Failures: 1
```
