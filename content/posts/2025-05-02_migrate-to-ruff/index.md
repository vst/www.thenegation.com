---
title: "Ruff and Ready: Linting Before the Party"
date: 2025-05-02 14:32:07
description: |
  A practical guide to migrating from traditional Python linters to Ruff, with a staged strategy, tooling insights, and lessons from a large production codebase.
slug: migrate-to-ruff
tags:
  - Technical Notes
  - Python
---

In this post, I share why I value linters and formatters, and how I migrate from
traditional Python tools to [ruff].

<!--more-->

> When I was using Haskell, I was fighting the type checker. Now I am fighting
> the Rust borrow checker.

I heard this quite a few times. But my relationship with \*_checkers_ is a bit
different: I see them as good friends, not enemies.

I listen to my friends, so I listen to the Haskell type checker as well. I
appreciate when it tells me that my thinking is wrong. Granted, it does not
always talk to me nicely, but my friends do not either.

The Haskell type checker has _science_ behind it. It is baked into the compiler.
When I want to make sure that I am doing the right thing, compiling my code is
usually enough. Passing `-Wall` and `-Werror` gives me good mileage. Then, I
have [fourmolu] as a formatter, and [hlint], [stan] and [weeder] as static
analyzers -- or commonly referred to as _linters_ -- which give me the courage
of an MMA fighter when I am refactoring my code, regardless of the size of the
change or how old the codebase is.

With dynamically typed programming languages, things are different. Take Python,
for example: There is no compiler and the type checker is completely optional,
which limits its usefulness, especially when third-party dependencies are not
completely type-annotated.

Let’s continue with the analogy. Imagine you go to a party only to find out that
your shirt is stained, because strangers point it out and laugh at you. Haskell
is the friend who tells you beforehand while you are still at home. With Python,
you have to throw a small party at home just to check if you are ready for the
big event. That small party involves a lot of testing, pseudo-type-checking,
static analysis, formatting, and linting. It grows into a full-blown party of
its own, making the actual event feel a bit dull.

So, while the Haskell type checker is backed by science, I like to think of
Python as offering a variety of engineering solutions instead. That is not
necessarily a bad thing, but the overall experience is significantly different.

## What I use for Python

I have been programming in Python for over 20 years now. For practical reasons,
it is one of my go-to languages, especially if I have to collaborate with
others.

One of our bread-and-butter projects has been using Python at its core. It is a
~100K lines codebase with significant use of metaprogramming. A few things
helped me to keep it maintainable and sane:

1. [mypy] (and [pyright] occasionally) as a type checker,
1. [black] as a formatter,
1. [isort] as an import sorter,
1. [pylint] as a linter,
1. [flake8] as another linter, with quite a few `flake8` plugins.

I do not start a project without `mypy` added to my test suite and IDE
configuration. I cannot stress enough how important it is to have type
annotations in Python, especially when you are shipping to production
frequently.

`black` and `isort` helped me to embrace the _opinionated_ style of formatting.
It was a leap forward for me, who is keen to use his own half-baked styles.
Collaborating with others is then easier, at least in terms of formatting. I
simply do not care about formatting anymore. If things look awkward to my eyes,
it is usually because I am overloading Python expressions, like nested
comprehensions or lambdas. I have learned to spot them and refactor into
composable functions.

`pylint` and `flake8` are very useful for linting, enforcing conventions and
static analysis. They helped me to catch quite a few bugs, improve the quality
of the code, adopt new language features, drop old and deprecated ones, and so
on. But they are slow, configurations overlap, and they are not always
consistent with each other.

I ended up putting the configuration for `pylint` and `flake8` into
`pyproject.toml`, enabled all rules, selectively disabled them, and put them
into two categories while disabling them: Rules which I genuinely want to
disable, and rules which I ignore for now but plan to enable later.

The result is okay-ish, but swinging between `pylint` and `flake8` is a bit
annoying. All in all, I wish I could have a single tool to do all of this.

## Ruff Enters the Scene

I have been following [ruff] since its early days. It is a formatter and linter
for Python, but implemented in Rust. It combines, in a sense, the functionality
of `black`, `isort`, `pylint` and `flake8` into a single tool. It even includes
rules from various popular `flake8` plugins, such as `flake8-bugbear`,
`flake8-bandit`, `flake8-type-checking`, and so on. It works with
`pyproject.toml`, unlike `flake8` that requires me to use `flake8-pyproject`.

It became quite stable and popular over time. It has good IDE support now, as
well. In my case, `python-lsp-ruff` is sufficient to enable `ruff` in `neovim`
or `emacs`. But, most importantly, it is fast -- very fast!

So, I decided to adopt `ruff` as my main, and going forward, probably my only
tool for formatting and linting Python codebases. For the larger codebases,
however, I had to devise a strategy to migrate to `ruff`.

## Migration Strategy

I have defined three stages for migrating large codebases to `ruff`:

- **Stage 1:** Add `ruff` to the codebase and run it without any rules.
- **Stage 2:** Provide minimal `ruff` configuration, _select_ ALL rules, but
  _ignore_ all errors and warnings for now.
- **Stage 3:** Enable rules one by one, or in small batches, and fix the code
  accordingly.

In the following subsections, I will describe each stage and report on my
experience in a large production codebase.

### Stage 1

This is what I call the _starter stage_:

1. Add `ruff` to dependencies: We are using Nix, so `pkgs.ruff` was enough for
   me.
1. Add minimal `ruff` configuration to `pyproject.toml`, without enabling any
   rules.

Running `ruff format` or `ruff check` at this stage may sound useless, but it
caught a few issues in this codebase:

- A mis-specified `# noqa` comment: I typed `# noqa: 501` instead of
  `# noqa: E501`. This was never reported by `pylint` or `flake8` before. Maybe,
  I had to enable some `pylint` or `flake8` rule or flag to catch this issue. I
  do not know. But `ruff` caught it right away.
- Redundant empty line after some `if TYPE_CHECKING:` statements: Strangely,
  `black` did not complain about it before, but `ruff` did. After removing the
  empty line, `black` was still happy with the formatting. So, maybe `ruff` is
  even more opinionated than `black`? If so, I have no objections.

Here is the minimal `ruff` configuration I used for _Stage 1_:

```toml
[tool.ruff]

## Set the required version of ruff:
required-version = ">=0.11.7"

## Exclude directories and files from ruff runs:
exclude = [
  ## Common excludes:
  ".bzr",
  ".direnv",
  ".eggs",
  # ... TRUNCATED ...
  "node_modules",
  "site-packages",
  "venv",

  ## Project excludes:
  "tests/fixtures/*",
  "tmp/*",
]

## Set line-length to 120 characters which we want to keep for now:
line-length = 120

[tool.ruff.analyze]

[tool.ruff.format]

[tool.ruff.lint]

## Enable specific rules or categories:
select = []

## Ignore specific rules or categories:
ignore = []
```

### Stage 2

In this stage, we want to enable all rules. `ruff` has a way to tell us all
available linters and their codes:

```console
$ ruff linter
 AIR Airflow
 ERA eradicate
FAST FastAPI
 YTT flake8-2020
 ANN flake8-annotations
... TRUNCATED ...
```

It can also output the list in JSON format:

```console
$ ruff linter --output-format=json
[
  {
    "prefix": "AIR",
    "name": "Airflow"
  },
  {
    "prefix": "ERA",
    "name": "eradicate"
  },
  {
... TRUNCATED ...
```

I am lazy person, so I worked harder and wrote a small [jq] script to generate a
list of rules to go into the `select` key in `ruff.lint` section:

```jq
def prc($parentPrefix; $parentName):
  .prefix as $thisPrefix
    | .name as $thisName
    | .categories as $categories
    | ($parentPrefix + $thisPrefix) as $prefix
    | (if $parentName then ($parentName + " -> " + $thisName) else $thisName end) as $name
    | ( if $categories
          then $categories[] | prc($prefix; $name)
          else { code: $prefix, name: $name }
        end
      )
;

def rpad(x; len; fill):
  if len == 0
    then x
    else x + (fill * (len - (x | length)))
  end
;

.
  | sort_by(.name | ascii_upcase)
  | .[]
  | prc(""; null)
  | ( rpad("\"" + .code + "\","; 8; " ") + " ## " + .name)
```

Running this `jq` script on the `ruff linter` output gives me the code prefix of
rules to select, along with the linter name as comment:

```console
$ ruff linter --output-format=json | jq --raw-output --from-file rules.jq
"AIR",   ## Airflow
"ERA",   ## eradicate
"FAST",  ## FastAPI
"YTT",   ## flake8-2020
"ANN",   ## flake8-annotations
"ASYNC", ## flake8-async
"S",     ## flake8-bandit
"BLE",   ## flake8-blind-except
"FBT",   ## flake8-boolean-trap
"B",     ## flake8-bugbear
"A",     ## flake8-builtins
"COM",   ## flake8-commas
"C4",    ## flake8-comprehensions
"CPY",   ## flake8-copyright
"DTZ",   ## flake8-datetimez
"T10",   ## flake8-debugger
"DJ",    ## flake8-django
"EM",    ## flake8-errmsg
"EXE",   ## flake8-executable
"FIX",   ## flake8-fixme
"FA",    ## flake8-future-annotations
"INT",   ## flake8-gettext
"ISC",   ## flake8-implicit-str-concat
"ICN",   ## flake8-import-conventions
"LOG",   ## flake8-logging
"G",     ## flake8-logging-format
"INP",   ## flake8-no-pep420
"PIE",   ## flake8-pie
"T20",   ## flake8-print
"PYI",   ## flake8-pyi
"PT",    ## flake8-pytest-style
"Q",     ## flake8-quotes
"RSE",   ## flake8-raise
"RET",   ## flake8-return
"SLF",   ## flake8-self
"SIM",   ## flake8-simplify
"SLOT",  ## flake8-slots
"TID",   ## flake8-tidy-imports
"TD",    ## flake8-todos
"TC",    ## flake8-type-checking
"ARG",   ## flake8-unused-arguments
"PTH",   ## flake8-use-pathlib
"FLY",   ## flynt
"I",     ## isort
"C90",   ## mccabe
"NPY",   ## NumPy-specific rules
"PD",    ## pandas-vet
"N",     ## pep8-naming
"PERF",  ## Perflint
"E",     ## pycodestyle -> Error
"W",     ## pycodestyle -> Warning
"DOC",   ## pydoclint
"D",     ## pydocstyle
"F",     ## Pyflakes
"PGH",   ## pygrep-hooks
"PLC",   ## Pylint -> Convention
"PLE",   ## Pylint -> Error
"PLR",   ## Pylint -> Refactor
"PLW",   ## Pylint -> Warning
"UP",    ## pyupgrade
"FURB",  ## refurb
"RUF",   ## Ruff-specific rules
"TRY",   ## tryceratops
```

After adding this to my `pyproject.toml` (right inside `[tool.ruff.lint].select`
key), running `ruff check` gave me a huge list of errors and warnings:

```txt
...
Found 14494 errors.
[*] 3546 fixable with the `--fix` option (3332 hidden fixes can be enabled with the `--unsafe-fixes` option).
```

It was time to ignore them all. So, I wrote another `jq` script to generate the
`ignore` list for me:

```jq
def rpad(x; len; fill):
  if len == 0
    then x
    else x + (fill * (len - (x | length)))
  end
;

def lpad(x; len; fill):
  if len == 0
    then x
    else (fill * (len - (x | length))) + x
  end
;

.[]
  | ( rpad("\"" + .code + "\","; 10; " ")
    + " ## "
    + lpad((.count | tostring); 5; " ")
    + (if .fixable then " * " else "   " end)
    + ("https://docs.astral.sh/ruff/rules/" + .name + "/")
    )
```

Running this `jq` script on the `ruff check` output gave me a list of all rules
to ignore, along with the number of occurrences, whether they are fixable
automatically by `ruff`, and a link to the documentation:

```console
$ ruff check --statistics --output-format=json | jq --raw-output --from-file ignore.jq
"D200",    ##  2249   https://docs.astral.sh/ruff/rules/unnecessary-multiline-docstring/
"S101",    ##  1484   https://docs.astral.sh/ruff/rules/assert/
"COM812",  ##   539 * https://docs.astral.sh/ruff/rules/missing-trailing-comma/
"D401",    ##   446   https://docs.astral.sh/ruff/rules/non-imperative-mood/
                      ... TRUNCATED ...
"RUF100",  ##     1 * https://docs.astral.sh/ruff/rules/ruff-specific-rules/
"RUF010",  ##     1 * https://docs.astral.sh/ruff/rules/explicit-f-string-type-conversion/
```

This went into my `pyproject.toml`, right inside `[tool.ruff.lint].ignore` key.

By the end of this stage, I was back to where I started. If I trusted that
`ruff` could replace `pylint` and `flake8` (and all the plugins we are using), I
would have removed them from the codebase. But I am still not sure yet for this
large codebase.

### Stage 3

This is a long and tedious stage. The objective is to go through the list of all
rules we ignore in `pylint`, `flake8` and `ruff` configurations, make sure that
`ruff` is covering the former two, and finally remove them from the codebase.

For the larger codebase, we are at the beginning of this stage. What I do is as
follows:

1. Create 2 sections as code comments inside the `ruff.lint.ignore` list:
   1. `## GENUINE IGNORES:` This is where I put the rules I genuinely want to
      ignore.
   2. `## TO-BE-ATTENDED:` This is like a TODO list, where I put the rules I
      want to enable or genuinely ignore in the future.
2. Occasionally regenerate the list of ignored rules. Number of occurrences may
   change, or some ignores may even disappear.
3. Run housekeeping sessions to enable rules from `## TO-BE-ATTENDED:` section,
   and fix the code accordingly. I usually do this in small batches, one or two
   rules at a time.

## Conclusion

Beyond testing, linters and code formatters are helpful to manage the complexity
of a codebase. For some languages, these may be the only options. As for Python,
managing linters can become a chore in itself. [ruff] is a great tool to combine
the functionality of several linters into a single tool along with a code
formatter and imports sorter. It is fast, reliable and has good IDE support.
Lastly, I like how `ruff` authors stay on top of their rules, making sure that
they are relevant, useful and coherent with each other. I hope that my
observations and sentiments are shared by the community.

On the other hand, my migration strategy helped me to adopt `ruff` in existing
codebases without breaking the workflow. I am still working through the
remaining rules in our larger codebase, but I am confident the migration will be
complete in a few months.

<!-- REFERENCES -->

[black]: https://black.readthedocs.io/en/stable/
[flake8]: https://flake8.pycqa.org/en/latest/
[fourmolu]: https://github.com/fourmolu/fourmolu
[hlint]: https://hackage.haskell.org/package/hlint
[isort]: https://pycqa.github.io/isort/
[mypy]: https://mypy.readthedocs.io/en/stable/
[pylint]: https://pylint.pycqa.org/en/latest/
[pyright]: https://github.com/microsoft/pyright
[ruff]: https://docs.astral.sh/ruff/
[stan]: https://hackage.haskell.org/package/stan
[weeder]: https://hackage.haskell.org/package/weeder
[jq]: https://stedolan.github.io/jq/
