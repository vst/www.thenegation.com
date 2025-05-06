---
title: "Nix Flake Templates"
date: 2025-05-06 11:36:39
description: |
  Notes on creating and using personal Nix Flake templates to bootstrap new projects or experiments.
taxonomies:
  tags:
    - Technical Notes
    - Templates
    - Nix
---

Nix is now central to how I structure my workstation setups and manage
development and production environments across my projects. Over time, I found
myself repeating certain setups. This post is a short note on how I started
working with Nix Flake templates to avoid or reduce this repetition.

<!-- more -->

## Using Nix and Self Plagiarism

I am actively using [Nix] from my workstation setup to development environments,
from Docker image builds to CI/CD pipelines, and even on production servers. One
of the themes that comes up often is provisioning a codebase, a development
environment and packaging configuration for a new project.

What I usually do is check one of my previous projects and copy the relevant
parts of the `flake.nix` file or `default.nix` and `shell.nix` files to the new
project. It does not require too much effort. At the end, Nix is a declarative
language that is heavily based on expressions, and studying and plucking
individual pieces from various sources is usually not a big deal.

I am totally fine and have no problem with plagiarism in this context.

## Actual Problem and a Possible Solution

On the other hand, individual programming languages, their tooling and testing
frameworks have their own peculiarities which I have to learn, adapt to and
re-learn on an ongoing basis. Over the years, I have figured out a few patterns,
learned how to avoid excessive tooling and picked up some tools that I like to
use for each language I am actively using. For example, a Python project in my
world starts its life as follows:

1. Stick to [setuptools] for packaging and distribution.
1. Use [pyproject.toml] for configuring as many tools as possible.
1. Use [mypy] for type checking.
1. Use [pytest] for testing.
1. Use [black] and [isort] for formatting.
1. Use [pylint] and [flake8] for linting and static analysis.
1. Use [ruff], more recently, for formatting and linting that replaces the above
   four tools.
1. Use [nox] for running tests and linters.

Provisioning a development and testing environment for a Python project like
this using Nix was not trivial at the beginning. But, looking at my Nix-powered
Python projects now, I can see that the setup is actually quite simple and
common across projects. This is true as well for my Haskell projects, R scripts
and even more idiosyncratic works such as developing a [GitHub CLI
extension][gh-extension].

So, I decided to collate various pieces of these setups into a Nix Flake
template repository. The idea is to have a template that I can use as a starting
point for a new project of a specific language and particular nature.

## What is a Nix Flake Template Repository?

A Nix Flake template repository is a repository that contains a Nix Flake with a
`templates` output. The `templates` output is an attribute set that contains the
name of the template and the template specification. The special attribute name
`defaultTemplate` is used to initialize a Flake without specifying a template
name.

There are [Official Nix Templates] and [Nix Community Templates] that you can
start using. Also, you will find lots of interesting templates in the wild as
well.

Here is the starting point of my own humble, newborn Nix Flake template
repository, <https://github.com/vst/nix-flake-templates>:

```nix
{
  description = "A Collection of Personal Nix Flake Templates";

  outputs = { self, ... }: {
    templates = {
      trivial = {
        path = ./templates/trivial;
        description = "A trivial template that does nothing much.";
      };

      python-script = {
        path = ./templates/python-script;
        description = "A simple Python program script.";
      };

      defaultTemplate = self.templates.trivial;
    };
  };
}
```

For example, running the following command will populate the current directory
with the contents of the `templates/trivial` directory, which is a single
`flake.nix` file:

```sh
nix flake init --template github:vst/nix-flake-templates#trivial
```

Then you can continue to edit the `flake.nix` file and add your own custom Nix
expressions.

If you omit the `#trivial` part, the `defaultTemplate` will be used, which is
the `trivial` template in this case.

## A Few Tricks

In my template repository, I use a few tricks to make template development
easier and usage more convenient, whenever applicable.

For example, I _gitignore_ all `flake.lock` files but keep them locally to
develop and test the templates.

Also, each template can be `run` without initializing a new project or checking
out the template repository, if there is a program to run in the flake output:

```console
$ nix run "github:vst/nix-flake-templates?dir=templates/python-script" --no-write-lock-file -- --name Nix --count 10
warning: not writing modified lock file of flake 'github:vst/nix-flake-templates?dir=templates/python-script':
• Added input 'flake-utils':
    'github:numtide/flake-utils/11707dc2f618dd54ca8739b309ec4fc024de578b?narHash=sha256-l0KFg5HjrsfsO/JpG%2Br7fRrqm12kzFHyUHqHCVpMMbI%3D' (2024-11-13)
• Added input 'flake-utils/systems':
    'github:nix-systems/default/da67096a3b9bf56a91d16901293e51ba5b49a27e?narHash=sha256-Vy1rq5AaRuLzOxct8nz4T6wlgyUR7zLU309k9mBC768%3D' (2023-04-09)
• Added input 'nixpkgs':
    'github:nixos/nixpkgs/979daf34c8cacebcd917d540070b52a3c2b9b16e?narHash=sha256-uKCfuDs7ZM3QpCE/jnfubTg459CnKnJG/LwqEVEdEiw%3D' (2025-05-04)
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
Hello Nix!
```

This is a typical `nix run` command with the `--no-write-lock-file` flag that
does not require or create a `flake.lock` file. Any arguments to the executable
are passed after the `--` argument.

## Conclusion

Cookie-cutter or similar tools to generate project skeletons never appealed to
me. It usually involves a lot of effort to test them in different settings and
hosts, but the result is often not worth it.

On the other hand, I also tried simpler solutions such as [gist]s or snippets
([1][snippet-1], [2][snippet-2]) which I can use as starting points for my
projects despite being not convenient for re-use and pretty much unmaintainable
due to lack of clear structure.

The Nix approach to templating, however, is simple and understandable, and most
importantly, educational. I just started putting together a few templates that I
can use as a starting point for my projects, small tools or quick-and-dirty
experiments. I am planning to use these templates to create or support future
blog content as well.

<!-- REFERENCES -->

[Nix Community Templates]: https://github.com/nix-community/templates
[Nix]: https://nixos.org/
[Official Nix Templates]: https://github.com/NixOS/templates
[black]: https://black.readthedocs.io/en/stable/
[flake8]: https://flake8.pycqa.org/en/latest/
[gh-extension]: https://cli.github.com/manual/gh_extension
[gist]: https://gist.github.com/
[isort]: https://pycqa.github.io/isort/
[mypy]: https://mypy.readthedocs.io/en/stable/
[nox]: https://nox.thea.codes/en/stable/
[pylint]: https://pylint.pycqa.org/en/latest/
[pyproject.toml]: https://peps.python.org/pep-0518/
[pytest]: https://docs.pytest.org/en/latest/
[ruff]: https://docs.astral.sh/ruff/
[setuptools]: https://setuptools.pypa.io/en/latest/
[snippet-1]: https://github.com/joaotavora/yasnippet
[snippet-2]: https://github.com/L3MON4D3/LuaSnip
