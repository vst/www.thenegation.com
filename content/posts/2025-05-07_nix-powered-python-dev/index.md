---
title: "Nix-Powered Python Development"
date: 2025-05-07 19:11:06
description: |
  Python development environment using Nix flakes with support for testing, linting, formatting, and LSP-based editor integration.
taxonomies:
  tags:
    - Technical Notes
    - Templates
    - Nix
    - Python
---

After a few years of floating from one hack to another, this is my practical
guide to setting up a reasonable Python development environment using Nix flakes
with support for testing, linting, formatting, and LSP-based editor integration.

<!-- more -->

I have mentioned in my earlier posts that [I am migrating][ruff-post] to [ruff]
for my Python projects, and [I have started][nix-tmpl-post] putting together a
[Nix template repository][vst-nix-repo].

First, we will look at the Python project requirements, structure and
configuration, and then we will look at the Nix configuration.

Before we dive into the details; I have created a Nix flake template for this
setup. You can use it to get started quickly. The template is available at:

<https://github.com/vst/nix-flake-templates/tree/main/templates/python-package>

## Python Part

Let us consider a Python project with following requirements:

1. It should be a proper Python package.
1. The packaging should be done using [setuptools].
1. Package data should be included in the package so that it can be installed
   during runtime.
1. There should be a [pyproject.toml] file and all possible tooling should be
   configured using it.
1. Codebase should be type-checked using [mypy].
1. Codebase should be tested using [pytest].
1. Codebase should be linted using `ruff`.
1. Codebase should be formatted using `ruff`, imports should be sorted with
   `ruff`, as well.
1. TOML files should be formatted using [taplo].
1. The test runner should be [nox].

These requirements are not too uncommon. I have seen many projects with similar
setup, with alternatives such as [tox] instead of [nox], or [black] and [pylint]
instead of [ruff], etc.

Let us look at the typical Python project structure, for example our dummy
`zamazingo` Python package with a command-line interface (CLI):

```dat
.
├── noxfile.py
├── pyproject.toml
├── README.md
├── tests
│   ├── __init__.py
│   ├── test_cli.py
│   └── test_utils.py
└── zamazingo
    ├── cli.py
    ├── __init__.py
    ├── resources
    │   ├── help.txt
    │   ├── __init__.py
    └── utils.py
```

The structure is pretty standard, with the `tests` directory containing the
tests, and the `zamazingo` directory containing the package code. The
`resources` package contains the package data, which will be included in the
package during packaging.

I will not go into the details of the test suite, but here is the output of
`nox --list` for a quick look at what we can do with it:

```console
$ nox --list
Test suite.

A few examples for invoking `nox`:

    nox --list
    nox
    nox -s taplo
    nox -s taplo -- --fix
    nox -s format
    nox -s format -- --fix
    nox -s check
    nox -k "not pytest"
    nox -k "not mypy and not pytest"

Sessions defined in /app/noxfile.py:

* taplo -> Lint and check format of TOML files in the repository.
* format -> Format codebase and sort imports.
* check -> Lint and check codebase.
* mypy -> Type-check on the codebase.
* pytest -> Run the test suite.

sessions marked with * are selected, sessions marked with - are skipped.
```

Now, the `pyproject.toml` file is the most important part of this setup:

```toml
[project]
name = "zamazingo"
version = "0.0.0"
description = "Yet Another Python CLI Application"
readme = "README.md"
requires-python = ">=3.11"
license = "MIT"
dependencies = ["click"]

[project.optional-dependencies]

test = ["mypy", "nox", "pytest", "ruff"]

[project.scripts]

zamazingo = "zamazingo.cli:main"

[build-system]

requires = ["setuptools"]
build-backend = "setuptools.build_meta"

[tool.setuptools.packages.find]

where = ["."]
include = ["zamazingo*"]
exclude = ["tests*"]

[tool.setuptools.package-data]

zamazingo = ["resources/*"]

[tool.pytest.ini_options]

addopts = ["--doctest-modules"]
testpaths = ["tests", "zamazingo"]

[tool.ruff]

required-version = ">=0.11.7"
fix = false

[tool.ruff.lint]

select = [
  "AIR",   ## Airflow
  "ERA",   ## eradicate
  "FAST",  ## FastAPI
  "YTT",   ## flake8-2020
  ## ... TRUNCATED FOR BREVITY ...
  "UP",    ## pyupgrade
  "FURB",  ## refurb
  "RUF",   ## Ruff-specific rules
  "TRY",   ## tryceratops
]
ignore = ["COM812"]
```

I have omitted some configuration in this example to keep it short, but you can
find the full configuration in the template repository.

We have a pretty much standard `pyproject.toml` file here that includes some
tooling configuration. Package data (such as text files or assets) are included
using the `[tool.setuptools.package-data]` section. Note the `[build-system]`
section, which is going to affect how we will configure Nix.

## Nix Part

We have some requirements for the Nix configuration as well:

1. The Nix configuration should be a Nix flake.
1. Our package should be packaged as a proper Nix derivation.
1. The `checkPhase` of our Nix derivation should run the test suite.
1. The package should be Nix-installable.
1. And most importantly, there should be a Nix shell that can be used for
   development and testing, including LSP support.

I was always confused about the last point. Over the years, I used different
structures for the Nix shell configuration of my Python projects. Finally, I
have settled on a structure that I find easy to understand and maintain. And
this is the structure I will be using here.

The challenge was this: How do we manage dependencies for different scopes?

There are at least three scopes we need to consider:

1. **Production Dependencies:** These are the dependencies that are required for
   the package to run. These must be specified in the `[project]` section of the
   `pyproject.toml` file. Nix does not automatically pick up the dependencies
   from the `pyproject.toml` file, so we need to specify them explicitly in the
   Nix expression.
2. **Testing Dependencies:** These are the dependencies that are required for
   testing the package. These may or may not be specified in the
   `[project.optional-dependencies]` section of the `pyproject.toml` file.
   Again, these are not automatically translated into Nix dependencies.
3. **Development Dependencies:** These are the dependencies that are required
   for development. Like the testing dependencies, these may or may not be
   specified in the `[project.optional-dependencies]` section of the
   `pyproject.toml` file, and must be specified explicitly in the Nix
   expression.

When I say that Nix does not automatically pick up the dependencies from the
`pyproject.toml` file, I am not telling the whole truth. Tools like [poetry2nix]
and some community functions can help you with this, but I do not want to rely
on them. I want a vanilla solution.

In our setup, we will use `buildPythonPackage` to build the package with tests
running in the `checkPhase`. We will use `mkPythonEditablePackage` to instruct
Nix to install the package in editable mode. This is important for development
and testing, as it allows us to make changes to the code and see the effects
immediately.

Here is our Nix flake:

```nix
{
  description = "A Python Package";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ## Import nixpkgs:
        pkgs = import nixpkgs { inherit system; };

        ## Read pyproject.toml file:
        pyproject = builtins.fromTOML (builtins.readFile ./pyproject.toml);

        ## Get project specification:
        project = pyproject.project;

        ## Get the package:
        package = pkgs.python3Packages.buildPythonPackage {
          ## Set the package name:
          pname = project.name;

          ## Inherit the package version:
          inherit (project) version;

          ## Set the package format:
          format = "pyproject";

          ## Set the package source:
          src = ./.;

          ## Specify the build system to use:
          build-system = with pkgs.python3Packages; [
            setuptools
          ];

          ## Specify test dependencies:
          nativeCheckInputs = [
            ## Python dependencies:
            pkgs.python3Packages.mypy
            pkgs.python3Packages.nox
            pkgs.python3Packages.pytest
            pkgs.python3Packages.ruff

            ## Non-Python dependencies:
            pkgs.taplo
          ];

          ## Define the check phase:
          checkPhase = ''
            runHook preCheck
            nox
            runHook postCheck
          '';

          ## Specify production dependencies:
          propagatedBuildInputs = [
            pkgs.python3Packages.click
          ];
        };

        ## Make our package editable:
        editablePackage = pkgs.python3.pkgs.mkPythonEditablePackage {
          pname = project.name;
          inherit (project) scripts version;
          root = "$PWD";
        };
      in
      {
        ## Project packages output:
        packages = {
          "${project.name}" = package;
          default = self.packages.${system}.${project.name};
        };

        ## Project development shell output:
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [
              package
            ];

            buildInputs = [
              #################
              ## OUR PACKAGE ##
              #################

              editablePackage

              #################
              # VARIOUS TOOLS #
              #################

              pkgs.python3Packages.build
              pkgs.python3Packages.ipython

              ####################
              # EDITOR/LSP TOOLS #
              ####################

              # LSP server:
              pkgs.python3Packages.python-lsp-server

              # LSP server plugins of interest:
              pkgs.python3Packages.pylsp-mypy
              pkgs.python3Packages.pylsp-rope
              pkgs.python3Packages.python-lsp-ruff
            ];
          };
        };
      });
}
```

Let us dissect this a bit.

Instead of manually typing relevant project metadata in the Nix expression, we
use the `builtins.fromTOML` function to read the `pyproject.toml` file and parse
it into `project` attribute set.

Then, we build the package using `buildPythonPackage`, which does the heavy
lifting for us. We set the `pname` and `version` attributes from the `project`
attribute as planned.

The format of our package is `pyproject`. As highlighted in the previous
section, we tell `buildPythonPackage` to use `setuptools` as the build system.

For testing, we set the `nativeCheckInputs` attribute to include the
dependencies we need for testing. These are the dependencies that are required
by our `nox` configuration. We also include the `taplo` dependency here, which
is not a Python package, but is required for linting and formatting TOML files.

The `checkPhase` is where we run the test suite, simply by calling `nox`.
Indeed, any optional dependencies you would specify in the `pyproject.toml` file
have no effect in the Nix setup, unless you plan to run tests in a non-Nix
environment.

Lastly, we set the `propagatedBuildInputs` attribute to include the production
dependencies. This is all about the package itself.

Let's get to the most tricky part, the Nix shell! Since we want to use the
package in editable mode, we are using `mkPythonEditablePackage` to create an
editable package descriptor. This is similar to installing a package in editable
mode via `pip install -e .`, allowing code changes to be picked up without
reinstallation, unless the package has to be rebuilt for some reason, such as
changing the script entry points. This is added as a `buildInput` in the
`devShell` output. The `buildInput` also includes various tools that we want to
use during development, such as `ipython` for more convenient interactive
development, `build` for building the package, and `python-lsp-server` for LSP
support along with some extra plugins such as `pylsp-mypy` for typechecking.

But the most important trick is to set the `inputsFrom` attribute to pull all
the production and testing dependencies from our actual package output and
include them in our Nix shell. This was always the missing piece for me: a way
to share dependencies between the Nix build and development shell cleanly. I was
using hacks like a virtual environment approach or `shellHook`s to setup the
environment.

As a bonus, the following `neovim` configuration works for me somewhat well:

```lua
lspconfig.pylsp.setup({
    settings = {
        pylsp = {
            plugins = {
                -- Disable default plugins:
                autopep8 = { enabled = false },
                flake8 = { enabled = false },
                mccabe = { enabled = false },
                pycodestyle = { enabled = false },
                pyflakes = { enabled = false },
                pylint = { enabled = false },
                yapf = { enabled = false },

                -- Enable and configure plugins of interest:
                rope_autoimport = { enabled = true },
                ruff = {
                    enabled = true,
                    extendSelect = { "I" },
                    format = { "I" },
                    formatEnabled = true,
                },
            },
        },
    },
})
```

The only thing I could not figure out is how to get auto-imports working with
`rope_autoimport.enabled = true` and `pylsp-rope` installed. Perhaps I am
missing some configuration.

OK, maybe one last caveat: If you want to maintain this package for non-Nix
environments, dependencies should be pinned to avoid breaking changes. Sometimes
pinned dependencies are not compatible with the versions in the `nixpkgs` set
you are using. In this case, overlays need to be used, or using `poetry2nix` or
similar tools may be a better option. I did not try the latter option yet.

## Conclusion

I have this setup materialized as a Nix flake template that you can use right
away:

```sh
nix flake init --template vst/nix-flake-templates#python-package
```

You can run the tests:

```sh
nix develop --command nox
```

... or run the package:

```sh
nix run . -- --name=there --count=3
```

Mind you, this is not the only way to set up a Nix environment for Python
development. First of all, there may be further requirements for your project
that may require tweaking this setup, or adopting an entirely different one.
Secondly, there are Nix functions out in the wild that can help you with this.
One example is [pyproject.nix]. I did not use it but studied it a bit. For now,
I am sticking to vanilla Nix.

<!-- REFERENCES -->

[black]: https://black.readthedocs.io/en/stable/
[click]: https://click.palletsprojects.com/
[mypy]: https://mypy.readthedocs.io/en/stable/
[nix-tmpl-post]: ../nix-flake-templates
[nox]: https://nox.thea.codes/en/stable/
[poetry2nix]: https://github.com/nix-community/poetry2nix
[pylint]: https://pylint.pycqa.org/en/latest/
[pyproject.nix]: https://github.com/pyproject-nix/pyproject.nix
[pyproject.toml]: https://peps.python.org/pep-0518/
[pytest]: https://docs.pytest.org/en/latest/
[ruff-post]: ../migrate-to-ruff
[ruff]: https://docs.astral.sh/ruff/
[setuptools]: https://setuptools.pypa.io/en/latest/
[taplo]: https://taplo.tamasfe.dev/
[tox]: https://tox.readthedocs.io/en/latest/
[vst-nix-repo]: https://github.com/vst/nix-flake-templates
