---
title: Develop R Packages under Nix Shell
date: 2024-01-05 08:22:10
description:
  A guide for creating, developing and testing R packages under a Nix Shell
  using R tools such as devtools, testthat and usethis.
slug: nix-r-package-guide
tags:
  - Computing
---

This is a guide for creating, developing and testing [R] packages under a [Nix]
Shell using R tools such as [devtools], [testthat] and [usethis].

<!--more-->

<!-- toc -->

[R] is a programming language and environment for statistical computing. I have
been using R since 2003, developed some R packages and published a few of them.
I am not using it on a regular basis anymore, but I noticed that various tools
emerged over time as _de-facto_ tools for creating and maintaining R packages.

[Nix], on the other hand, is a package manager that can be used to provision
development, testing and production environments in a reproducible manner.

In this guide, we will use Nix to provision a development environment for
creating an R package. Such an environment can then be used by other developers
to contribute to the package. Furthermore, it can be used for automated testing
(such as on CI/CD pipelines), packaging and even deploying solutions to
production environments as it is reproducible. It mainly helps with a huge class
of _"works on my machine"_ kind of problems.

The most important requirement for this guide is to have Nix installed on one's
workstation. The [official guide] should help.

Let's start...

## Nix Shell for Bootstrapping

We will use R and a particular set of packages to create our new R package.
Let's say that we do not have R installed on our workstation, or we do not want
to use the available R in this process.

A _Nix Shell_ is like[^1] a terminal session where we can define dependencies
and declare environment variables that will override our global system setup
without touching it. We can then persist our Nix Shell definition in a file and
distribute it so that such terminal session can be reproduced elsewhere with the
same dependencies and environment variables.

At this moment, we just need R with some packages to bootstrap our package.

It is very important to understand that the R provisioned by Nix Shell will not
have access to R packages installed system-wide or to user-specific library
sites. The advantage is that we will get an R setup only with the packages we
asked for with their pinned versions. The disadvantage is that we have to ask
for an R setup with its dependencies explicitly.

It is not that difficult, though. We need R with the following packages:

- [devtools]
- [testthat]
- [usethis]

Issue the following command to build and enter our (temporary) Nix shell:

```sh
nix-shell --packages 'rWrapper.override{ packages = [ rPackages.devtools rPackages.testthat rPackages.usethis ]; }'
```

It may take some time, but we will enter a shell where we can now launch our R
session:

```sh
R
```

The R version may be different than your globally installed R version. Also,
check your installed packages which will most likely be different and fewer in
number compared to your existing global R setup:

```R
rownames(installed.packages())
```

Good. Let's create the package.

## Initializing the R Project

`usethis` has a function to do this. Assuming that you will create an R package
with the name `hebele` under `./hebele` path:

```R
usethis::create_package(
  path = "./hebele",
  fields = list(
    Package = "hebele",
    Title = "Sample R Package Project Powered by Nix",
    Description = "This is a sample R package project accompanied by Nix artifacts for development and deployment purposes.",
    "Authors@R" = utils::person("Vehbi Sinan", "Tunalioglu", email = "vst@vsthost.com", role = c("aut", "cre")),
    URL = "https://github.com/vst/hebele",
    BugReports = "https://github.com/vst/hebele/issues"
  ),
  rstudio = FALSE,
  roxygen = TRUE,
  check_name = TRUE,
  open = FALSE
)
```

We have the skeleton of our project. Let's change to the working directory of
our package:

```R
usethis::proj_activate("./hebele")
```

In the next subsections, we will add some flesh to our project:

### README.md File

Create a `README.md` file:

```R
usethis::use_readme_md()
```

Note that this function will open a `README.md` file template using your
`$EDITOR` for you to edit it. Change it or do it later.

### NEWS.md File

Create a `NEWS.md` file:

```R
usethis::use_news_md()
```

Note that this function will open a `NEWS.md` file template using your `$EDITOR`
for you to edit it. Change it or do it later.

### LICENSE File and Descriptor

Create a `LICENSE` file as per MIT license:

```R
usethis::use_mit_license()
```

This will update your `DESCRIPTION` file, create `LICENSE` and `LICENSE.md`
files and add `LICENSE.md` to `.Rbuildignore` file.

### First R File and Definition

Let's create an R file that contains an R function to be exported by our
package:

```R
usethis::use_r("greeting.R")
```

Note that this function will open an empty R file using your `$EDITOR` for you
to edit it. Let's add the following content to it:

```R
#' Prepares greeting string.
#'
#' @param name Whom to greet.
#' @return A greeting.
#' @examples
#' hello()
#' hello("Birader")
#'
#' @export
hello <- function (name = "World") {
    paste0("Hello ", name, "!")
}
```

### First R Test File and Test Definition

To create a test file, issue the following statement:

```R
usethis::use_test("test-greeting")
```

Note that this function will open an R test file template using your `$EDITOR`
for you to edit it. Let's add the following content to it:

```R
test_that("hello works as expected", {
  expect_equal(hello(), "Hello World!")
  expect_equal(hello("birader"), "Hello birader!")
})
```

### Package Check

Now, we can use `devtools` to check our package:

```R
devtools::check(".")
```

> Note that you may get a NOTE about the `NEWS.md` file contents. This is normal
> as our `NEWS.md` does not have proper content yet and you will have to deal
> with it when you are about to release your package.

### Git Setup

Let's initialize the project as a Git repository and make our first commit. You
may wish to use [conventional commits] which you will later benefit much from:

```R
usethis::use_git(message = "chore: init repository")
```

## Adding Dependencies

By now, you should be able to load the package and call your definitions:

```R
devtools::load_all(".")
hello()
hello("birader")
```

Let's say, we want to create a new function, namely `helloStranger` which greets
a person with a random name. For this, we would like to use [randomNames]
library. But, we do not have it yet.

First, exit the R. Then, exit the Nix Shell. Create a new Nix Shell with the
added dependency:

```sh
nix-shell --packages 'rWrapper.override{ packages = [ rPackages.devtools rPackages.randomNames rPackages.testthat rPackages.usethis ]; }'
```

Then, run R:

```sh
R
```

Activate the project if you did not run R from within the package directory:

```R
usethis::proj_activate("./hebele")
```

Let's add the package to our project (`DESCRIPTION` to be specific):

```R
usethis::use_package("randomNames")
```

Now, add the following function to our `greeting.R` file (you can directly edit
`./R/greeting.R` file, or simply run `usethis::use_r("greeting.R")`):

```R
#' Prepares greeting string for a stranger.
#'
#' @return A greeting message with some random first/last name.
#' @examples
#' hello_stranger()
#'
#' @export
hello_stranger <- function () {
    hello(randomNames::randomNames(which.names="both", name.order="first.last", name.sep=" "))
}
```

Check your project to see if everything is in order:

```R
devtools::check(".")
```

Finally, you commit your changes:

```R
usethis::use_git(message = "feat: add hello_stranger function")
```

## Saving Nix Shell in a File

We better put our Nix Shell definition into a file and commit it to our Git
repository.

The name of the file is `shell.nix`, and the content is:

```sh
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-23.11") { }
, ...
}:

let
  ## Development dependencies:
  devDependencies = [
    pkgs.rPackages.devtools
    pkgs.rPackages.testthat
    pkgs.rPackages.usethis
  ];

  ## Production (package) dependencies:
  libDependencies = [
    pkgs.rPackages.randomNames
  ];

  ## Our R package with development and production dependencies:
  thisR = pkgs.rWrapper.override {
    packages = devDependencies ++ libDependencies;
  };
in
pkgs.mkShell {
  buildInputs = [
    ## Include our R package with its dependencies:
    thisR

    ## Any additional packages we want in our Nix Shell:
    pkgs.git
  ];
}
```

However, we need to exclude it from the R build process. Add the following line
to `.Rbuildignore`:

```gitignore
shell.nix
```

From now onwards, we do not need to specify any arguments on `nix-shell` command
as `nix-shell` command will find and read `shell.nix` in the directory it is
executed:

```sh
nix-shell
```

One thing to be noted: Every time we add a new dependency to our R package, we
need to add it to our `shell.nix` first, and then, add it to our package using
`usethis::use_package` function. Likewise, if we need a development dependency,
we will add it to our `shell.nix`.

## TODOs

The main motivation behind this post was to demonstrate how to bootstrap an R
package using a Nix Shell, nothing more. I left these out, but we could have
done following on top of what we have done here:

- Setup a [linter] for static analysis
- Setup a [code formatter] for checking and correcting the format of our code
- Setup [R Language Server]
- Setup GitHub Actions to build and check the package
- Setup GitHub Actions for automated releases[^2]

[^1]:
    It is much more than that, indeed. I just said so for the sake of our
    purpose.

[^2]:
    [Release Please] would be nice, but it does not officially support R
    language yet. However, it [may happen][RP R Issue] one day.

<!-- REFERENCES -->

[R]: https://www.r-project.org
[Nix]: https://en.wikipedia.org/wiki/Nix_package_manager
[official guide]: https://nixos.org/download.html
[devtools]: https://devtools.r-lib.org
[testthat]: https://testthat.r-lib.org
[usethis]: https://usethis.r-lib.org
[conventional commits]: https://www.conventionalcommits.org
[randomNames]: https://rdrr.io/cran/randomNames
[linter]: https://lintr.r-lib.org
[code formatter]: https://styler.r-lib.org
[R Language Server]: https://github.com/REditorSupport/languageserver
[Release Please]: https://github.com/googleapis/release-please
[RP R Issue]: https://github.com/googleapis/release-please/issues/2151
