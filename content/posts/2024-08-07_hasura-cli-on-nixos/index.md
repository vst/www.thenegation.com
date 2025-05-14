---
title: "Hasura CLI on NixOS: A Working Solution"
date: 2024-08-07 22:02:10
description:
  A working solution to get Hasura CLI work on NixOS like any other program.
taxonomies:
  tags:
    - Technical Note
    - Nix
    - Hasura
    - Hacking
    - Computing
---

This post comes after an exciting discovery of a solution to a problem I and my
team have been facing for a while: Getting Hasura CLI work on NixOS like any
other program.

<!--more-->

## Problem

NixOS has its own way of managing packages and dependencies. Unlike a _typical_
Linux distribution, programs are installed in isolated environments called Nix
profiles. This is great for reproducibility and isolation, but it can be a bit
challenging when you want to use a program that is not available in the Nix
package set.

[Hasura] CLI proved to be one of these tricky programs due to its extensions
architecture or implementation.

Simply, `<nixpkgs>` has a very outdated package. Building a recent Hasura CLI
from source is fine, but users get runtime errors due to that the plugin system
does not work on NixOS.

I could not get to the source of the problem, but I found a workaround after
trying many options, including community members' hacks.

## Interim Solution

Two solutions worked for us although the first one was simply not practical to
use:

1. Use Hasura CLI from within a Docker container: This caused more problems then
   it solved.
2. Use `buildFHSUserEnv`: This solution worked for me and my team for quite some
   time, but it has its own issues such as a half-broken Nix shell where many
   commands do not work.

## A Working Solution

I discovered a [solution] by [@adamgoose] as a comment on a related [GitHub
issue]. It made sense and I gave it a try.

It was simple and it worked! I then decided to share it as a Nix function on
GitHub:

<https://github.com/vst/hasura-cli-nix>

You can try it in a Nix shell:

```nix
{ ... }:

let
  pkgs = import <nixpkgs> { };
  hasura-cli-nix = pkgs.fetchFromGitHub {
    owner = "vst";
    repo = "hasura-cli-nix";
    rev = "98e796e57a37ffe98233cb174e439e3e7657166d";
    sha256 = "sha256-SiOSip48jk1CbtcMmN79+ca6A3slS9XGffhZFTLoWYU=";
  };
  hasura-cli = (pkgs.callPackage hasura-cli-nix { }).cli;
in
pkgs.mkShell {
  buildInputs = [
    hasura-cli
  ];
}
```

... and:

```sh
$ nix-shell --run "hasura metadata diff"
```

## Wrap-Up

[Hasura] is a great tool. I was worried about a few things such as huge RAM
consumption, excessive focus on new features and functions despite many
outstanding issues, long time rewrite of the server in Rust, etc...

None of them were deal-breakers for me. But our Hasura CLI experience was quite
concerning and frustrating.

There is not a single program on my NixOS system that does not work as expected
except Hasura CLI. This exception is now resolved.

<!-- REFERENCES -->

[Hasura]: https://hasura.io
[@adamgoose]: https://github.com/adamgoose
[solution]:
  https://github.com/hasura/graphql-engine/issues/8441#issuecomment-2055727178
[GitHub issue]: https://github.com/hasura/graphql-engine/issues/8441
