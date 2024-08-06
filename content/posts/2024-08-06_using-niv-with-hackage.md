---
title: "Using niv to Manage Haskell Dependencies"
date: 2024-08-06 20:31:10
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - Nix
    - Hacking
    - Computing
---

Using Nix to manage project dependencies and development environments is a great
way to keep your projects reproducible and isolated. [niv] can help you further
in this by pinning the versions of your dependencies outside of your Nix code as
JSON data.

In this post, I will show you what using [niv] looks like and how to override
Haskell dependencies. I will also demo a small script I wrote to add Hackage
packages to the `sources.json` file.

<!-- more -->

## What is niv?

[niv] a command-line program that streamlines the process of adding, updating or
dropping dependencies in a Nix project. It generates two files upon first use:

1. `nix/sources.json` - a simple JSON file containing the pinned versions of and
   some metadata about the dependencies.
2. `nix/sources.nix` - a Nix module that imports the JSON file and provides an
   attribute set of the dependencies.

The program itself is needed only when you want to work on the JSON file.

Let's see it in action:

```sh
$ mkdir my-nix-shell

$ cd my-nix-shell

$ niv init
Initializing
  Creating nix/sources.nix
  Creating nix/sources.json
  Using known 'nixpkgs' ...
  Adding package nixpkgs
    Writing new sources file
  Done: Adding package nixpkgs
Done: Initializing

$ ls nix
sources.json  sources.nix

$ cat nix/sources.json
{
    "nixpkgs": {
        "branch": "nixos-unstable",
        "description": "Nix Packages collection",
        "homepage": null,
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "6c43a3495a11e261e5f41e5d7eda2d71dae1b2fe",
        "sha256": "16f329z831bq7l3wn1dfvbkh95l2gcggdwn6rk3cisdmv2aa3189",
        "type": "tarball",
        "url": "https://github.com/NixOS/nixpkgs/archive/6c43a3495a11e261e5f41e5d7eda2d71dae1b2fe.tar.gz",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"
    }
}

$ niv update nixpkgs --branch nixos-24.05
Update nixpkgs
Done: Update nixpkgs

$ cat nix/sources.json
{
    "nixpkgs": {
        "branch": "nixos-24.05",
        "description": "Nix Packages collection",
        "homepage": null,
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "8b5b6723aca5a51edf075936439d9cd3947b7b2c",
        "sha256": "0r9a5p748wj5lkpipy6r03d0lqzyv56krcf26l1367xg4nafn95c",
        "type": "tarball",
        "url": "https://github.com/NixOS/nixpkgs/archive/8b5b6723aca5a51edf075936439d9cd3947b7b2c.tar.gz",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"
    }
}

$ cat <<EOF > shell.nix
{ ... }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.figlet
  ];
}
EOF

$ nix-shell --pure --run "echo niv | figlet"
[... truncated ...]
       _
 _ __ (_)_   __
| '_ \| \ \ / /
| | | | |\ V /
|_| |_|_| \_/
```

## Using niv with Hackage

Personally, I have been using [niv] since almost I started using Nix regardless
the size of the Nix project: my NixOS configuration, codebases, or even small
scripts. I am simply pinning and pulling everything I need via [niv].

Haskell projects are no exception.

Sometimes, `nixpkgs` does not have the Haskell package version I need. In this
case, I override the package with a custom one in the Haskell package set. I use
[niv] to pin the version of the package I want to use. If I want to add it from
GitHub, that is easy:

```sh
$ niv add haskell-github-trust/smtp-mail --rev 51dea96ef7d743cba75cbba6b40ef1655ef99223
Adding package smtp-mail
  Writing new sources file
Done: Adding package smtp-mail

$ niv show smtp-mail
smtp-mail
  branch: master
  description: Simple email sending via SMTP
  homepage: https://hackage.haskell.org/package/smtp-mail
  owner: haskell-github-trust
  repo: smtp-mail
  rev: 51dea96ef7d743cba75cbba6b40ef1655ef99223
  sha256: 06ln6nzhqabqhhaqrlah36ibp9rs9lbnrr197i23lvhmvfiqd3c0
  type: tarball
  url: https://github.com/haskell-github-trust/smtp-mail/archive/51dea96ef7d743cba75cbba6b40ef1655ef99223.tar.gz
  url_template: https://github.com/<owner>/<repo>/archive/<rev>.tar.gz
```

If I want to add it from Hackage, I can use the attributes and URL templates:

```sh
$ niv add \
  --template "https://hackage.haskell.org/package/<name>-<version>/<name>-<version>.tar.gz" \
  --attribute homepage="https://hackage.haskell.org/package/autodocodec" \
  --attribute name="autodocodec" \
  --version 0.4.1.0 autodocodec
Adding package autodocodec
  Writing new sources file
Done: Adding package autodocodec

$ nix show autodocodec
autodocodec
  homepage: https://hackage.haskell.org/package/autodocodec
  name: autodocodec
  sha256: 0fdfcp8zcbkkmrm778kxlrj7fj9h0ixjka3ks58mlnz7wxs1n18k
  type: tarball
  url: https://hackage.haskell.org/package/autodocodec-0.4.1.0/autodocodec-0.4.1.0.tar.gz
  url_template: https://hackage.haskell.org/package/<name>-<version>/<name>-<version>.tar.gz
  version: 0.4.1.0
```

Once we have these dependencies, we can use them in our Nix expressions:

```nix
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  ghcOverrides = self: super: {
    autodocodec = self.callCabal2nix "autodocodec" "${sources.autodocodec}" { };
    smtp-mail = self.callCabal2nix "smtp-mail" "${sources.smtp-mail}" { };
  };
in
{
  # ...
}
```

I like that JSON file contains metadata about the dependencies. It is useful if
I want to know more about the dependencies I am using, such as a link to the
homepage. Hackage has all this information. Unfortunately, the `niv` command
does not have first class support for Hackage.

## Dirty Hackage Support for niv

I wrote a small and dirty script that can add a Hackage package to the
`sources.json` file with all the information available on Hackage as JSON. This
script is available as a [gist].

Let's use it:

```sh
$ bash niv-add-hackage.sh autodocodec-openapi3
Adding package autodocodec-openapi3
  Writing new sources file
Done: Adding package autodocodec-openapi3

$ niv show autodocodec-openapi3
autodocodec-openapi3
  author: Tom Sydney Kerckhove
  copyright: 2021-2022 Tom Sydney Kerckhove
  description:
  homepage: https://github.com/NorfairKing/autodocodec#readme
  license: MIT
  name: autodocodec-openapi3
  sha256: 10yrfgqwblbb516920m19wjk7lhxin6nf0accaf3xdxbkz71k01n
  synopsis: Autodocodec interpreters for openapi3
  type: tarball
  uploaded_at: 2024-07-29T14:54:16.685736428Z
  url: https://hackage.haskell.org/package/autodocodec-openapi3-0.2.1.4.tar.gz
  url_template: https://hackage.haskell.org/package/<name>-<version>.tar.gz
  version: 0.2.1.4
```

Now, I can see when the package was uploaded, who is the author, and what is the
license, etc... just by running `niv show` or looking at the `sources.json`
file.

As for updating the package to a newer version, I can simply `niv drop` first,
and `niv-add-hackage` again.

This should work fine until [niv] has first class support for Hackage.

## Wrap-Up

I am still not using Nix Flakes, and [niv] is one of the few reasons left for
me.

I like that I can pull almost anything remote into my project with it, and even
use a few shell tricks to introspect all my `sources.json` files across
projects.

<!-- REFERENCES -->

[gist]: https://gist.github.com/vst/3c16bbdd812f22f0e5dce72918b830f5
[niv]: https://github.com/nmattia/niv
