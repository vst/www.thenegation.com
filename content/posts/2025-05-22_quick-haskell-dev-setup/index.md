---
title: "Hacking Haskell with Nix: Two Tricks"
date: 2025-05-22 22:10:37
description: >-
  Two tricks to quickly hack a Haskell script or even a minimal but complete
  Haskell project using Nix.
taxonomies:
  tags:
    - Technical Notes
    - Hacking
    - Haskell
    - Nix
---

I have mentioned a few times in my posts that [Haskell] is my go-to language.
This is true even for small applications which go beyond a simple shell script.

Hacking Haskell with Nix is an easy and fun way to quickly prototype. I want to
share two tricks that I use.

<!-- more -->

## Nix-Based Haskell Scripting

If you are using [Nix], you may have heard of [Nix-Shell Shebang]:

```python
#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3 -p python3Packages.click

import click

@click.command()
@click.option('--count', default=1, help='Number of greetings.')
@click.option('--name', prompt='Your name', help='The person to greet.')
def hello(count, name):
    """Simple program that greets NAME for a total of COUNT times."""
    for x in range(count):
        click.echo(f"Hello {name}!")

if __name__ == '__main__':
    hello()
```

You can `chmod +x script.py` and run it directly:

```console
$ ./script.py --count 3 --name "John Doe"
Hello John Doe!
Hello John Doe!
Hello John Doe!
```

This is a brilliant hack allowing you to not only pick the interpreter for your
script, but also add dependencies to the shebang line.

Let us see how the same approach works with Haskell:

```haskell
#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (hask: [ hask.aeson hask.bytestring ])"

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Exit (die)

main :: IO ()
main = do
  jsonData <- BLC.getContents
  case Aeson.decode jsonData of
    Just value -> tellJsonType value
    Nothing -> die "Invalid JSON!"

tellJsonType :: Aeson.Value -> IO ()
tellJsonType (Aeson.Object _) = putStrLn "JSON value is an object."
tellJsonType (Aeson.Array _) = putStrLn "JSON value is an array."
tellJsonType (Aeson.String _) = putStrLn "JSON value is a string."
tellJsonType (Aeson.Number _) = putStrLn "JSON value is a number."
tellJsonType (Aeson.Bool _) = putStrLn "JSON value is a boolean."
tellJsonType Aeson.Null = putStrLn "JSON value is null."
```

Same as before, you can `chmod +x script.hs` and run it directly:

```console
$ echo '{}' | ./script.hs
JSON value is an object.
```

Granted, this approach does not scale well, as you must manually pull in all
development dependencies into a Nix shell, such as `cabal`,
`haskell-language-server`, `fourmolu`, etc.

If that is the case, why not use a proper but minimal Haskell project template?

## Minimal Haskell Project Template

I use `cabal`. A simple, `cabal`-based Haskell package needs only two files:

1. `Main.hs`
2. `haskell-simple-app.cabal`

Let us say that we are using the previous Haskell program, but without the Nix
shebang:

```haskell
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Exit (die)

main :: IO ()
main = do
  jsonData <- BLC.getContents
  case Aeson.decode jsonData of
    Just value -> tellJsonType value
    Nothing -> die "Invalid JSON!"

tellJsonType :: Aeson.Value -> IO ()
tellJsonType (Aeson.Object _) = putStrLn "JSON value is an object."
tellJsonType (Aeson.Array _) = putStrLn "JSON value is an array."
tellJsonType (Aeson.String _) = putStrLn "JSON value is a string."
tellJsonType (Aeson.Number _) = putStrLn "JSON value is a number."
tellJsonType (Aeson.Bool _) = putStrLn "JSON value is a boolean."
tellJsonType Aeson.Null = putStrLn "JSON value is null."
```

Our script depends on two libraries: `aeson` and `bytestring`. Then, the `cabal`
file looks like this:

```cabal
cabal-version: 2.4
name:          haskell-simple-app
version:       0.0.0.1
license:       MIT
author:        Your Name
maintainer:    you@example.com
synopsis:      Minimal single-file Haskell executable
category:      Utility
build-type:    Simple

executable haskell-simple-app
  main-is:          Main.hs
  hs-source-dirs:   .
  ghc-options:      -Wall
  build-depends:
    , aeson
    , base        >=4.14 && <5
    , bytestring

  default-language: Haskell2010
```

I am not explaining the `cabal` file here, but you should get the general idea:
A set of metadata followed by an `executable` section that describes how to
build an executable with its dependencies.

In addition to the above two files, how about a [Nix Flakes] to develop and package
this project as a Nix program?

```nix
{
  description = "A Simple Application in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        thisHaskell = pkgs.haskellPackages.override {
          overrides = self: super: {
            haskell-simple-app = self.callCabal2nix "haskell-simple-app" ./. { };
          };
        };

        thisShell = thisHaskell.shellFor {
          packages = p: [ p.haskell-simple-app ];
          withHoogle = true;
          buildInputs = [
            ## Haskell related build inputs:
            thisHaskell.apply-refact
            thisHaskell.cabal-fmt
            thisHaskell.cabal-install
            thisHaskell.cabal2nix
            thisHaskell.fourmolu
            thisHaskell.haskell-language-server
            thisHaskell.hlint

            ## Other build inputs for various development requirements:
            pkgs.nil
            pkgs.nixpkgs-fmt
          ];
        };

        thisPackage = pkgs.haskell.lib.justStaticExecutables thisHaskell.haskell-simple-app;
      in
      {
        packages = {
          haskell-simple-app = thisPackage;
          default = self.packages.${system}.haskell-simple-app;
        };

        devShells = {
          default = thisShell;
        };
      });
}
```

If you are familiar with Nix and [Nix Flakes], this should be straightforward:

1. We create our Nix Haskell package set, `thisHaskell`, with our project
   `haskell-simple-app` using `callCabal2nix`.
2. We create a Nix shell, `thisShell`, with our project and a few other Haskell
   development tools.
3. We create a Nix package, `thisPackage`, with our project built as a static
   executable (not really statically linked, but the Nix closure cleared).

With all these three files in place, we can then:

```console
$ echo '{}' | nix run
JSON value is an object.
```

You can do anything you want with this setup, including installing the program
into your Nix profile and use it from anywhere on your system:

```console
$ nix profile install .#haskell-simple-app
$ echo '{}' | haskell-simple-app
JSON value is an object.
```

## Conclusion

I wanted to share with you two ways to quickly hack a Haskell script or even a
minimal but complete Haskell project using Nix.

I kept these as snippets here and there. Today, I added the `haskell-simple-app`
template to [my Nix Flakes Templates repository]. I am planning to refine it
further, but you can use it now to create a new Haskell project with Nix:

```console
nix flake init --template github:vst/nix-flake-templates#haskell-simple-app
```

Or even run this template straight away without creating a new project:

```console
echo '{}' | nix run "github:vst/nix-flake-templates?dir=templates/haskell-simple-app" --no-write-lock-file
```

Still, why would you do that instead of creating a proper project?

<!-- REFERENCE -->

[Nix]: https://nixos.org/
[Nix-Shell Shebang]: https://wiki.nixos.org/wiki/Nix-shell_shebang
[Nix Flakes]: https://wiki.nixos.org/wiki/Flakes
[Haskell]: https://www.haskell.org/
[my Nix Flakes Templates repository]: https://github.com/vst/nix-flake-templates
