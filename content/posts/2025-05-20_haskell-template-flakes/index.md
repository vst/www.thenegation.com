---
title: "Haskell Project Template with Nix Flakes"
date: 2025-05-20 21:43:46
description: >
  This post demonstrates my Haskell project template with Nix Flakes. It is a
  simple template that I use to quickly spin up new Haskell projects.
taxonomies:
  tags:
    - Technical Notes
    - Haskell
    - Nix
---

This post introduces my Haskell project template powered by Nix Flakes -- a
simple setup I use to quickly spin up new Haskell applications.

<!-- more -->

[Haskell] has been my go-to language for over 7 years. First, I started with
[Stack], then switched to plain [Cabal] and finally settled on [Nix] to
provision a development environment for Haskell projects.

Since I primarily develop applications rather than libraries, I find it more
practical to structure my development environment and builds around producing
standalone Nix packages, statically built executables and container images using
Nix.

Nix offers several approaches for working with Haskell. In my experience, the
three most prominent are:

1. [The Haskell infrastructure in Nixpkgs][nixpkgs-haskell] -- simple and
   built-in.
2. [Haskell.nix] -- a more flexible but complex option from [IOHK].
3. [haskell-flake] -- a recent and modular setup based on [flake-parts].

I tried the second option a few years ago but have not tried the third one. I
have been using the first one, using classic Nix. Only recently, I have
refactored some of my projects to use [Nix Flakes]. Today, I also refactored my Haskell
project [template] to use Flakes.

My plan is to invest some time in _flake-parts_ and [Horizon Haskell]. In this post,
I will simply go through my template's `flake.nix` file and show what it does.

The template may evolve, but the examples in this post reference a [pinned
version][template-pinned] for consistency. Also note that the template is a
GitHub Repository Template that you can use to create a new repository with the
same structure. Check out the `run-template.sh` script that will help you update
template variables.

## The flake.nix

Here is the `flake.nix` file:

```nix
{
  description = "Haskell Project Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ## Import nixpkgs:
        pkgs = import nixpkgs { inherit system; };

        ## Load readYAML helper:
        readYAML = pkgs.callPackage ./nix/read-yaml.nix { };

        ## Read package information:
        package = readYAML ./package.yaml;

        ## Get our Haskell:
        thisHaskell = pkgs.haskellPackages.override {
          overrides = self: super: {
            ${package.name} = self.callCabal2nix package.name ./. { };
          };
        };

        ## Prepare dev-test-build script:
        dev-test-build = pkgs.writeShellApplication {
          name = "cabal-dev-test-build";
          text = builtins.readFile ./nix/dev-test-build.sh;
          runtimeInputs = [ pkgs.bash pkgs.bc pkgs.moreutils ];
        };

        ## Prepare Nix shell:
        thisShell = thisHaskell.shellFor {
          ## Define packages for the shell:
          packages = p: [ p.${package.name} ];

          ## Enable Hoogle:
          withHoogle = false;

          ## Build inputs for development shell:
          buildInputs = [
            ## Haskell related build inputs:
            thisHaskell.apply-refact
            thisHaskell.cabal-fmt
            thisHaskell.cabal-install
            thisHaskell.cabal2nix
            thisHaskell.fourmolu
            thisHaskell.haskell-language-server
            thisHaskell.hlint
            thisHaskell.hpack
            thisHaskell.weeder

            ## Our development scripts:
            dev-test-build

            ## Other build inputs for various development requirements:
            pkgs.docker-client
            pkgs.git
            pkgs.nil
            pkgs.nixpkgs-fmt
            pkgs.nodePackages.prettier
            pkgs.upx
          ];
        };

        thisPackage = pkgs.haskell.lib.justStaticExecutables (
          thisHaskell.${package.name}.overrideAttrs (oldAttrs: {
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [
              pkgs.git
              pkgs.makeWrapper
              pkgs.ronn
            ];

            postFixup = (oldAttrs.postFixup or "") + ''
              ## Create output directories:
              mkdir -p $out/{bin}

              ## Wrap program to add PATHs to dependencies:
              wrapProgram $out/bin/${package.name} --prefix PATH : ${pkgs.lib.makeBinPath []}
            '';
          })
        );

        thisDocker = pkgs.dockerTools.buildImage {
          name = "${package.name}";
          tag = "v${package.version}";
          created = "now";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ pkgs.cacert ];
            pathsToLink = [ "/etc" ];
          };

          runAsRoot = ''
            #!${pkgs.runtimeShell}
            ${pkgs.dockerTools.shadowSetup}
            groupadd -r users
            useradd -r -g users patron
          '';

          config = {
            User = "patron";
            Entrypoint = [ "${thisPackage}/bin/${package.name}" ];
            Cmd = null;
          };
        };
      in
      {
        ## Project packages output:
        packages = {
          "${package.name}" = thisPackage;
          docker = thisDocker;
          default = self.packages.${system}.${package.name};
        };

        ## Project development shell output:
        devShells = {
          default = thisShell;
        };
      });
}
```

The 125-line `flake.nix` file might look a bit overwhelming at first, but it is
actually quite simple considering that this file gives us the following:

1. A Nix-installable package,
2. A Docker image with the package application as the entrypoint, and
3. A development shell with all the development dependencies.

There are some placeholders in the file, such as building `man` pages with
`ronn` during the package's `postFixup` phase. I am not using this feature in
all of my projects, but it comes in handy when I need it. Likewise, we can add
extra `bin` paths to the `PATH` variable in the `postFixup` phase for system
binaries.

Let us go over notable parts and features of the file.

First of all, we do not refer to the name of the package. Instead, we load the
`package.yaml` file and read the name and version from it. Note that the
`readYAML` function is a simple helper I manage in my `nix` directory as there
is no built-in for reading YAML files in Nix yet:

```nix
readYAML = pkgs.callPackage ./nix/read-yaml.nix { };
package = readYAML ./package.yaml;
```

Then, we create a Haskell package set with our package included:

```nix
thisHaskell = pkgs.haskellPackages.override {
  overrides = self: super: {
    ${package.name} = self.callCabal2nix package.name ./. { };
  };
};
```

This is used when we create the development shell:

```nix
thisShell = thisHaskell.shellFor {
  packages = p: [ p.${package.name} ];
  buildInputs = [
    ## ...
  ];
};
```

... and when we create the package:

```nix
thisPackage = pkgs.haskell.lib.justStaticExecutables (
  thisHaskell.${package.name}.overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [
      ## ...
    ];

    postFixup = (oldAttrs.postFixup or "") + ''
      ## ...
    '';
  })
);
```

Finally, we build a Docker image with the package as the entrypoint:

```nix
thisDocker = pkgs.dockerTools.buildImage {
  ## ...
  config = {
    User = "patron";
    Entrypoint = [ "${thisPackage}/bin/${package.name}" ];
    Cmd = null;
  };
};
```

The tools I include in this `flake.nix` file come in handy, especially with
other features of this template. For example, I included a
`cabal dev-test-build` script that helps me build the package, run tests,
linters and formatters. Additionally, and, probably most importantly, I have a
script to statically build the application in a Docker image and compress it
with `upx` to reduce the size of the final binary.

I am pretty sure that there are many better ways to do what I do in this
template repository, but I am happy using it to quickly spin up a new Haskell
project. There are quite a few other things and methods I use in real-life
projects, such as multi-package builds and [GitHub Release Please Action] integration,
which are quite easy to add upon this template.

If you find this template useful, consider starring the [repository][template]
or bookmarking it for future reference. You might also want to check out my new
[Nix Flake Templates] collection, where I plan to share more Haskell setups as
my use cases expand.

<!-- REFERENCE -->

[Haskell]: https://www.haskell.org
[Stack]: https://docs.haskellstack.org
[Cabal]: https://www.haskell.org/cabal
[Nix]: https://nixos.org
[Docker]: https://www.docker.com
[nixpkgs-haskell]: https://nixos.org/manual/nixpkgs/stable/#haskell
[Haskell.nix]: https://input-output-hk.github.io/haskell.nix/
[haskell-flake]: https://community.flake.parts/haskell-flake
[flake-parts]: https://flake.parts
[Horizon Haskell]: https://horizon-haskell.net/
[Nix Flakes]: https://wiki.nixos.org/wiki/Flakes
[template]: https://github.com/vst/haskell-template-hebele
[template-pinned]:
  https://github.com/vst/haskell-template-hebele/tree/5ed842cfe9538a9e3d935ac2fd0d039b3c37080c
[GitHub Release Please Action]:
  https://github.com/googleapis/release-please-action
[Nix Flake Templates]: https://github.com/vst/nix-flake-templates
[IOHK]: https://iohk.io
