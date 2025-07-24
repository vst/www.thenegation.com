---
title: "Build a CLI Emoji Picker with fzf and Nix"
date: 2025-05-12 21:10:27
description: >
  A hands-on tutorial showing how to build an emoji picker CLI using fzf and
  package it as a Nix flake.

slug: nix-fzf-script-tutorial
tags:
  - Technical Notes
  - Tutorial
  - Nix
---

In my blog post [yesterday], I mentioned [fzf]. Its simplicity and power make it
a good tool for many scripting tasks. In this post, we will see a practical
example of how to use it in a CLI program and package it with Nix.

<!--more-->

## `fzf` Part

[fzf] is a _"command-line fuzzy finder"_. But, its API offers a few features
that make it a small UI framework.

Let us see it in action.

Our job is to create a simple CLI program that lists all [GitHub Emojis], lets
the user search for one and copy either the selected emoji or its `:EMOJICODE:`
to the clipboard.

I found the list of emojis in the [github/gemoji] repository, in particular in
the following file:

```txt
https://github.com/github/gemoji/blob/master/db/emoji.json
```

To reference it below, we will assign its raw file URL to the environment
variable `FZF_EMOJI_DATA_FILE`:

```sh
export FZF_EMOJI_DATA_FILE=https://raw.githubusercontent.com/github/gemoji/0eca75db9301421efc8710baf7a7576793ae452a/db/emoji.json
```

It looks like this:

```console
$ curl -sSL "${FZF_EMOJI_DATA_FILE}" | jq . | head -n 15
[
  {
    "emoji": "😀",
    "description": "grinning face",
    "category": "Smileys & Emotion",
    "aliases": [
      "grinning"
    ],
    "tags": [
      "smile",
      "happy"
    ],
    "unicode_version": "6.1",
    "ios_version": "6.0"
  },
```

We want the prompt for each emoji to look like this:

```txt
🍇 :grapes: Food & Drink » grapes
```

... which is, as a [jq] expression:

```jq
.[] | (.emoji + " :" + .aliases[0] + ": "+ .category + " » " + .description)
```

Therefore, in one command, we can get the list of emojis with:

```sh
curl -sSL "${FZF_EMOJI_DATA_FILE}" |
  jq -r '.[] | (.emoji + " :" + .aliases[0] + ": "+ .category + " » " + .description)'
```

Now, the `fzf` part. It simply expects a list of search items on `stdin`, line
by line:

```sh
curl -sSL "${FZF_EMOJI_DATA_FILE}" |
  jq -r '.[] | (.emoji + " :" + .aliases[0] + ": "+ .category + " » " + .description)' |
  fzf
```

You can now search for emojis. If you hit `enter`, the selected line will be
printed to `stdout` and `fzf` will exit.

This is not very useful yet. Let us say:

1. If we hit `<ENTER>`, it should copy the selected emoji to the clipboard.
2. If we hit `<CTRL> + C`, it should copy the `:EMOJICODE:` to the clipboard.

This is achieved with the `--bind` option:

```sh
curl -sSL "${FZF_EMOJI_DATA_FILE}" |
  jq -r '.[] | (.emoji + " :" + .aliases[0] + ": "+ .category + " » " + .description)' |
  fzf \
    --delimiter " " \
    --bind 'enter:become(printf {1} | wl-copy --trim-newline)' \
    --bind 'ctrl-c:become(printf {2} | wl-copy --trim-newline)'
```

First, we added the `--delimiter` option to split the line into fields by the
space (` `) character. This way, we can use `{1}` and `{2}` to refer to the
first and second fields, respectively.

Then, we added two `--bind` options:

1. `enter:become(printf {1} | wl-copy --trim-newline)`:
   - `enter` is the key binding.
   - `become(...)` is a command to run when the key binding is pressed.
   - `printf {1}` prints the first field (the emoji).
   - `wl-copy --trim-newline` copies the emoji to the clipboard.
2. `ctrl-c:become(printf {2} | wl-copy --trim-newline)`:
   - `ctrl-c` is the key binding.
   - `become(...)` is a command to run when the key binding is pressed.
   - `printf {2}` prints the second field (the `:EMOJICODE:`).
   - `wl-copy --trim-newline` copies the `:EMOJICODE:` to the clipboard.

Note that we are using `wl-copy` to copy to the clipboard. I am on Wayland, so
if you are on X, you can use `xclip` or `xsel` instead.

Done!

## Nix Part

We want to package the above solution as an executable using Nix. Once
installed, we should be able to run the command as `fzf-emoji` and get the same
behavior as above.

One thing you might have noticed is the waiting time when you run the command:
`curl` must download the emoji list every time. We can fix this by building the
package with a copy of the emoji list. Going forward, each time we run the
program, it will use the local copy of the emoji list.

Let us start with the script which we will put inside the `script.sh` file:

```sh
#!/usr/bin/env bash

_jq_query='
  .[] | (.emoji + " :" + .aliases[0] + ": "+ .category + " » " + .description)
'

jq --raw-output "${_jq_query}" "${FZF_EMOJI_DATA_FILE}" |
  fzf \
    --delimiter " " \
    --bind 'enter:become(printf {1} | wl-copy --trim-newline)' \
    --bind 'ctrl-c:become(printf {2} | wl-copy --trim-newline)'
```

We define the `jq` query in a variable `_jq_query` so that we can use it in the
`jq` command. We also use the `--raw-output` option to get the output without
quotes. The data file is passed as an environment variable,
`FZF_EMOJI_DATA_FILE`. The rest is the same as before.

The Nix part is a simple flake:

```nix
{
  description = "fzf-emoji - A simple emoji picker using fzf";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emojis = {
      url = "https://raw.githubusercontent.com/github/gemoji/refs/heads/master/db/emoji.json";
      flake = false;
    };
  };

  outputs = { flake-utils, nixpkgs, emojis, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        program = pkgs.writeShellApplication {
          name = "fzf-emoji";
          text = builtins.readFile ./script.sh;
          runtimeEnv = {
            FZF_EMOJI_DATA_FILE = emojis;
          };
          runtimeInputs = with pkgs; [
            bash
            fzf
            jq
            wl-clipboard
          ];
        };
      in
      {
        packages = rec {
          fzf-emoji = program;
          default = fzf-emoji;
        };
      }
    );
}
```

Assuming that you are familiar with Nix Flakes, let us focus on the important
parts.

The first thing to notice is the `emojis` input. This is a raw URL to the emoji
list. This input is not a flake, hence the `flake = false` option. We can then
refer to it in the output as `emojis`.

Our program is a shell application, so we use the `writeShellApplication`
function provided by `nixpkgs`. We pass the following arguments:

- `name`: the name of the program.
- `text`: the content of the script. We use `builtins.readFile` to read the
  script from the file `script.sh` we created earlier.
- `runtimeEnv`: the environment variables to set when running the program. We
  set the `FZF_EMOJI_DATA_FILE` variable to the `emojis` input that is a Nix
  store path of the emoji list.
- `runtimeInputs`: the dependencies of the program. We need `bash`, `fzf`, `jq`,
  and `wl-clipboard` to run the program.

Finally, we define the package as `fzf-emoji` and set it as the default package.

You can now build the package with:

```sh
nix build
```

... or run it with:

```sh
nix run
```

If you put it in a GitHub repository, you can use the following command to run
it:

```sh
nix run github:yourusername/yourrepo
```

... or install it with:

```sh
nix profile install github:yourusername/yourrepo
```

... so that you can run it as `fzf-emoji` in your shell.

## Homework Part

I already put this in a GitHub repository, so you can check it out here:

[https://github.com/vst/fzf-emoji](https://github.com/vst/fzf-emoji)

But there are a few things you can do to improve it. Here are a few assignments.

### 1. Add `-v` option to the program

`fzf-emoji` should accept a `-v` option to print the version of the program and
then exit:

```console
$ fzf-emoji -v
fzf-emoji v1.2.3
```

I suggest that you define the version in the flake file as a runtime environment
variable and use it in the script.

### 2. Use `mkDerivation` instead of `writeShellApplication`

The `writeShellApplication` function is a convenience function that creates a
shell application (see its [definition]). Indeed, it is a wrapper around
`mkDerivation`. As an exercise, you can try using `mkDerivation` directly.
Otherwise, you will need to use `writeShellApplication`'s `derivationArgs`
argument for further customization.

### 3. Pin `emoji` input to a specific commit

Instead of using the raw URL, you can use a specific commit of the
`github/gemoji` repository. By doing this, you can ensure that the emoji list
always has the same structure. Otherwise, if the structure changes or the file
is moved or deleted, the build will fail.

While doing so, you can use the `github:github/gemoji` syntax to refer to the
repository. If you do this, I suggest that you copy the file to the Nix build
output, under `share/emoji.json`. This will keep all the files of the program
under a single Nix store path. This is usually done during the `installPhase` of
the `mkDerivation` function.

### 4. Add a `man` page

This might be tricky, but the outcome is rewarding.

Writing `man` pages is not easy. Luckily, there is a tool called [ronn] that can
help you with that. It converts a Markdown file to a `man` page. The only thing
you need to do is to write the Markdown file with the right sections and markup.
Then, you can put the `man` file in the `share/man/man1` directory of the
package. This is usually done during the `installPhase` or `postInstall` phase
of the `mkDerivation` function.

<!-- REFERENCES -->

[yesterday]: https://thenegation.com/posts/wayland-app-launchers-rofi/
[GitHub Emojis]:
  https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#using-emojis
[github/gemoji]: https://github.com/github/gemoji
[fzf]: https://github.com/junegunn/fzf
[definition]:
  https://github.com/NixOS/nixpkgs/blob/9a79275905bb533af304d51dd770aca23f0b1ace/pkgs/build-support/trivial-builders/default.nix#L245
[jq]: https://stedolan.github.io/jq/
[ronn]: https://github.com/apjanke/ronn-ng
