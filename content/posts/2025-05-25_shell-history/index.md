---
title: "A Glimpse into My Shell"
date: 2025-05-25 23:20:42
description: >-
  A glimpse into my shell history reveals the tools I rely on daily.
taxonomies:
  tags:
    - Technical Notes
    - Hacking
---

A glimpse into my shell history reveals the tools I rely on daily. These are not
curated -- just raw, frequent commands logged over time.

<!-- more -->

Since I have exhausted all my curiosity, I ran a quick command on my terminal to
see which commands I use the most:

```sh
history 0 |
  awk '{print $2}' |
  sort |
  uniq -c |
  sort -n |
  tail -n 25 |
  tac
```

Here are the results:

```
    756 ls
    683 ssh
    596 gh
    507 git
    436 watson
    413 vim
    412 cat
    398 cd
    343 nix
    276 docker
    166 bash
    141 direnv
    139 ruff
    125 nox
    125 find
    123 curl
     93 nix-shell
     92 sudo
     92 devcli
     92 basecamp
     89 zenpolicy
     87 npm
     73 vst-nixos-switch
     71 cabal
     70 mv
```

If I were a carpenter, would these be my tools? Not really. I practically live
inside my terminal, where I run `tmux` and `vim` inside it, probably half a
dozen at a time. Inside `vim`, I use quite a few plugins, most notably `neogit`,
the `magit` replacement for `vim`.

Still, these 25 commands capture how I interact with my system or what I do on
my projects.

## Navigation

I do not have a GUI-based file manager on my system. Both `ls (756)` and
`cd (398)` are my primary tools for navigating the filesystem.

Another function of `ls` is that it is my fidget spinner. I use it to do nothing
while I am thinking about what to do next.

I am not using any `zsh` plugins like `z` or `autojump`. I prefer to use `cd`
and `ls` explicitly. Maybe I should give them a try.

## Core Tools

I record my life in `git (507)`. Since I am using GitHub -- not completely
willingly -- I use `gh (596)` quite often, but not with stock commands only. I
have quite a few aliases and extensions, including `gh watson` to track my time
in GitHub repositories.

`vim` is not a surprise here. It has been almost a year since I switched to
`neovim` from Emacs.

Another core tool is `nix (343)` and `nix-shell (93)`. I use Nix to manage both
my system and development environments. `direnv (141)` helps me to enter the Nix
development environment automatically when I `cd` into a directory with a
`.envrc` file. And `vst-nixos-switch (73)` is a custom command I use to deploy
my NixOS system.

I am quite surprised that I am using `docker (276)` so much. I used to depend on
Docker a lot on my workstation, but I have been using it less and less lately.
Well, not less enough to drop it from the top 25 commands, though.

Also, `ssh (683)` is my only tool for remote access. So, it is quite normal to
see it here. One thing I do with `ssh` is to run commands on remote hosts
without falling into an interactive shell, hence the high count.

## Programming Tools

These commands tell quite a bit about my work, I guess.

Recently, I have been doing quite a bit of Python development. In particular, I
have been replacing `flake8`, `isort`, and `pylint` with `ruff (139)`.
`nox (125)` is my primary tool for running Python tests. So, the concentration
of Python-related commands is not surprising.

It is like blue-collar work for me, but I am still developing with JavaScript
and TypeScript. I use `npm (87)` quite often. Nope, I am not using `yarn` or
`pnpm`. I do not even know why `pnpm` exists and I do not want to know.

Although I usually provision a `devcli (92)` script for each project, I still
run `cabal (71)` quite often. Unlike other programming languages, I do not keep
running a program to see if it works in Haskell. There are times when I keep
programming for hours, and then I run `cabal test` for performing doctests or
`cabal dev-test-build -c` to build, test, lint and format the codebase.

## Utilities

`cat (412)` and `find (125)` did not really surprise me. I use `cat` usually to
pipe the contents of one or more files to another command, not just printing
them to the terminal. I use `find` to search for files, but almost half of the
time, I perform an `exec` or `xargs` operation on the found files or
directories.

`mv (70)` is my primary tool for moving or renaming files, except when I am
working on a project inside `neovim`, where I use the sidebar to move files
around.

`sudo (92)` is also quite normal, because I am my own boss.

Finally, `bash (166)` is to be expected, because I use it to run shell scripts
which I quickly write to automate some tasks which do not fit into a one-liner.

## Custom Stuff

I track my time with `watson (436)`.

`basecamp (92)` is a custom tool my team and I wrote to keep track of incidents,
notes, announcements, server and service registries, etc.

And finally, `zenpolicy (89)` is something from my kitchen. It will take some
time, but hopefully, you will know more about it when I ship it.

## Conclusion

So, how do these numbers look?

```console
$ history 0 |
    awk '{print $2}' |
    sort |
    uniq -c |
    sort -n |
    tail -n 25 |
    tac |
    awk '{printf "%s,%d\n", $2, $1}' |
    uplot barplot -d, -o /dev/stdout -t "Top 25 Commands"
                                 Top 25 Commands
                    ┌                                        ┐
                 ls ┤■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 757.0
                ssh ┤■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 683.0
                 gh ┤■■■■■■■■■■■■■■■■■■■■■■■■■■ 596.0
                git ┤■■■■■■■■■■■■■■■■■■■■■■ 507.0
             watson ┤■■■■■■■■■■■■■■■■■■■ 436.0
                vim ┤■■■■■■■■■■■■■■■■■■ 413.0
                cat ┤■■■■■■■■■■■■■■■■■■ 412.0
                 cd ┤■■■■■■■■■■■■■■■■■ 398.0
                nix ┤■■■■■■■■■■■■■■■ 343.0
             docker ┤■■■■■■■■■■■■ 276.0
               bash ┤■■■■■■■ 166.0
             direnv ┤■■■■■■ 141.0
               ruff ┤■■■■■■ 139.0
                nox ┤■■■■■ 125.0
               find ┤■■■■■ 125.0
               curl ┤■■■■■ 124.0
            history ┤■■■■■ 104.0
          nix-shell ┤■■■■ 93.0
               sudo ┤■■■■ 92.0
             devcli ┤■■■■ 92.0
           basecamp ┤■■■■ 92.0
          zenpolicy ┤■■■■ 89.0
                npm ┤■■■■ 87.0
   vst-nixos-switch ┤■■■ 73.0
              cabal ┤■■■ 71.0
                    └                                        ┘
```

Your shell history never lies. Mine reflects how I like it: simple interfaces,
power tools, and workflows built around the CLI. If I use something hundreds of
times a week, it must be a good, reliable investment.

<!-- REFERENCE -->
