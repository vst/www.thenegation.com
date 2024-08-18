---
title: "Learning to Like Neovim"
date: 2024-08-18 21:43:13
taxonomies:
  tags:
    - Technical Note
    - Vim
    - Emacs
    - Hacking
    - Computing
---

After using Emacs for almost 20 years, usually day-in and day-out, I decided to
give Neovim a try. I have been using Neovim for two months now, and I can say
that I am starting to like it. In this article, I will share my experience of
switching from Emacs to Neovim, highlighting the differences, challenges, and
benefits I encountered along the way.

<!-- more -->

Disclaimer: Although my not primary objective, I intend to insult both Vim and
Emacs users in this article. I will occassionally insult users of other editors
as well. If I do not mention the editor you are using, please do not feel left
out: I did not even bother to insult you. I am (not) sorry for that.

This post is not a tutorial, nor a structured comparison of Neovim and Emacs. It
merely reflects my personal experience after 2 months of using Neovim. I may
write a more structured article after I have more experience with Neovim.

## My Journey with Emacs and Other Editors

I have been programming for more than 20 years and have used many different
editors. Two of them stood out: Emacs and Vim.

Emacs has been my daily driver since about late 2004. I have used it for many
purposes. You may have heard such stories before, but let me share what is
relevant for this article: I used Emacs as an email client (Gnus) for more than
5 years until 2011, as a web browser (w3m) for a while, as a file manager
occasionally, and as my music player before Spotify took over my life.

I have also used Vim, but usually for quick edits and mostly on remote servers.

For large projects, I almost always used IDEs. I have used Netbeans, Eclipse,
IntelliJ IDEA, Atom, and Visual Studio Code enough to talk about them. Yet, I
always returned to Emacs for my smaller projects, or for projects involving R,
Lisp, LaTeX, etc.

About 2 years ago, after a few years of working with Visual Studio Code, I
switched to Emacs entirely, thanks to the LSP mode, to work on large Haskell,
Python, and TypeScript projects. I was quite happy with it. TypeScript
development was a bit clunky, but it was even clunkier with Visual Studio Code
anyway.

I have to say that configuring was always a pain with Emacs. Over the years, I
declared "Emacs Config Bankruptcy" a few times and started from scratch. After
switching to NixOS, I adopted [emacs-overlay] that has built-in support for
literate programming with Org mode for Emacs configuration. It was bliss.

## Why I Decided to Switch

I was happy with Emacs. Indeed, I am still happy with it.

Let me put it this way: My litmus test for picking a new tool or technology is
imagining myself using it in my retirement days. Emacs passes this test almost
perfectly.

My switch to Neovim happened for two main, but coincidental reasons:

Firstly, after I switched to Emacs as an IDE, I convinced my friend and
colleague [Ali] to depart from Visual Studio Code and switch to either Emacs or
Neovim. He had experience with both, and he chose Neovim as I anticipated he
would: He is young and pragmatic. He can be quite pedantic, too, but in the end,
he was quite happy with his setup. You will see why this matters soon.

Secondly, I was working on a Haskell project where I was aggressively
refactoring the codebase, just 2 months ago. But suddenly, I bumped into a
possible [bug] that rendered my LSP rename functionality unusable. I was quite
frustrated.

Without much thought, I decided to give Neovim a try. So, it was not
premeditated.

## How I Switched to Neovim

First, I ran my litmus test: Can I imagine myself using Neovim in my retirement
days? How does it play with NixOS? Is there a decent community around it? Is it
actively developed? Do I like Lua as a configuration language?

All checked.

However, I realized that I did not have time to configure Neovim from scratch.
But I knew Ali had a decent configuration. Ali and I share our NixOS
configurations with each other. I also had the luxury of asking him questions
about his Neovim setup.

So, I took his Neovim configuration, made a few small changes, activated my new
NixOS configuration, and continued where I left off.

## First Impressions of Neovim

I was quite happy with Neovim. It was fast, snappy, and responsive.

The keybindings, however, were a challenge. Because (I guess) I marked Neovim as
the system-wide `$EDITOR`, my zsh keybindings switched to `vim` keybindings.
That was even more frustrating!

But I was so busy with my project that I did not even bother to play with my
configuration any further.

Also quite funny: Many things are missing compared to my Emacs setup. I have 16
GitHub issues on my NixOS configuration repository dedicated to my Neovim setup.
There is an LSP bug that makes me exit and re-enter Neovim at least once every
other day. However, I am quite happy with it, and I am simply ignoring these
issues for now.

## Comparison to Emacs

Emacs is an [operating system]. Vi(m), in the spirit of `ed`, is a program that
adheres well to [Unix Philosophy]. Neovim is a program that tries to be a bit
more than Vim and significantly less than Emacs.

I can confidently say these things:

1. Neovim is faster and more responsive than Emacs.
2. Neovim is pretty much as configurable and extensible as Emacs, as far as I am
   concerned.
3. I can launch many instances of Neovim for different projects, but Emacs is
   too heavy for that, and its frame-based multi-project support did not work
   well for me.
4. I love Lisp, but the lack of namespaces in Elisp is a pain. I am happy to use
   Lua for my Neovim configuration.
5. Neovim has a much bigger and more vibrant community than Emacs.

These are all good points.

But at the same time, I find Neovim plugins to be of lesser quality than Emacs
plugins on average. This is not a big deal, and it is actually quite normal and
even expected: If there are more people writing plugins for Neovim, there will
be more people with less programming experience as well. Programmers gain
experience over time. These plugins will just get better. The only challenge is
to pick your plugins wisely.

One important thing to mention here is the keybindings. Somehow, I had the
impression that Emacs had much more coherent keybinding maps compared to Vim.
The more I use Neovim, the more I am convinced that this is not necessarily
true. I am quite happy with Neovim's keybindings. In fact, sometimes I figure
out keybindings myself without even looking at the documentation.

## Changes to My Productivity and Workflows

I can not say that I am more productive with Neovim. But I am pretty sure that I
am not less productive either.

As for my workflow, I find it quite convenient to quickly launch a new Neovim
instance on a random `tmux` pane and start hacking.

I do not like the idea of using servers like workstations. But we have some
servers where we occasionally need to do some work. I am quite happy to use
Neovim on these servers. All our NixOS servers even have Neovim configured with
a subset of Ali's configuration. This is definitely a plus in terms of
convenience and productivity.

## What I Miss from Emacs

Actually, this question is not really relevant for me. By now, I know that I can
do almost everything I did with Emacs in Neovim.

My answer is, therefore, I miss all the things that I had configured in Emacs
and have not had time to configure in Neovim yet.

Maybe I just miss the keybindings?! I am not sure. I started liking Neovim's
modal editing.

## Going Forward

Neovim is a great editor. I see myself using it for a while now. So far, my only
investment in it has been learning the keybindings and a few concepts. Yet, I am
fully functional on it.

At some point, I will revisit my Neovim configuration and adapt it to my needs
as I am used to doing with Emacs. Only then will I be able to tell if I am
really so happy with Neovim that I can finally delete my Emacs configuration
(which I do not need to do anyway).

One thing is for sure; I do not see myself using an editor packed into Electron
(no Visual Studio Code or a fork of it, like [cursor]).

Still, I am open to new advancements in the field of text editors. I am keeping
an eye on [Kakoune], [Helix] and [Zed], for example.

<!-- REFERENCES -->

[emacs-overlay]: https://github.com/nix-community/emacs-overlay
[bug]: https://github.com/emacs-lsp/lsp-mode/issues/4473#issuecomment-2164211783
[operating system]: https://wiki.c2.com/?EmacsAsOperatingSystem
[Unix Philosophy]: https://en.wikipedia.org/wiki/Unix_philosophy
[cursor]: https://www.cursor.com
[Ali]: https://yildiz.dev
[Kakoune]: https://kakoune.org
[Zed]: https://zed.dev
[Helix]: https://helix-editor.com
