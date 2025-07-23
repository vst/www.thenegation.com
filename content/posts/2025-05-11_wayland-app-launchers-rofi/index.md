---
title: "Wayland Application Launchers: Stick with Rofi"
date: 2025-05-11 23:55:27
description: >
  A quick log of my recent attempt to replace `rofi` with something
  Wayland-native, and the alternatives I explored.

slug: wayland-app-launchers-rofi
tags:
  - Technical Notes
  - GNU/Linux
---

Today is a lazy Sunday, and I did what nobody should do on a Sunday: Spend time
trying to replace something that already works. This time, my victim was [rofi].

<!--more-->

I have been using GNU/Linux since 2001 on my workstations. During this time, I
have never used Microsoft Windows, and I used macOS for a short period of time
as a bootloader for Emacs.

On GNU/Linux, I occasionally used Gnome and KDE in the beginning, but most of
the time, I used minimalistic window managers. Since I switched to Wayland, I
have been using Sway as my window manager.

Granted, I do not use more than a few desktop applications. The ones I launch
run until I reboot my workstation, such as Firefox, Wezterm, Emacs etc. I even
replaced Emacs with Neovim recently. Everything else is just CLI or TUI tools.

One of the funny scenes to watch for me is when my family members attempt to use
my mouse to launch a Web browser to show me something. They first get confused
and then angry when they realize that there is nothing to click on the screen.

So, what do I use to launch desktop applications? [rofi].

It is a very simple but powerful _"window switcher, application launcher and
dmenu replacement"_. It looks and feels similar to macOS' Spotlight. It has the
concept of _modes_, which are basically different prompts for different
purposes. You can create your own prompts.

One of the most powerful features of rofi is the ability to use it as a
standalone prompt. For example, [bemoji] is a shell script that uses `rofi` (or
another picker) to search emojis, select one, and copy it to the clipboard:

```sh
nix run nixpkgs#bemoji
```

I am happy with `rofi`, except that Wayland support is currently provided
through a fork called [rofi-wayland]. My setup from when I was using X works on
Wayland with some exceptions. Instead of spending my time fixing these issues, I
usually look for alternatives, mostly out of laziness and curiosity.

Today, I did that again. The outcome was still sticking with `rofi`, but some
alternatives are worth mentioning, in alphabetical order:

- [albert]: Powerful, but a bit involved. I am looking for something simpler.
- [fuzzel]: Probably a good idea, many people advocate for it. See also [raffi].
- [kickoff]: Looks nice and simple, but maybe limited.
- [kupfer]: A bit too old-fashioned.
- [nwg-drawer]: Only for launching apps and performing power operations. Looks
  nice, though.
- [onagre]: Looks interesting, but does not seem to be able to replace `rofi`.
- [raffi]: A launcher built on top of [fuzzel], works with a configuration file.
  I like it and will keep it in mind.
- [rmenu]: Quite customizable. I like the idea behind it. I am going to keep it
  in mind.
- [sherlock]: New, fancy. Need to investigate. It did not work out of the box,
  but might be too involved for my use case.
- [sirula]: Only an app launcher, from .desktop files. Not what I am looking
  for.
- [sway-launcher-desktop]: Nice, but limited. [fzf] based, works on the console.
  Good idea, though.
- [sysmenu]: Limited, not working out of the box as expected.
- [tofi]: Advertises itself as a `dmenu` and `rofi` replacement and claims to be
  very fast. It is limited to launching applications. I could not see how I
  would customize it with my own scripts.
- [ulauncher]: Probably the most comprehensive one. Not for me, though, and
  using it with extensions might be tricky with Nix.
- [wldash]: Nice, but limited functionality.
- [wox]: Interesting, but maybe a bit too much for me. Could not get it to work
  out of the box the way I expected.
- [yofi]: Nice, but too rough. I can use it for shell programming for prompting
  users for actions.

There is a dedicated section on Wayland launchers in the [Awesome Wayland]. You
can check it out yourself if you are interested in more options. For me, in the
final analysis, I did not find a replacement for `rofi`. Yet, I see [fuzzel] and
[raffi] as future options.

But also, [sway-launcher-desktop] is a brilliant hack that uses [fzf] to
implement a launcher that works in the console. I can think of many such use
cases. As a starting point, I revisited my `fzf` shell integration configuration
today and decided to invest in it a bit more for my scripting efforts.

<!-- REFERENCES -->

[Awesome Wayland]:
  https://github.com/rcalixte/awesome-wayland?tab=readme-ov-file#launchers
[albert]: https://albertlauncher.github.io
[aphorme]: https://github.com/Iaphetes/aphorme_launcher
[bemoji]: https://github.com/marty-oehme/bemoji/
[fuzzel]: https://codeberg.org/dnkl/fuzzel
[fzf]: https://github.com/junegunn/fzf
[kickoff]: https://github.com/j0ru/kickoff
[kupfer]: https://kupferlauncher.github.io/
[nwg-drawer]: https://github.com/nwg-piotr/nwg-drawer
[onagre]: https://github.com/onagre-launcher/onagre
[raffi]: https://github.com/chmouel/raffi
[rmenu]: https://github.com/imgurbot12/rmenu
[rofi-wayland]: https://github.com/in0ni/rofi-wayland
[rofi]: https://github.com/davatorium/rofi
[sherlock]: https://github.com/Skxxtz/sherlock
[sirula]: https://github.com/DorianRudolph/sirula
[sway-launcher-desktop]: https://github.com/Biont/sway-launcher-desktop
[sysmenu]: https://github.com/System64fumo/sysmenu
[tofi]: https://github.com/philj56/tofi
[ulauncher]: https://ulauncher.io/
[wldash]: https://github.com/kennylevinsen/wldash
[wox]: https://github.com/Wox-launcher/Wox
[yofi]: https://github.com/l4l/yofi
