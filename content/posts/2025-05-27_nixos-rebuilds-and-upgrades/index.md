---
title: "NixOS Rebuilds, Upgrades and Generation Diffs"
date: 2025-05-27 21:20:42
description: >-
  My experience with upgrading NixOS to v25.05, how I use nvd to check
  differences between generations, and when to reboot after upgrades.
slug: nixos-rebuilds-and-upgrades
tags:
  - Technical Notes
  - Hacking
  - NixOS
  - Nix
---

Today, I upgraded my NixOS system to the latest version, v25.05. It went
smoothly. I just want to report my experience here.

<!--more-->

## In-Place Upgrades: NixOS vs Other Distributions

I have been using GNU/Linux for well over two decades now. And I have never
upgraded my distribution in-place. I always did a fresh install, because I did
not want to deal with leftover files, changed configuration options and so on.

But with NixOS, I only did fresh installs a few times across half a dozen
workstations and cloud hosts, only when I provisioned them for the first time or
changed my disks. The rest of the time, I just changed my NixOS channel to the
latest version and ran `nixos-rebuild switch --upgrade`. It almost always worked
without a hitch. Only sometimes, I had to fix some minor issues, like deprecated
options or changed package names. But I never had to reinstall my system.

Today was no exception. In about half an hour, I had my system upgraded from
NixOS v24.11 to v25.05.

## NixOS Rebuilds and Diffs

While I was upgrading, I took note of a feature I have been using for a while
now: Checking the differences between the new and the old _generations_.

A _generation_ is a snapshot of your NixOS system at a certain point in time.
This concept is quite central to both NixOS and Home Manager. It allows you to
roll back to a previous state of your system if something goes wrong.

These generations are stored in the `/nix/var/nix/profiles/` directory:

```console
$ ls -dv /nix/var/nix/profiles/system-* | tail -n10
/nix/var/nix/profiles/system-86-link
/nix/var/nix/profiles/system-87-link
/nix/var/nix/profiles/system-88-link
/nix/var/nix/profiles/system-89-link
/nix/var/nix/profiles/system-90-link
/nix/var/nix/profiles/system-91-link
/nix/var/nix/profiles/system-92-link
/nix/var/nix/profiles/system-93-link
/nix/var/nix/profiles/system-94-link
/nix/var/nix/profiles/system-95-link
```

Each of these directories is a symlink to a specific generation of your system,
which is a derivation stored under `/nix/store/`.

If you make even the smallest change to your NixOS configuration and run
`nixos-rebuild switch`, it will create a new generation of your system. You can
then compare the current generation with the previous one using [nvd]:

```console
$ nvd diff /nix/var/nix/profiles/system-65-link /nix/var/nix/profiles/system-66-link
<<< /nix/var/nix/profiles/system-65-link
>>> /nix/var/nix/profiles/system-66-link
Version changes:
[ ... TRUNCATED FOR BREVITY ... ]
[U.]  #82  qsv                       2.2.1 -> 4.0.0
[C.]  #83  rofi                      1.7.5, 1.7.5+wayland3 -> 1.7.5+wayland3
[ ... TRUNCATED FOR BREVITY ... ]
[C*]  #87  systemd                   <none>, 256.10 x2, 256.10-man, 257.3 -> <none>, 256.10 x2, 256.10-man, 257.5
[C.]  #88  systemd-minimal           256.10 x2, 257.3 -> 256.10 x2, 257.5
[ ... TRUNCATED FOR BREVITY ... ]
Added packages:
[A.]  #1  bemoji                     0.4.0
[A.]  #2  ponymix                    5
[A.]  #3  rofi-bluetooth-unstable    2023-02-03
[A.]  #4  rofi-network-manager       0-unstable-2024-09-03
[A.]  #5  rofi-pulse-select          0.2.0
Removed packages:
[R.]  #1  python3.12-configargparse  1.7
[R.]  #2  rofimoji                   6.5.0
[ ... TRUNCATED FOR BREVITY ... ]
Closure size: 8610 -> 8683 (1200 paths added, 1127 paths removed, delta +73, disk usage +151.4MiB).
```

You can see the differences in three sections:

1. **Version changes**: Packages that have been upgraded/downgraded.
2. **Added packages**: New packages that have been added to the system.
3. **Removed packages**: Packages that have been removed from the system.

The reason I left `systemd` in the sample output above is to talk about when we
need a reboot.

## Rebooting After NixOS Updates

Whether you upgrade to a new NixOS version or just upgrade your system within
the same version, you might need to reboot your system if certain packages have
been upgraded.

When using Ubuntu, you might have noticed that the system prompts you to reboot
after a kernel upgrade. This is because the package manager knows when to
reboot. NixOS does not prompt you, so you have to check yourself.

There is no hard and fast rule, but a good rule of thumb is to reboot if

- the Linux kernel has been upgraded, or
- `systemd` has been upgraded.

The curse of NixOS is that it is a _symlink_ hell, but this is also a blessing.
Check it out:

```console
$ stat -c '%N' /run/booted-system /run/current-system
'/run/booted-system' -> '/nix/store/9lhqy6mvqgmrisirdhbli43i5kmv3aky-nixos-system-myhost-25.05.20250525.7c43f08'
'/run/current-system' -> '/nix/store/9lhqy6mvqgmrisirdhbli43i5kmv3aky-nixos-system-myhost-25.05.20250525.7c43f08'
```

These two symbolic links point to two derivations in the Nix store, exactly what
we call _generations_ which we can list under `/nix/var/nix/profiles/`. You can
check yourself.

When we run `nixos-rebuild switch`, it will create a new generation of our
system, and change the `/run/current-system` symlink to point to the new
generation. The `/run/booted-system` symlink will still point to the previous
generation, which is the one that was active when the system booted.

So, if you want to check whether you need to reboot your system, you can run:

```sh
nvd diff /run/booted-system /run/current-system | grep -E 'systemd|linux'
```

If there is no output, you do not need to reboot. If there is output, you might
need to reboot your system to apply the changes.

There is even a tool that does this for you: [nixos-needsreboot]. I did not feel
like installing it, so I used the command above.

## Bonus

You can add the following to your NixOS configuration:

```nix
{
  system.activationScripts = {
    diffGens = ''
      PATH=$PATH:${lib.makeBinPath [ pkgs.nix ]}
      echo "----- CHANGES (ROOT) -----" | ${pkgs.lolcat}/bin/lolcat -S 40
      ${pkgs.nvd}/bin/nvd diff /run/current-system "$systemConfig"
      echo "----- END OF CHANGES -----" | ${pkgs.lolcat}/bin/lolcat -S 40
    '';
  };
}
```

This will run the `nvd diff` command every time you switch to a new generation
of your system. It will show you the differences between the current generation
and the one you are switching to. It helped me out a lot to quickly see and
understand what a rebuild did to my system.

I use the `lolcat` package to colorize the two lines indicating the start and
end of the `nvd diff` command output.

The `$systemConfig` variable is a symlink to the recently built NixOS
configuration, which is the one you are switching to. I had no idea that it
existed until I read the [source code] of the related NixOS module.

That is what I like about NixOS: Often, you can find a solution to your problem
by reading the source code of the NixOS modules.

<!-- References -->

[nvd]: https://khumba.net/projects/nvd/
[nixos-needsreboot]: https://github.com/thefossguy/nixos-needsreboot
[source code]:
  https://github.com/NixOS/nixpkgs/blob/62b852f6c6742134ade1abdd2a21685fd617a291/nixos/modules/system/activation/activation-script.nix#L58
