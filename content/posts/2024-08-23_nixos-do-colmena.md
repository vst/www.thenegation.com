---
title: "Managing NixOS on DigitalOcean with Colmena"
date: 2024-08-23 21:14:29
taxonomies:
  tags:
    - Technical Note
    - NixOS
    - Hacking
    - Computing
---

In this post, we will prepare a [DigitalOcean] image for [NixOS], launch a
droplet with it and manage it using [Colmena].

<!-- more -->

[DigitalOcean] does not have out-of-the-box support for [NixOS]. However, we can
use a custom image to launch a droplet with NixOS.

So, we will create our own custom image that is supported by the [DigitalOcean
NixOS module].

Then, we will upload the image to DigitalOcean and launch a droplet with it.

Finally, we will manage the droplet using [Colmena].

## Preparing the Custom Image

The Nix expression to build a DigitalOcean image looks like this:

```nix
## file: ./do-image.nix
let
  ## Pin the latest NixOS stable (nixos-24.05) release:
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/797f7dc49e0bc7fab4b57c021cdf68f595e47841.tar.gz";
    sha256 = "sha256:0q96nxw7jg9l9zlpa3wkma5xzmgkdnnajapwhgb2fk2ll224rgs1";
  };

  ## Import nixpkgs:
  pkgs = import nixpkgs { };

  ## Prepare the NixOS configuration:
  config = {
    imports = [
      "${nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
    ];
  };
in
(pkgs.nixos config).digitalOceanImage
```

Let's see what we do here:

1. We pin the latest NixOS stable release. In this example, we are using the
   latest commit hash `797f7dc49e0bc7fab4b57c021cdf68f595e47841` from the NixOS
   24.05 branch.
2. We import the `nixpkgs` as `pkgs`.
3. We prepare the NixOS configuration. We import the `digital-ocean-image.nix`
   module from `nixpkgs`.
4. Finally, we return the expression to be built using our configuration.

Note that our configuration is intentionally minimal. This is what we need to
launch a droplet with NixOS on DigitalOcean. We can add more configuration
later.

Now, let's build the image:

```sh
nix-build ./do-image.nix
```

This will create a file named `nixos.qcow2.gz` in the Nix store which we can see
under the `result` symlink:

```sh
$ ls -lh result/nixos.qcow2.gz
-r--r--r-- 1 root root 453M Jan  1  1970 result/nixos.qcow2.gz
```

The next step is to upload the image to DigitalOcean. For this, you can follow
the [Custom Images Quickstart] section of the [DigitalOcean documentation]. In
my case, I used following information while uploading the image:

1. Name: `NixOS 24.05 (797f7dc4)`
2. Distribution: `Unknown`
3. Data Center Region: `Singapore`
4. Tags: NixOS
5. Notes: `NixOS 24.05 from revision 797f7dc49e0bc7fab4b57c021cdf68f595e47841`

Depending on your Internet speed, it may take some time to upload the image to
DigitalOcean. Also, it may take some time while DigitalOcean processes the
image: You may see the _Uploaded_ status stuck at `Pending` for a while (between
5mins to 30mins in my experience).

## Launching a New Droplet

Launcing a droplet with our custom NixOS is pretty much the same as launching a
droplet with a stock image. You just need to select the custom image you
uploaded in the previous step.

Just do not forget to add your SSH key to the droplet so you can access it.

In my case, I launched a droplet with the following configuration:

1. Region: `Singapore`
2. Custom Image: `NixOS 24.05 (797f7dc4)`
3. Droplet Type: `Basic`
4. CPU Option: `Regular 1GB / 1CPU (USD 6 / month)`
5. SSH Key: `...`
6. Add improved metrics monitoring and alerting (free): `No`
7. Hostname: `hebele`
8. Tags: `testing`

Note that the "Add improved metrics monitoring and alerting" option is not
applicable to our custom image, unfortunately. I did not check why though.

I have been assigned the IP address `152.42.214.184`. Let's issue a command via
SSH to our droplet:

```console
$ ssh root@152.42.214.184 uname -a
Linux hebele 6.6.46 #1-NixOS SMP PREEMPT_DYNAMIC Wed Aug 14 11:59:04 UTC 2024 x86_64 GNU/Linux
```

Well done!

## Managing the Droplet with Colmena

[Colmena] is a tool to manage multiple NixOS systems. It is similar to [NixOps]
but fancier in spirit. I am not sure about the size of its userbase, but I do
not see many other alternatives to my liking except [NixOps4] and [deploy-rs].
So far, I am happy with it, so is my team.

First of all, once we start using Colmena, we will no longer need the
`/etc/nixos/configuration.nix`. Instead, the configuration will be in our
`hive.nix` for this particular host.

Secondly, typical mode of operation is to operate Colmena on a workstation
instead of the host itself.

Lastly, and most importantly, configuration is going to be built on the
workstation, deployed to the host along with all built artifacts, and then
activated on the host. You can build the configuration on the host, too, but I
have quite decent Internet connection and using my workstation's Nix store as
cache sounds good to me.

[Colmena Documentation] is quite decent. I recommend you to read it, especially
the [Tutorial] section.

Let's start by installing Colmena:

```sh
nix-shell -p colmena
```

Then, create our configuration:

```nix
## file: ./hive.nix
{ ... }:

let
  ## Pin the latest NixOS stable (nixos-24.05) release:
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/797f7dc49e0bc7fab4b57c021cdf68f595e47841.tar.gz";
    sha256 = "sha256:0q96nxw7jg9l9zlpa3wkma5xzmgkdnnajapwhgb2fk2ll224rgs1";
  };

  ## Known and authorized SSH public keys:
  publicKeys = {
    vst = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJIQtEmoHu44pUDwX5GEw20JLmfZaI+xVXin74GI396z"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3"
    ];
  };
in
{
  ## Colmena metadata:
  meta = {
    ## Define default nixpkgs which will be passed as `pkgs` to host
    ## configurations:
    nixpkgs = import nixpkgs-src { };
  };

  ## Defaults which apply to all hosts:
  defaults = { config, pkgs, ... }: {
    ## Configure root user with authorized SSH keys:
    users.users = {
      root = {
        openssh.authorizedKeys.keys = publicKeys.vst;
      };
    };

    ## Install system packages:
    environment.systemPackages = [
      pkgs.curl
      pkgs.fastfetch
      pkgs.htop
      pkgs.jq
      pkgs.nvd
      pkgs.screen
      pkgs.tmux
    ];

    ## Enable `git` program:
    programs.git = {
      enable = true;
    };

    ## Enable `neovim` program:
    programs.neovim = {
      enable = true;
      vimAlias = true;
      defaultEditor = true;
    };
  };

  ## Host configuration for `hebele`:
  hebele = { modulesPath, lib, ... }: {
    ## Colmena specific configuration:
    deployment = {
      targetHost = "152.42.214.184";
    };

    ## Same configuration like /etc/nixos/configuration.nix:
    imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
      (modulesPath + "/virtualisation/digital-ocean-config.nix")
    ];

    ## If you want to avoid warnings:
    system.stateVersion = "24.05";

    ## Open firewall ports for SSH and HTTP:
    networking = {
      firewall = {
        allowedTCPPorts = [ 22 80 ];
      };
    };

    ## Simple Nginx configuration:
    services.nginx = {
      enable = true;
      virtualHosts."localhost" = {
        locations."/" = {
          return = "200 'Hello, NixOS!'";
          extraConfig = ''
            default_type text/plain;
          '';
        };
      };
    };
  };
}
```

Finally, let's deploy the configuration and reboot the server:

```sh
colmena apply boot --reboot
```

Once Colmena says that servers are rebooted, we can do useless things like:

```console
$ ssh root@152.42.214.184 fastfetch

          ▗▄▄▄       ▗▄▄▄▄    ▄▄▄▖             root@hebele
          ▜███▙       ▜███▙  ▟███▛             -----------
           ▜███▙       ▜███▙▟███▛              OS: NixOS 24.05pre-git (Uakari) x86_64
            ▜███▙       ▜██████▛               Host: Droplet (20171212)
     ▟█████████████████▙ ▜████▛     ▟▙         Kernel: Linux 6.6.46
    ▟███████████████████▙ ▜███▙    ▟██▙        Uptime: 1 min
           ▄▄▄▄▖           ▜███▙  ▟███▛        Packages: 426 (nix-system)
          ▟███▛             ▜██▛ ▟███▛         Shell: bash 5.2.32
         ▟███▛               ▜▛ ▟███▛          Display (QEMU Monitor): 1024x768 @ 75Hz
▟███████████▛                  ▟██████████▙    Terminal: dumb
▜██████████▛                  ▟███████████▛    CPU: DO-Regular @ 2.49 GHz
      ▟███▛ ▟▙               ▟███▛             GPU: RedHat Virtio 1.0 GPU
     ▟███▛ ▟██▙             ▟███▛              Memory: 219.23 MiB / 969.05 MiB (23%)
    ▟███▛  ▜███▙           ▝▀▀▀▀               Swap: Disabled
    ▜██▛    ▜███▙ ▜██████████████████▛         Disk (/): 2.02 GiB / 24.53 GiB (8%) - ext4
     ▜▛     ▟████▙ ▜████████████████▛          Local IP (ens3): 152.42.214.184/19 *
           ▟██████▙       ▜███▙                Locale: en_US.UTF-8
          ▟███▛▜███▙       ▜███▙
         ▟███▛  ▜███▙       ▜███▙              ████████████████████████
         ▝▀▀▀    ▀▀▀▀▘       ▀▀▀▘              ████████████████████████

```

That's it! Going forward, we can use `colmena apply` if we do not need to reboot
the server.

We can manage many more servers with Colmena. We just add new hosts like we
added `hebele` in the `hive.nix`.

Before we close this section; note that we no longer need the
`/etc/nixos/configuration.nix` file. We can remove it. In fact, it is a good
idea to remove it to avoid confusion and mistakenly rebuilding the configuration
on the host via `nixos-rebuild`.

## Wrap Up

In this post, we have prepared a custom NixOS image for DigitalOcean, launched a
droplet with it, and started managed it using Colmena.

We are using NixOS on AWS, DigitalOcean and Hetzner. AWS is slightly more
comfortable as NixOS is available as one of community AMIs. However,
DigitalOcean is not that bad either. We can create our own custom image and
launch droplets with it. We did not experience any issues so far.

Personally, I am planning to move my workstation configuration to Colmena, too.
I just need to do some plumbing. Still, I am looking forward to [NixOps4]. If I
switch to flakes one day, I may give a try to [deploy-rs] as well.

<!-- REFERENCES -->

[DigitalOcean]: https://www.digitalocean.com
[NixOS]: https://nixos.org
[Colmena]: https://colmena.cli.rs
[DigitalOcean NixOS module]:
  https://github.com/NixOS/nixpkgs/blob/nixos-24.05/nixos/modules/virtualisation/digital-ocean-image.nix
[Custom Images Quickstart]:
  https://docs.digitalocean.com/products/custom-images/getting-started/quickstart/
[DigitalOcean documentation]: https://docs.digitalocean.com/
[NixOps]: https://github.com/NixOS/nixops
[NixOps4]: https://github.com/nixops4/nixops4
[deploy-rs]: https://github.com/serokell/deploy-rs
[Colmena Documentation]: https://colmena.cli.rs
[Tutorial]: https://colmena.cli.rs/unstable/tutorial/index.html
