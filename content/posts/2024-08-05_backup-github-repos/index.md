---
title: "Backup GitHub Repositories with gidek"
date: 2024-08-05 21:24:10
description: A simple Haskell program to backup your GitHub repositories.
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - NixOS
    - Git
    - Hacking
    - Computing
---

Do you backup your Git repositories? If not, you should consider doing so. I
might even have a solution for you if you are using GitHub: [gidek]. It even has
some NixOS goodies packed with it.

<!--more-->

## Why Backup Git Repositories?

Git is a distributed version control system. It means that every recent clone of
a Git repository is pretty much a full backup of the repository.

If you are using a centralized hosting service like GitHub, GitLab, or
Bitbucket, you are relying on the service provider to keep your repository safe
and alive. While these services are considered usually reliable, outages have
happened in the past.

Also, you might want to keep a backup of your repositories for other reasons:

- You may lose access to your account.
- You might want to migrate to another service.
- Your auditors might ask for your repository backups.
- Some smart colleague might delete a branch or a tag.
- Some smarter colleague might override the history of a branch.
- If a meteor hits the Earth, you might want to have a backup of your code. I
  do.

## How to Backup Git Repositories?

The simplest solution is to clone first:

```sh
git clone --mirror https://github.com/terremoth/awesome-hilarious-repos.git
```

... and update next time:

```sh
cd awesome-hilarious-repos.git && git remote update
```

Now, do it 101 times for your personal repositories and 186 times for your
organizations'. Do it every day. And also make sure that the repository names
and owners are not changed.

There are some tools you can subscribe online to, or install on your workstation
to backup your Git repositories. Some of them have quite good enterprise
reputation. But they are not free.

In my case, I want to stick to free and open-source software. I also want to use
the same solution both at home and at work. I want to have a simple and reliable
solution that I can trust and hack.

I helped myself with [gidek].

## gidek

[gidek] is a simple Haskell program that I wrote to backup my GitHub
repositories. It does more or less what I described above: clones all the
repositories of interest, and updates them later.

But there are a few more considerations handled by [gidek]:

1. It uses GitHub API to retrieve the list of the repositories ([`gh`][gh] to be
   precise). It means that it can handle private and public, old and new
   repositories as well.
2. It uses the GitHub repository ID as the target directory name. It means that
   it can handle repository renames and ownership changes gracefully.
3. It can handle multiple GitHub users and organizations, as well as single
   repositores, in a single run in a given configuration file.

You can use Nix to install gidek or download the statically compiled binary from
the [releases page]. Checkout the [gidek] repository for more information.

Once installed, you can prepare a configuration file like this:

```yaml
cat <<EOF > config.yaml
store: /data/gidek/store
token: $(gh auth token)
repos:
  - type: single
    name: vst/gidek
  - type: user
    name: vst
  - type: organization
    name: fourmolu
EOF
```

..., optionally see the plan that would gidek run:

```sh
gidek --config config.yaml plan
```

..., and run backups:

```sh
gidek --config config.yaml backup
```

Now, you can add this to your `crontab` or `systemd timer` to run it
periodically.

I am not using Bitbucket, GitLab or other services (at least not actively).
Therefore, [gidek] currently supports only GitHub. But adding support for other
services should not be hard.

## How About NixOS?

I knew you all would ask for this! Yes, [gidek] comes with a NixOS module that
introduces the `gidek` service and the `gidek` program to your system:

```nix
{
  #...

  imports = [
    ## I am using niv to pin the gidek version:
    "${sources.gidek}/nix/modules/nixos"
  ];

  services.gidek = {
    enable = true;
    user = "vst";
    schedule = "Sat *-*-* 00:00:01";
  };

  programs.gidek = {
    enable = true;
    config = {
      store = "/data/gidek";
      ## I am using sops-nix to manage my secrets:
      token_file = config.sops.secrets.github_token.path;
      repos = [
        { type = "single"; name = "vst/gidek"; }
        { type = "user"; name = "vst"; }
        { type = "organization"; name = "fourmolu"; }
      ];
  };

  #...
}
```

✅ _Most Important Task of the Day: Promote NixOS however annoying_

## Conclusion

I and my company are using [gidek] to backup our GitHub repositories. It gave me
some peace of mind. I hope it will give you some, too.

<!-- REFERENCES -->

[gidek]: https://github.com/vst/gidek
[releases page]: https://github.com/vst/gidek/releases
[gh]: https://cli.github.com/
