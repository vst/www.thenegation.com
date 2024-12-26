---
title: "Easy GitHub CLI Extensions with Nix"
date: 2024-08-19 20:35:24
description: Writing a simple GitHub CLI extension and packaging it with Nix.
taxonomies:
  tags:
    - Technical Note
    - GitHub
    - Nix
    - Hacking
    - Computing
---

GitHub CLI (`gh`) is one of my favourite tools. In addition to its built-in
commands, it allows you to write your own extensions. In this post, I will show
you how to write a simple GitHub CLI extension and how to package it with Nix,
in particular under Nix Home Manager.

<!-- more -->

## GitHub CLI and Its Extensions

[GitHub CLI] (`gh`) is the official command-line tool for GitHub. It provides a set
of commands for interacting with GitHub repositories, issues, pull requests, and
more. It also provides convenience functionality to issue authenticated API requests
to both REST and GraphQL APIs of GitHub:

```sh
$ gh api repos/vst/hostpatrol/releases \
    --jq ".[]|[.name,.created_at]|@csv" |
    qsv table
v0.0.15  2024-04-19T07:19:14Z
v0.0.14  2024-04-16T02:48:40Z
v0.0.13  2024-04-15T01:46:18Z
[...]
```

... or:

```sh
$ gh api graphql -f query='
  query {
    viewer {
      login
    }
  }
'
{
  "data": {
    "viewer": {
      "login": "vst"
    }
  }
}
```

In addition to its built-in commands, `gh` allows you to write aliases or your
own extensions. For example, you can define an alias to list all your
repositories:

```sh
$ gh alias set pv 'pr view'
$ gh pv
...
```

Extensions are more powerful than aliases. They are written in any language and
installed as a binary or a script. The extension is executed as a subcommand of
`gh`. You may check out the [official gh extensions documentation] for more details
or [available extensions] on GitHub.

## Nix Home Manager and `gh`

[Nix Home Manager] is a tool for managing a user environment with Nix. It
already has a nice way to install and configure `gh` with the `programs.gh`
option:

```nix
{
  # ...

  programs.gh = {
    enable = true;
    settings = {
      aliases = {
        co = "pr checkout";
        pv = "pr view";
      };
    };
    extensions = [
      pkgs.gh-s
    ];
  };

  # ...
}
```

Note the [`programs.gh.extensions`][pghe] option. It allows you to install any
`gh` extension from the Nixpkgs repository. However, if you want to write your
own extension, you need to package it yourself. That is not a big deal!

## Writing a Simple Extension

Let's say, we want to write an extension that lists all the repositories of the
user. The extension is a simple shell script that uses the `gh` command to list
the repositories current user owns and tabulates with [`xsv`][xsv] command:

```sh
#!/usr/bin/env bash

gh api \
  user/repos \
  --method GET \
  --raw-field type=owner \
  --paginate \
  --jq '.[]|[.full_name,.url]|@csv' |
  xsv table
```

Now, we can add it into the `programs.gh.extensions` option in the Nix Home
Manager configuration:

```nix
{
  # ...

  programs.gh = {
    enable = true;
    settings = {
      aliases = {
        co = "pr checkout";
        pv = "pr view";
        lr = "list-repos";
      };
    };
    extensions = [
      pkgs.gh-s
      (pkgs.writeShellApplication {
        name = "gh-list-my-repos";
        derivationArgs = {
          pname = "gh-list-my-repos";
        };
        runtimeInputs = [ pkgs.gh pkgs.xsv ];
        text = ''
          #!/usr/bin/env bash

          gh api \
            user/repos \
            --method GET \
            --raw-field type=owner \
            --paginate \
            --jq '.[]|[.full_name,.url]|@csv' |
            xsv table
        '';
      })
    ];
  };

  # ...
}
```

Note that:

1. We used the `writeShellApplication` function to create a shell script.
2. We added the `gh` and `xsv` packages as runtime inputs.
3. We added extra derivation arguments to set the package name which is needed
   by Nix Home Manager to install and locate the extension.

That's it! You can now use the `list-my-repos` command as a subcommand of `gh`:

```sh
$ gh list-my-repos
...
```

<!-- REFERENCES -->

[GitHub CLI]: https://cli.github.com
[available extensions]: https://github.com/topics/gh-extension
[official gh extensions documentation]:
  https://cli.github.com/manual/gh_extension
[pghe]:
  https://home-manager-options.extranix.com/?query=programs.gh.extensions&release=release-24.05
[xsv]: https://github.com/BurntSushi/xsv
[Nix Home Manager]: https://nix-community.github.io/home-manager/
