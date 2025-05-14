---
title: Managing NixOS Secrets via SOPS, sops-nix and opsops
date: 2024-08-03 18:29:25
description:
  A technical note on managing secrets in a declarative way using SOPS, sops-nix
  and opsops.
taxonomies:
  tags:
    - Technical Note
    - Security
    - SOPS
    - NixOS
    - Computing
---

Secret provisioning is a critical operation during the deployment and management
of a software system. The way it is done can have significant impact on both
security and operational efficiency.

In this post, I am going to discuss [SOPS], [sops-nix] and [opsops] tools for
managing secrets and how I and my team use it.

<!--more-->

## Imperative vs Declarative Secret Management

There are three approaches to managing secrets in a software system:

1. **Manual:** Secrets are provided by the person deploying the system. This is
   the simplest approach, but it is also the least secure. It is difficult to
   audit who has access to the secrets and when they were accessed. It is also
   difficult to rotate the secrets.
1. **Imperative:** Secrets are managed by the deployment scripts or the
   deployment tool. The secrets are stored in a secure location and are injected
   into the system at deployment time. This approach is simple and relatively
   easy to understand. However, it has similar drawbacks as in the manual
   approach.
1. **Declarative:** Secrets are managed by a separate tool. The secrets are
   stored in a secure location and are accessed by the system when needed. This
   approach is more secure and easier to audit. It also makes it easier to
   rotate the secrets.

There are many tools available for managing secrets in a declarative way. Most
of the Infrastructure as Code (IaC) tools like Terraform, Ansible, and
CloudFormation have built-in support for managing secrets. However, these tools
are not designed specifically for managing secrets, can be cumbersome to use or
not applicable at all in certain environments.

## Enter SOPS

[SOPS] is a simple, yet powerful tool for managing secrets. It consumes a clear
secret file in various formats and encrypts values using keys. The encrypted
file can be then stored in a version control system and decrypted when needed.

The recently launched [SOPS] Website is quite informative and provides a good
overview of the tool.

But to demonstrate, let's consider a simple example. Suppose we have a YAML file
containing clear secrets `secrets_clear.yaml`:

```yaml
# file: secrets_clear.yaml
service:
  api:
    key: "my-api-key"
    secret: "my-api-secret"
  db:
    username: "my-db-username"
    password: "my-db-password"
    database: "my-db-database"
smtp:
  host: "my-smtp-host"
  port: 587
  username: "my-smtp-username"
  password: "my-smtp-password"
```

Once this file is encrypted using SOPS against some age keys generated from an
SSH public key, secrets will look something like:

```yaml
# file: secrets_encrypted.yaml
service:
  api:
    key: ENC[AES256_GCM,data:...]
    secret: ENC[AES256_GCM,data:...]
  db:
    username: ENC[AES256_GCM,data:...]
    password: ENC[AES256_GCM,data:...]
    database: ENC[AES256_GCM,data:...]
smtp:
  host: ENC[AES256_GCM,data:...]
  port: ENC[AES256_GCM,data:...]
  username: ENC[AES256_GCM,data:...]
  password: ENC[AES256_GCM,data:...]
```

This encrypted file can be decrypted using the SSH private key corresponding to
the age key used for encryption. For example: We can prepare an encrypted
secrets file for 5 different servers, copy it to these servers, and decrypt it
there. To repeat, only the servers that have the corresponding SSH private key
can decrypt the secrets.

## Nix Buddy: sops-nix

The data definition and operational model of [SOPS] is well suited for a
Nix-powered system. [sops-nix] offers both [NixOS] and [Nix Home Manager]
modules which provide a declarative way to manage secrets using [SOPS].

The [sops-nix] module decrypts secrets at activation time and provides them as
individual files under the secret store, usually `/run/secrets`. The file tree
under the secret store is the same as the key tree in the encrypted file: Each
encrypted value is the content of the file, its key is the file name, and its
parent directories are the path to the key:

```sh
$ cat /run/secrets/service/api/key
my-api-key
```

There are 2 more important aspects of sops-nix:

1. It provides a way to manage the owner and permissions of the generated secret
   files.
2. It provides a simple templating system to produce new secret files by
   interpolating secrets into templates.

For example, given the following Nix snippet:

```nix
{
  ## ...
  sops.templates."service.env" = {
    content = ''
      API_KEY=${config.sops.placeholder."service/api/key"}
      API_SECRET=${config.sops.placeholder."service/api/secret"}
    '';
  };
  ## ...
}
```

... you will get a file `/run/secrets-rendered/service.env` with the following
content:

```sh
$ cat /run/secrets-rendered/service.env
API_KEY=my-api-key
API_SECRET=my-api-secret
```

Use it to set your Docker container environment variables file, for example.

## Streamlining Secrets Generation with opsops

[opsops] is a CLI program that I wrote to generate the clear secrets file in a
declarative way. It is agnostic to what you use it for: It can be used to
generate secrets which will be deployed to NixOS or somewhere else.

My main motivations were the following:

1. I am still not in love with the idea of storing secrets in a version control
   system even though they are encrypted.
2. I do not want to edit a YAML file manually to add or update secrets.
3. I want to know where the secrets are coming from and who else might have
   access to them.

So, for a quick spin, consider the following opsops configuration:

```yaml
secrets:
  zamazingo:
    secret:
      type: "process"
      value:
        command: "zamazingo"
        arguments: ["--hip", "hop"]
  github:
    token:
      type: "script"
      value:
        content: 'printf "%s" "$(gh auth token)"'
  example.com:
    password:
      type: "script"
      value:
        interpreter: "python3"
        content: |
          import netrc
          import sys

          _login, _account, password = netrc.netrc().authenticators("example.com")

          sys.stdout.write("password")
  dockerhub:
    password:
      type: "op"
      value:
        account: "PAIT5BAHSH7DAPEING3EEDIE2E"
        vault: "Cloud Accounts"
        item: "yies1Ahl4ahqu1afao4nahshoo"
        field: "password"
  influxdb:
    token:
      type: "op-read"
      value:
        account: "IPAEPH0JI3REE8FICHOOVU4CHA"
        uri: "op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/API Tokens/write-only"
```

This file has similar structure to both desired clear secrets file and the
encrypted file except that the leave values are just specifications for how to
retrieve/generate the secret.

Currently, there are 4 types of sources for the secrets:

1. `process`: The secret is generated by running a process.
2. `script`: The secret is generated by running a script with a specific
   interpreter (defaults to `sh`).
3. `op`: The secret is retrieved from 1Password using the `op` command.
4. `op-read`: The secret is retrieved from 1Password using the `op` command and
   the URI of the secret.

I am planning to add new types of sources in the future. It is implemented in
Haskell and contributions are much welcome.

The output of running `opsops` will be a clear secrets file that can be
encrypted using SOPS:

```yaml
zamazingo:
  secret: hebelehubele
github:
  token: gho_meecubier5dinohSh3tohphaekuo5Phahpei
example.com:
  password: password
dockerhub:
  password: ohbauy5eing8pheSh6iigooweeZee6ch
influxdb:
  token: mu9aephabeadi7zi8goo9peYo8yae7ge
```

I am (almost) happily checking in the `opsops` configuration file to the version
control system.

## Real World Value

To summarize:

1. Using [opsops], one can refer to the configuration file to see where secrets
   are originally coming from and who has access to it. This is at least a
   starting point for auditing the secrets.
2. Using [sops-nix], the [SOPS] secrets become first class citizens in the NixOS
   configuration.

Therefore, using a multi-host NixOS deployment tool like [colmena], we can
manage multiple secrets files for different hosts and different environments in
a familiar way. This makes auditing easier, and rotating secrets becomes a
breeze.

I am managing secrets on all my personal computers, cloud and in-house servers
using [SOPS], [sops-nix] and [opsops].

As a team, we are migrating our infrastructure entirely to NixOS, and we chose
the same secret management approach for our development and production hosts in
this process.

There are still some rough edges in the tools and the workflow. But so far, this
approach did not fail me or my team.

<!-- REFERENCES -->

[SOPS]: https://getsops.io
[sops-nix]: https://github.com/Mic92/sops-nix
[opsops]: https://github.com/vst/opsops
[colmena]: https://colmena.cli.rs
[NixOS]: https://nixos.org
[Nix Home Manager]: https://nix-community.github.io/home-manager/
