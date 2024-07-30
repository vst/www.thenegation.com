---
title: Archiving PostgreSQL Backups on NixOS
date: 2024-07-30 09:18:10
taxonomies:
  tags:
    - Technical Notes
    - NixOS
    - Computing
---

This is a technical note on how to archive PostgreSQL backups on [NixOS] to one
or more targets using [rclone].

<!-- more -->

<!-- toc -->

## Problem

Some of my objectives during provisioning new hosts and services are to ensure
that:

1. The system is reproducible,
2. The process and related artifacts are documented, and
3. The data is backed up and archived securely.

Using NixOS solves the first two objectives due to its declarative nature.
However, the third one is not directly addressed by NixOS alone. It is depends
on how services are configured and data is managed.

If I am lucky, the host is stateless, i.e., all data is provisioned from an
external source such as a separate database server. This means the entire host
is disposable: when desired, I can launch a new host on my favourite cloud
provider, redeploy services, and make necessary DNS record changes.

However, this is not always the case. Sometimes, the host is running a database
server, typically a PostgreSQL service along with other services. In such cases,
I need to ensure that the data is backed up regularly and archived safely and
securely to one or more targets.

In relation to PostgreSQL, I want to ensure that:

1. Database snapshots (backups) are taken regularly,
2. Backups are encrypted, and
3. Encrypted backups are archived to a single target or preferably to multiple
   targets.

## Context

There are two ways of approaching this problem:

1. Use an external service that performs the backup and archiving for this host
   (acting as a supervisor), or
2. Use a service running on the host itself to perform the backup and archiving.

I prefer the second one because it is more cost-effective, gives me more control
over the process, and allows me to stick to the NixOS way of doing things to
maintain ubiquity.

In terms of archive targets, I prefer to use object stores, such as S3-compliant
services. This is because they are cost-effective, reliable, provide a good
level of security, and are easy to integrate with other services. Additionally,
I want to have multiple targets to avoid vendor lock-in and to have a backup
plan in case one of the targets fails. It would be nice if I could add other
target types easily in the future.

Finally, I do not want that the data to leave the host unencrypted. Archive
files should be encrypted before they leave the host using a secure, widely
supported, and hopefully post-quantum secure encryption algorithm.

## Solution

Consider following scenario:

1. We have a PostgreSQL service running on a NixOS host,
2. There are two databases that we want to backup: `db1` and `db2`,
3. We want to take a snapshot of these databases 15 minutes past every hour,
4. We want to encrypt the snapshots using GPG with symmetric encryption and a
   passphrase,
5. We want to archive the encrypted snapshots to two S3-compliant object stores:
   `s3://let-say-backblaze.com/bucket1` and `s3://let-say-wasabi.com/bucket2`.

I learned from [Abhinav's Note][abhinav-note] that `services.postgresqlBackup`
service creates `systemd` services for each database backup, and we can add an
`ExecStartPost` entry. This entry will be our workhorse.

This is the Nix function that will do it:

```nix
## file: ./lib/pg-db-archive.nix
{ config
, pkgs
, dbName
, fileEncKey
, fileRclone
}:
let
  script = pkgs.writeShellScriptBin "pg-db-archive" ''
    #!/usr/bin/env bash

    ## Fail on any error:
    set -e

    ## Define the backup directory path:
    _dirBackup="${config.services.postgresqlBackup.location}"

    ## Define the path to the file to be encrypted:
    _fileBackup="''${_dirBackup}/${dbName}.sql.gz"

    ## Define the path to the encrypted file to be archived:
    _fileArchive="''${_dirBackup}/${dbName}_$(date --utc +%Y%m%dT%H%M%SZ).sql.gz.enc"

    ## Encrypt the file:
    echo "Encrypting database dump..."
    ${pkgs.gnupg}/bin/gpg --symmetric --cipher-algo AES256 --batch --yes --passphrase-file ${fileEncKey} --output "''${_fileArchive}" "''${_fileBackup}"
    echo "Database file is encrypted successfully."

    ## Archive the file:
    echo "Archiving encrypted database dump..."
    ${pkgs.rclone}/bin/rclone --config "${fileRclone}" copy "''${_fileArchive}" "archive-target-database:/${dbName}/"
    echo "Encrypted database file is archived successfully."

    ## Remove the local archive file:
    echo "Removing local encrypted database dump..."
    rm -f "''${_fileArchive}"
    echo "Local encrypted database file is removed successfully."
  '';
in
{
  ExecStartPost = ''
    ${script}/bin/pg-db-archive
  '';
}
```

We can then use this function in our `configuration.nix` file:

```nix
## file: ./configuration.nix
{ ... }:
{
  ## ...

  ## Our typical PostgreSQL service:
  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    ensureDatabases = [
      "db1"
      "db2"
    ];
  };

  ## Setup the PostgreSQL backup service:
  services.postgresqlBackup = {
    enable = true;
    startAt = "*-*-* *:15:00";
    pgdumpOptions = "--no-owner";
    databases = [
      "db1"
      "db2"
    ];
  };

  systemd.services."postgresqlBackup-db1".serviceConfig = (import ./lib/pg-db-archive.nix) {
    config = config;
    pkgs = pkgs;
    dbName = "db1";
    fileEncKey = "/run/secrets/keys/db1";
    fileRclone = "/run/secrets/rclone/database-archive.conf";
  };

  systemd.services."postgresqlBackup-db2".serviceConfig = (import ./lib/pg-db-archive.nix) {
    config = config;
    pkgs = pkgs;
    dbName = "db2";
    fileEncKey = "/run/secrets/keys/db2";
    fileRclone = "/run/secrets/rclone/database-archive.conf";
  };

  ## ...
}
```

... whereby:

1. The contents of `/run/secrets/keys/{db1,db2}` are the passphrase for the GPG
   encryption:

   ```txt
   hebele-hubele
   ```

2. The contents of `/run/secrets/rclone/database-archive.conf` are the
   configuration file for the [rclone]:

   ```conf
   [archive-backblaze]
   type = s3
   provider = Other
   env_auth = false
   acl = private
   no_check_bucket = true
   access_key_id = SOME_ACCESS_KEY_ID
   secret_access_key = SOME_SECRET_ACCESS_KEY
   endpoint = https://let-say-backblaze.com

   [archive-target-database-backblaze]
   type = alias
   remote = archive-backblaze:bucket1/databases

   [archive-wasabi]
   type = s3
   provider = Other
   env_auth = false
   acl = private
   no_check_bucket = true
   access_key_id = ANOTHER_ACCESS_KEY_ID
   secret_access_key = ANOTHER_SECRET_ACCESS_KEY
   endpoint = https://let-say-wasabi.com

   [archive-target-database-wasabi]
   type = alias
   remote = archive-wasabi:bucket2/databases

   [archive-target-database]
   type = union
   action_policy = all
   create_policy = all
   search_policy = all
   upstreams = archive-target-database-backblaze:/ archive-target-database-wasabi:/
   ```

Once this configuration is activated, we can do following:

1. List `systemd` timers:

   ```sh
   systemctl list-timers postgresqlBackup-*
   ```

2. Run the `systemd` backup and archive service for a specific database:

   ```sh
   systemctl start postgresqlBackup-db1.service
   systemctl start postgresqlBackup-db2.service
   ```

3. See the service logs:

   ```sh
   journalctl --unit postgresqlBackup-db1.service
   journalctl --unit postgresqlBackup-db2.service
   ```

## Wrap-up

I have been using this recently and I am quite happy with the result. It is
simple and manageable. This approach is also quite portable with very little
effort.

In my actual setups, I use [sops-nix] (along with my [opsops]) to produce the
GPG key and rclone configuration files. This way, I can manage the secrets in a
secure and comfortable way.

The [rclone] configuration is quite flexible too: If I need another target, I
can just add another remote and update the `archive-target-database`
[union][rclone-union] target. It will be automatically picked up by the
`pg-db-archive` script as it only relies on the `archive-target-database`
remote. Note that the new target can be any target [supported by rclone].

<!-- REFERENCES -->

[NixOS]: https://nixos.org
[abhinav-note]: https://notes.abhinavsarkar.net/2023/mastodon-backup
[opsops]: https://github.com/vst/opsops
[rclone-union]: https://rclone.org/union/
[rclone]: https://rclone.org
[sops-nix]: https://github.com/Mic92/sops-nix
[supported by rclone]: https://rclone.org/overview/
