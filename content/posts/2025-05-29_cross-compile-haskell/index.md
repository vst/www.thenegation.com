---
title: "Cross-Compiling Haskell under NixOS with Docker"
date: 2025-05-29 23:55:54
description: >-
  Cross-compiling Haskell projects under NixOS using Docker images for ARM
  architectures, and running them under emulation on x86_64 hosts.
taxonomies:
  tags:
    - Technical Notes
    - Docker
    - Hacking
    - Haskell
    - Nix
---

I learned how to cross-compile Haskell projects under NixOS using Docker images
for ARM architectures, and how to run them under emulation on `x86_64` hosts.

<!-- more -->

## Motivation

I attended the [AWS Summit 2025] in Singapore. I enjoyed the event. There were
booths from various companies which I found interesting, such as [GitLab] and
[ClickHouse]. More importantly, I got to meet very interesting people.

Among the booths, there was a particular one that caught my attention: AWS was
showcasing their ARM-based [Graviton] processors. I chatted with the AWS folks,
and asked a few questions which I had in mind for quite some time.

I compiled a few of my Haskell projects on my Raspberry Pi 4, which is based on
the ARM architecture. I was curious to see how some others would perform on the
Graviton processors. I could go and compile them on the Graviton processors, on
my Raspberry Pi 4, or rather cross-compile them on my `x86_64` workstation.

## Cross-Compiling Haskell Projects

Cross-compiling Haskell projects always seemed intimidating to me. I do not know
if it is practically possible, either. Even statically linking Haskell binaries
is quite a challenge, especially under Nix. Instead, I am currently statically
compiling my Haskell projects under a Docker image that is built and published
by [benz0li]:

<https://github.com/benz0li/ghc-musl>

I am using a script that generates a `cabal.project.freeze` from my Nix setup,
compiles the project inside a Docker container from the above image, copies the
binary to the host, and then compresses it using [upx].

You can check the [script] under my [Haskell project template repository].

I knew that [benz0li] publishes the Docker images for both `x86_64` and `arm64`
architectures. He has even recently published additional images to deal with the
[GMP licensing restrictions].

So I decided to try running the ARM-based Docker image on my `x86_64` host,
which I had never tried before. First, I needed to make sure that I can do that.
This is the normal invocation of the Docker container:

```console
$ docker run --rm quay.io/benz0li/ghc-musl:9.8.4 uname -a
Linux 8c14a21fc636 6.12.30 #1-NixOS SMP PREEMPT_DYNAMIC Thu May 22 12:29:54 UTC 2025 x86_64 Linux
```

As expected, the `uname -a` command ran inside the container shows that it is
running on the `x86_64` architecture. Now, we can try to run the ARM-based
Docker image:

```console
$ docker run --rm --platform linux/arm64 quay.io/benz0li/ghc-musl:9.8.4 uname -a
# exec /usr/bin/uname: exec format error
```

## Configuring QEMU Support on NixOS

That is expected: We cannot run an ARM-based Docker image on an `x86_64` host
without some additional setup, in particular, using [QEMU].

Most of the tutorials I found online suggested using:

```bash
docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
```

... which I decided would not work on my NixOS host. Instead, I used the NixOS
option to enable QEMU emulation:

```nix
{
    boot.binfmt = {
      emulatedSystems = [ "aarch64-linux" ];
    };
}
```

This did not work, either. [Apparently], Docker needs the static binaries
provided by the `multiarch/qemu-user-static` image. So I changed my
configuration as advised:

```nix
{
    boot.binfmt = {
      emulatedSystems = [ "aarch64-linux" ];
      preferStaticEmulators = true; # Make it work with Docker
    };
}
```

## Good News

And it worked as such:

```console
$ docker run --rm --platform linux/arm64 quay.io/benz0li/ghc-musl:9.8.4 uname -a
Linux 15afb3b1a45b 6.12.30 #1-NixOS SMP PREEMPT_DYNAMIC Thu May 22 12:29:54 UTC 2025 aarch64 Linux
```

Now, I could change the script to consume arbitrary arguments and pass them to
the `docker run` command:

```bash
bash build-static.sh --platform=linux/arm64
```

Honestly, I was not expecting it to work, but it did, although it was noticeably
slower! One thing I noticed was being able to run both the `x86_64` and `arm64`
binaries on my `x86_64` host, which I was not expecting at all. Apparently, my
system is now capable of running both architectures at the same time -- with the
latter running under emulation.

You can check the [script] and adopt it for your own Haskell projects.

## Conclusion

It was an interesting day.

Firstly, I ran a non-`x86_64` Docker image under emulation on my `x86_64` host,
which I had never done before. Secondly, now I know that I can cross-compile my
Haskell projects for ARM architectures using the `arm64` Docker image provided
by [benz0li]. Going forward, I can fearlessly cross-compile my Haskell projects
for any supported, non-native architectures.

And I am definitely going to try the Graviton processors, as soon as I spin up a
Graviton EC2 instance on AWS.

<!-- REFERENCES -->

[GMP licensing restrictions]:
  https://github.com/benz0li/ghc-musl?tab=readme-ov-file#gmp-licensing-restrictions
[benz0li]: https://github.com/benz0li
[script]:
  https://github.com/vst/haskell-template-hebele/blob/ee5af52bf558c0514b482a710df497ed9fb27460/build-static.sh
[Haskell project template repository]:
  https://github.com/vst/haskell-template-hebele
[QEMU]: https://www.qemu.org/
[AWS Summit 2025]: https://aws.amazon.com/events/summits/singapore/
[ClickHouse]: https://clickhouse.com/
[GitLab]: https://about.gitlab.com/
[Graviton]: https://aws.amazon.com/ec2/graviton/
[upx]: https://upx.github.io/
[Apparently]:
  https://discourse.nixos.org/t/docker-ignoring-platform-when-run-in-nixos/21120/20
