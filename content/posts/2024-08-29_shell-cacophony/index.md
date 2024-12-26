---
title: "Shell Cacophony"
date: 2024-08-29 20:05:47
description: Using jq, qsv, and uplot to waste more time in the terminal.
taxonomies:
  tags:
    - Technical Note
    - Unix
    - Hacking
    - Computing
---

I am using [`jq`][jq], [`qsv`][qsv], [`uplot`][uplot] quite often. This post is
to make sure that you know and use them, too. I hope you will waste as much time
as I do, especially with [`uplot`][uplot].

<!-- more -->

## Motivation

This post is for people like me who are facing a problem, reaching out to the
terminal and piping random UNIX commands to solve it, but eventually regret not
starting with Python or some other programming language in the first place.

I just want to make sure that we have some more shell goodies in our toolbox so
that we keep doing the same thing over and over again: [`jq`][jq], [`qsv`][qsv],
[`uplot`][uplot].

## `jq`

I am pretty sure that you have heard about [`jq`][jq] before, and most of you
are already using it. But for those who are not familiar with it, `jq` is a
lightweight and command-line JSON processor, sort of like `sed` for JSON data.

For example, use [xColors API] to generate 10 random colors and extract the hex codes
to print them:

```bash
curl -s "https://x-colors.yurace.pro/api/random?number=10" |
  jq -r ".[]|.hex"
```

## `qsv`

[`qsv`][qsv] is a command-line tool to work with CSV files. It is the successor
of [`xsv`][xsv] and is written in Rust. Current progress is quite impressive as
[`qsv`][qsv] now has SQL and Lua support.

For example, if we want to tabulate bird emojis from [EmojiHub API]:

```bash
curl -s "https://emojihub.yurace.pro/api/all/group/animal-bird" |
  jq -r ".[]|[.name,.category,.group,.unicode[0]]|@csv" |
  qsv rename --no-headers name,category,group,unicode |
  qsv table
```

Here, we are using `jq` to extract the necessary fields and output records as
CSV. Then, we are using `qsv` to add column names and finally tabulate.

## `uplot`

[`uplot`][uplot] is a command-line tool to plot data on the terminal. Be warned
that it is quite addictive.

Let's check the number of important (`p <= 3`) `journald` entries per day over
the last 1 week:

```bash
journalctl --no-pager --since="1 week ago" --priority=3 --output=json --output-fields=__REALTIME_TIMESTAMP |
  jq -r .__REALTIME_TIMESTAMP |
  cut -c 1-10 |
  xargs -I{} date --utc -d @{} +%Y-%m-%d |
  uniq -c |
  awk '{print $2","$1}' |
  uplot bar -d,
```

Here, we are using `journalctl` to get the entries with priority less than or
equal to 3 in JSON format with realtime timestamp information only. Then, we are
using `jq` to extract the timestamp, `cut` to get the date first 10 digits of
the timestamp (in microseconds), `xargs` to convert the timestamp to date format
using `date`, `uniq` to count the number of entries per day, `awk` to format the
output as CSV, and finally `uplot` to plot the data.

The output looks like this (trust me, it will look much nicer and colourful on
your terminal):

```txt
           ┌                                        ┐
2024-08-22 ┤■ 1.0
2024-08-23 ┤■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 44.0
2024-08-24 ┤■■■■■ 8.0
2024-08-25 ┤■■■■■■■■■■■■■■■■■■■■■ 33.0
2024-08-26 ┤■■■■■■■■■■■■■■■■ 25.0
2024-08-27 ┤■■■■■■■■■■■■ 18.0
2024-08-28 ┤■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 53.0
2024-08-29 ┤■■■■■■ 10.0
           └                                        ┘
```

Likewise, we can get the number of entries by unit since yesterday:

```sh
journalctl --no-pager --since=yesterday --priority=3 --output=json --output-fields=_SYSTEMD_UNIT |
  jq -r ._SYSTEMD_UNIT |
  sort |
  uniq -c |
  awk '{print $2","$1}' |
  uplot bar -d,
```

... which produces the following completely unsurprising output:

```txt
                                    ┌                                        ┐
                  bluetooth.service ┤■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 58.0
                       cups.service ┤■ 1.0
                                    ⋮
                               null ┤■■ 4.0
systemd-coredump@0-378352-0.service ┤■ 1.0
                                    ⋮
                                    └                                        ┘
```

## Conclusion

I hope you find these tools useful.

Closing with our motto: _Stick to the terminal and keep piping random UNIX
commands to (not) solve our problems!_

## Bonus

`qsv` is currently not available on nixpkgs. So, I fixed it for you:

```nix
{ stdenv
, lib
, fetchzip
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  pname = "qsv";
  version = "0.132.0";

  src = fetchzip {
    url = "https://github.com/jqnatividad/qsv/releases/download/${version}/qsv-${version}-x86_64-unknown-linux-gnu.zip";
    hash = "sha256-yko+wTSGxOZWU1cJS17sPYPQeBcfyeiwQUu6dPhpL1s=";
    stripRoot = false;
  };

  nativeBuildInputs = [
    autoPatchelfHook
    stdenv.cc.cc.lib
  ];

  buildInputs = [ ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    install -m755 -D source/qsvp $out/bin/qsv
    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/jqnatividad/qsv";
    description = "CSVs sliced, diced & analyzed.";
    platforms = platforms.linux;
  };
}
```

<!-- REFERENCES -->

[jq]: https://stedolan.github.io/jq/
[qsv]: https://github.com/jqnatividad/qsv
[uplot]: https://github.com/red-data-tools/YouPlot
[xsv]: https://github.com/BurntSushi/xsv
[xColors API]: https://x-colors.yurace.pro/
[EmojiHub API]: https://github.com/cheatsnake/emojihub
