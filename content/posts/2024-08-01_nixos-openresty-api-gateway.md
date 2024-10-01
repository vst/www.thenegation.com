---
title: OpenResty on NixOS for an API Gateway
date: 2024-08-01 11:45:40
description: A technical note on using OpenResty on NixOS as an API gateway.
taxonomies:
  tags:
    - Technical Notes
    - NixOS
    - Computing
---

Are you using an API gateway? Do you really need one? If you are using [NixOS]
and feel comfortable with some [Lua], you may want to consider [OpenResty] on
NixOS as an API gateway.

<!-- more -->

An _API Gateway_ is simply a service that acts as a reverse proxy for API
requests to one or more upstream services. Modern API gateways streamline the
management of many functionalities such as authentication, authorization, rate
limiting, caching, logging, monitoring, etc. There are quite a few such
proprietary and open-source API gateways available. Cloud service providers
offer their own managed API gateways as well.

We have been using [Apache APISIX] for a while now. It is a high-performance,
cloud-native API gateway solution. It also has a nice dashboard for managing
APIs. However, I have been looking for a simpler and more portable solution for
our use case. In particular, I want to be able manage the API gateway as a NixOS
service so that the configuration can be tested and redeployed easily.

Two of the fairly popular open-source API gateways, [Kong] and [Apache APISIX],
are based on [OpenResty] that is mainly powered by [Nginx] and [Lua]. NixOS has
first-class support for OpenResty. So, naturally, I decided to give it a try.
The result was promising enough for us to start migrating our current APISIX
deployments to OpenResty on NixOS.

In this post, I will show how to set up OpenResty as NixOS service along with a
few rudimentary location examples.

We will test the NixOS configuration on a QEMU-based virtual machine. For this,
I will use my short technical note on my blog, "[Running NixOS Guests on QEMU]".

## Configuration

Below is the `configuration.nix` file contents:

```nix
## file: ./configuration.nix
{ pkgs, ... }:

let
  thisVersion = "0.0.1";
in
{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.firewall.allowedTCPPorts = [ 22 80 ];

  users.users.root = {
    initialPassword = "hebele-hubele";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJIQtEmoHu44pUDwX5GEw20JLmfZaI+xVXin74GI396z"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3"
    ];
  };

  services.openssh.enable = true;

  services.nginx = {
    enable = true;
    package = pkgs.openresty;

    virtualHosts."localhost" = {
      default = true;

      locations."/" = {
        return = "200 'Hello World!'";
      };

      locations."/version" = {
        extraConfig = ''
          default_type application/json;

          content_by_lua_block {
            ngx.say("{\"version\":\"${thisVersion}\"}")
          }
        '';
      };

      locations."/proxy" = {
        proxyPass = "http://httpbin.org/";
      };

      locations."/proxy/rewrite" = {
        proxyPass = "http://httpbin.org/";
        extraConfig = ''
          header_filter_by_lua_block {
            if ngx.req.get_method() == "POST" and ngx.status == 200 then
              ngx.status = 201
            end
          }
        '';
      };
    };
  };

  system.stateVersion = "24.05";
}

```

## Explanation

This configuration is pretty straightforward if you are familiar with NixOS and
NixOS Nginx service options.

A few highlights:

1. Instead of using the default Nginx package, we set `services.nginx.package`
   to `pkgs.openresty`. This will install OpenResty instead of Nginx.
2. The `/` location is pretty much Nginx.
3. The `/proxy` location is proxying requests to `httpbin.org`, again a typical
   Nginx configuration option.
4. The `/version` location is using Lua to return a JSON response with the
   version number.
5. The `/proxy/rewrite` location is proxying requests to `httpbin.org` and
   changing the status code to `201` if the request method is `POST`.

The last one is when OpenResty shines. You can use Lua to manipulate the request
and response.

Note that the last example is actaully a production fix for us. We have a legacy
service that returns `200` for all successful requests. But, the more recent
versions of the service client requires an `201` response for successful `POST`
requests.

## Create and Launch VM

Let's build the virtual machine with the following command:

```sh
nix-build '<nixpkgs/nixos>' -A vm -I nixpkgs=channel:nixos-24.05 -I nixos-config=./configuration.nix
```

The built artifact can be found in the `result` symlink.

Then, launch the VM with `run-nixos-vm` script:

```sh
QEMU_KERNEL_PARAMS="console=ttyS0" QEMU_NET_OPTS="hostfwd=tcp:127.0.0.1:2222-:22,hostfwd=tcp:127.0.0.1:24680-:80" ./result/bin/run-nixos-vm -nographic; reset
```

... and test the configuration:

```sh
$ curl -D - http://localhost:24680
HTTP/1.1 200 OK
Server: openresty
Date: Thu, 01 Aug 2024 03:23:51 GMT
Content-Type: application/octet-stream
Content-Length: 12
Connection: keep-alive

Hello World!
```

Do we have a version?

```sh
$ curl -D - http://localhost:24680/version
HTTP/1.1 200 OK
Server: openresty
Date: Thu, 01 Aug 2024 03:24:33 GMT
Content-Type: application/json
Transfer-Encoding: chunked
Connection: keep-alive

{"version":"0.0.1"}
```

Let's proxy some requests:

```sh
$ curl -D - http://localhost:24680/proxy/anything/hello/world
HTTP/1.1 200 OK
Server: openresty
Date: Thu, 01 Aug 2024 03:25:29 GMT
Content-Type: application/json
Content-Length: 355
Connection: keep-alive
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true

{
  "args": {},
  "data": "",
  "files": {},
  "form": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/8.7.1",
    "X-Amzn-Trace-Id": "Root=1-66ab002a-0ccb8633321d4b9563c6c599"
  },
  "json": null,
  "method": "GET",
  "origin": "123.123.123.123",
  "url": "http://httpbin.org/anything/hello/world"
}
```

Let's rewrite the response status upon successful `POST` requests:

```sh
$ curl -D - -X POST http://localhost:24680/proxy/rewrite/anything
HTTP/1.1 201 Created
Server: openresty
Date: Thu, 01 Aug 2024 03:26:17 GMT
Content-Type: application/json
Content-Length: 344
Connection: keep-alive
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true

{
  "args": {},
  "data": "",
  "files": {},
  "form": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/8.7.1",
    "X-Amzn-Trace-Id": "Root=1-66ab0059-53f1b550657f07c10284f9e6"
  },
  "json": null,
  "method": "POST",
  "origin": "123.123.123.123",
  "url": "http://httpbin.org/anything"
}
```

Note that the `POST` request to `httpbin.org` would return `200` status code:

```sh
curl -D - -X POST http://httpbin.org/anything
HTTP/1.1 200 OK
Date: Thu, 01 Aug 2024 03:27:18 GMT
Content-Type: application/json
Content-Length: 344
Connection: keep-alive
Server: gunicorn/19.9.0
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true

{
  "args": {},
  "data": "",
  "files": {},
  "form": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/8.7.1",
    "X-Amzn-Trace-Id": "Root=1-66ab0096-7a3aa51e62f480b749d819f9"
  },
  "json": null,
  "method": "POST",
  "origin": "123.123.123.123",
  "url": "http://httpbin.org/anything"
}
```

## Final Remarks

I acknowledge the need for complex and expensive API gateway solutions for large
and busy services. But, for small and medium-sized services, a simple API
gateway can be sufficient.

OpenResty is a powerful tool that can be used for many purposes, potentially
growing into a custom, in-house API gateway solution. Deploying OpenResty on
NixOS gives us the ability to manage the configuration as code. This is an
advantage for testing and reproducibility, and brings us one step closer to
using OpenResty as an API gateway with endless possibilities.

Therefore, OpenResty on NixOS can be a good choice for both small and large
service setups and teams. I like the idea that I invest into OpenResty and NixOS
instead of a proprietary or particular solution.

But there can be some sharp edges in relation to NixOS. For example, I wanted to
demonstrate how to use OpenResty for securing APIs with OpenID. Currently, both
NixOS 24.05 and NixOS Unstable channels have broken `lua-resty-openidc`
dependencies. It may not be a big deal for seasoned NixOS users, but it is a
showstopper for newcomers and too much for a simple blog post.

<!-- REFERENCES -->

[Apache APISIX]: https://apisix.apache.org
[Kong]: https://konghq.com
[Lua]: https://www.lua.org
[Nginx]: https://nginx.org
[NixOS]: https://nixos.org
[OpenResty]: https://openresty.org
[Running NixOS Guests on QEMU]: https://thenegation.com/posts/nixos-on-qemu/
