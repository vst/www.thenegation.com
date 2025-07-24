---
title: "Why and How to Patch a Python Package in Nix"
date: 2025-05-08 17:24:26
description: >
  How to apply a patch to a Python package in Nix without relying on unreleased
  code or maintaining a fork.

slug: patch-python-package-on-nix
tags:
  - Technical Notes
  - Dependency Management
  - Nix
  - Python
  - Django
---

I bumped into an annoying issue today while upgrading my Python dependencies in
a codebase. And I thought it would be a good idea to share the solution with
you. Thanks to [Nix] for making this kind of fix so straightforward.

<!--more-->

Admittedly, learning and getting used to Nix may take a while. But once you get
the hang of it, you will be amazed by its power, flexibility and reliability.
When I say _reliability_, I mean it: You can rely on Nix to look after you when
you drop your guard.

Let me show you how Nix can help you patch a Python package without having to
pin your package to a specific but unreleased commit from upstream or
maintaining a fork of it, and give you a bit of further reassurance that your
patch is not going to break anything else in your codebase.

## Background of the Story

I am maintaining a Python codebase that is based on [Django] and [Django REST
Framework][drf] (DRF). The codebase uses [Nix] for development, testing, CI/CD
and packaging.

DRF is quite stable and does not change often. The core author of DRF has
[declared] DRF to be feature complete. Most development now focuses on bug fixes
and keeping up with newer Django versions.

After upgrading to DRF v3.16.0, our tests started failing. The issue was related
partly to [UniqueTogetherValidator] and partly to nullable fields in
unique-together constraints, which is a known [limitation] of DRF.

Luckily, a [PR] was recently merged to DRF `master` that fixes a corner case of
[UniqueTogetherValidator]. Unfortunately, there was no new release of DRF after
the PR was merged, and the release cadence of DRF is quite slow nowadays.

## Problem

I reviewed the PR that consisted of three commits and touched only 2 files. It
was squash-merged into the `master` branch. There were a few other changes
merged into the `master` before and after our PR was merged.

The good thing was that none of the prior changes touched the files that were
changed in the PR. Also, the change we were interested in was small and easy to
reason about.

Now, I had a few options:

1. Use the `master` branch of DRF in my codebase. Overriding a Python package in
   `nixpkgs` is straightforward, but I did not want to assume responsibility for
   other commits.
2. Fork the DRF repository as of the v3.16.0 release and apply the changes from
   the PR. I would avoid unintended consequences of other changes, but I do not
   want to maintain a fork of DRF, even for a short time.
3. Pick the changes from the PR and apply them as a patch. But how?

## Solution

The first two solutions are not ideal. The third solution is uncommon in the
Python ecosystem. But if you're using Nix, patching Python packages becomes
easy.

So, let's see how to do that.

### Step 1: Create the Patch

First, we need the patch.

After checking out the DRF repository, I found the two commit hashes that I was
interested in:

1. `ac50cec76c9fae76a01931d748e69d003dd79b94` -- the v3.16.0 release commit.
2. `543996711d323722a1017e376619ae462a726ada` -- the commit that was
   squashed-merged from the PR into the `master` branch.

Then, I used `git diff` to create a patch file, but only for the files I was
interested in:

```sh
git diff \
  ac50cec76c9fae76a01931d748e69d003dd79b94 \
  543996711d323722a1017e376619ae462a726ada \
  -- rest_framework/validators.py tests/test_validators.py \
  > djangorestframework_3.16.0.1.patch
```

### Step 2: Override DRF in Nix Package Set

I moved the patch to my codebase, under `./nix/patches/python/`. Here is how I
applied it to the DRF package in `nixpkgs`:

```nix
{
    ## TRUNCATED FOR BREVITY...

    ## Our base Python:
    python = pkgs.python312.override {
      packageOverrides = self: super: {
        ## See: https://github.com/encode/django-rest-framework/pull/9688
        djangorestframework = super.djangorestframework.overridePythonAttrs (old: {
          patches = (old.patches or []) ++ [ ./nix/patches/python/djangorestframework_3.16.0.1.patch ];
        });
      };
    };

    ## TRUNCATED FOR BREVITY...
}
```

What it essentially does is to override the `djangorestframework` package in
`self` (ie. the resulting package set) with an overridden one `super` (ie. the
original package set). The override is achieved by calling the
`overridePythonAttrs` on the original package. The only attribute that we are
interested in is the `patches`. But we do not know if there are any patches
already applied to the original package (`old.patches`). We should preserve them
and append our patch to the list of patches. In case there are no patches
applied to the original package, we use an empty list `[]` as a default value.

Nice! We now have a patched version of DRF -- no need for a development version
or a custom fork.

## Bonus: Nix's Safety Net

Here is the most interesting part of the story. After I updated my `flake.nix`,
I immediately entered the Nix shell to test the patch. But it took a few minutes
for the Nix shell to be ready: Not only our patched DRF, but all dependent
packages were also rebuilt instead of being used from the cache!

Initially, I was a bit annoyed. Then I remembered that Nix is actually smarter
than I am: My little patch to DRF was not just affecting the DRF package, but
could impact all dependent packages as well. Nix always builds _and_ checks each
package and its dependencies once the hash of the package has changed. It does
the right thing even when you forget to think about it.

These small but meaningful benefits are exactly why I love Nix.

<!-- REFERENCES -->

[Django]: https://www.djangoproject.com/
[Nix]: https://nixos.org/
[PR]: https://github.com/encode/django-rest-framework/pull/9688
[UniqueTogetherValidator]:
  https://www.django-rest-framework.org/api-guide/validators/#uniquetogethervalidator
[declared]: https://github.com/encode/django-rest-framework/discussions/9130
[drf]: https://www.django-rest-framework.org/
[limitation]:
  https://www.django-rest-framework.org/api-guide/validators/#optional-fields
