#!/usr/bin/env bash
set -euo pipefail

## If HUGO_BASEURL is not set, try to determine it based on the environment
## variables provided by Cloudflare Pages:
if [[ -z "${HUGO_BASEURL:-}" ]]; then
  ## If CF_PAGES_BRANCH is "main", we can assume the site will be deployed to the
  ## root domain, so we can set HUGO_BASEURL to "https://www.thenegation.com/".
  ## Otherwise, if CF_PAGES_URL is set, we can use that as the base URL. If
  ## neither of these conditions are met, we will leave HUGO_BASEURL unset, and
  ## Hugo will default to whatever is specified in the config file (which is
  ## probably "/") or the default ("/").
  if [[ "${CF_PAGES_BRANCH:-}" == "main" ]]; then
    HUGO_BASEURL="https://www.thenegation.com/"
  elif [[ -n "${CF_PAGES_URL:-}" ]]; then
    HUGO_BASEURL="${CF_PAGES_URL}"
  fi
fi

## Export HUGO_BASEURL so that it is available to the Hugo build process:
export HUGO_BASEURL

## Install dependencies:
pnpm install --frozen-lockfile

## Build the site:
pnpm run build
