#!/usr/bin/env bash

## Purpose: Create an untracked static/version.json file containing
##          the current version, commit and build time.

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

version="$(jq '.version' package.json)"
commit="$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
buildTime="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

jq \
  --null-input \
  --compact-output \
  --arg version "$version" \
  --arg commit "$commit" \
  --arg buildTime "$buildTime" \
  '{version: $version, commit: $commit, buildTime: $buildTime}' \
  >static/version.json
