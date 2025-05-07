#!/usr/bin/env bash

## Get the path to the Markdown file:
_path="${1}"

## Extract the title from the front-matter of the Markdown file:
_title="$(yq --front-matter=extract .title "${_path}")"

## Convert the Markdown file to target format (commonmark):
_body="$(dev-md-format "${_path}")"

## Get the directory of the Markdown file:
_dirname=$(dirname -- "${_path}")

## Extract the slug from the filename:
_slug="$(echo "${_dirname%.*}" | cut -f 2- -d "_")"

## Build the URL for the post:
_url="https://thenegation.com/posts/${_slug}/"

## Build the payload for the API request:
_payload="$(
  jq \
    --null-input \
    --arg title "${_title}" \
    --arg body "${_body}" \
    --arg url "${_url}" \
    '{
    "article": {
      "title": $title,
      "body_markdown": $body,
      "published": false,
      "canonical_url": $url
    }
  }'
)"

## Make the API request:
curl \
  -sX POST \
  -H "api-key: ${THENEGATION_DEVTO_APIKEY}" \
  -H "content-type: application/json" \
  -d "${_payload}" \
  "https://dev.to/api/articles" |
  jq -r .url
