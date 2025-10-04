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
    --arg slug "${_slug}" \
    --arg body "${_body}" \
    --arg url "${_url}" \
    --arg publication "${THENEGATION_HASHNODE_PUBLICATION_ID}" \
    '{
  "query": "mutation CreateDraft($input: CreateDraftInput!) { createDraft(input: $input) { draft { id slug title } } }",
  "variables": {
    "input": {
      "title": $title,
      "publicationId": $publication,
      "contentMarkdown": $body,
      "slug": $slug,
      "originalArticleURL": $url,
      "metaTags": {
        "title": $title
      }
    }
  }
}'
)"

## Make the API request:
curl \
  -sX POST \
  -H "authorization: ${THENEGATION_HASHNODE_APIKEY}" \
  -H "content-type: application/json" \
  -d "${_payload}" \
  "https://gql.hashnode.com/" |
  jq .
