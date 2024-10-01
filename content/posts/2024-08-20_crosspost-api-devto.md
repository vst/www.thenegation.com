---
title: "Cross-Posting to Dev.to with API"
date: 2024-08-20 22:54:52
description: Let's cross-post this blog post to Dev.to using its API.
taxonomies:
  tags:
    - Technical Note
    - Hacking
    - Computing
---

Let's cross-post this blog post to [Dev.to] using its API.

<!-- more -->

## Getting Started

We will use the [Dev.to API] (Version 1) to create a new article on Dev.to. The
API requires an API key to authenticate the user. You can get the API key from
the [Dev.to settings page].

If you have obtained the API key, you can use it to authenticate the API
requests via the `api-key` header. Let's give it a try:

```sh
$ curl -sH "api-key: MyVerySecretApiKey" https://dev.to/api/users/me | jq .
{
  "type_of": "user",
  "id": 1431,
  "username": "username480",
  "name": "Willy \"Myron\" \\:/ Herzog",
  "twitter_username": "twitter480",
  "github_username": "github480",
  "summary": null,
  "location": null,
  "website_url": null,
  "joined_at": "Apr 14, 2023",
  "profile_image": "/uploads/user/profile_image/1431/b547e3a6-5076-44dd-a4f6-9b85022b4e76.jpeg"
}
```

OK, we are ready. Let's create a new article on Dev.to.

## Create a New Article

To create a new article, we need to send a POST request to the `/articles`
endpoint. The payload seems to be a JSON object with the following structure:

```json
{
  "article": {
    "title": "string",
    "body_markdown": "string",
    "published": false,
    "series": "string",
    "main_image": "string",
    "canonical_url": "string",
    "description": "string",
    "tags": "string",
    "organization_id": 0
  }
}
```

The `tags` field is documented to be a string, but it seems to be an array of
strings. Let's do a dry run (`published: false`):

```sh
curl https://dev.to/api/articles \
    -s \
    -H "api-key: MyVerySecretApiKey" \
    -H "content-type: application/json" \
    -X POST \
    -d '{
  "article": {
    "title": "Cross-Posting to Dev.to with API",
    "body_markdown": "Example markdown content",
    "published": false,
    "tags": ["devto", "technical"],
    "canonical_url": "https://example.com/path/to/article"
  }
}'
```

_It worked on my computer!_

## Scripting?

I have written a small script to automate the process. It consumes the path to
the markdown file as an argument and creates a draft article on Dev.to. I am not
setting tags as I would like to do it manually as per available tags on Dev.to.

```sh
#!/usr/bin/env bash

## Get the path to the Markdown file:
_path="${1}"

## Extract the title from the front-matter of the Markdown file:
_title="$(yq --front-matter=extract .title "${_path}")"

## Convert the Markdown file to target format (commonmark):
_body="$(dev-md-format "${_path}")"

## Get the filename of the Markdown file:
_filename=$(basename -- "${_path}")

## Extract the slug from the filename:
_slug="$(echo "${_filename%.*}" | cut -f 2- -d "_")"

## Build the URL for the post:
_url="https://thenegation.com/posts/${_slug}/"

## Build the payload for the API request:
_payload="$(jq \
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
```

For my convenience:

1. I added this script to my Nix shell with name `dev-cross-post-devto`.
2. I updated my `.envrc` file to export the `THENEGATION_DEVTO_APIKEY` variable.

So, now I can cross-post to Dev.to with a single command:

```sh
dev-cross-post-devto content/posts/2024-08-20_crosspost-api-devto.md
```

<!-- REFERENCES -->

[Dev.to]: https://dev.to
[Dev.to API]: https://docs.dev.to/api/
[Dev.to settings page]: https://dev.to/settings/extensions
