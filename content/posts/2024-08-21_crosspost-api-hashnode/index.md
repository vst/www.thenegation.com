---
title: "Cross-Posting to Hashnode with API"
date: 2024-08-21 17:42:49
description: Let's cross-post this blog post to Hashnode using its API.
taxonomies:
  tags:
    - Technical Note
    - Hacking
    - Computing
---

In my previous post, I showed how to cross-post to [Dev.to] using its API. In
this post, I will show how to cross-post to [Hashnode] using its API.

<!-- more -->

## Getting Started

[Dev.to]'s API is a REST API. It is pretty straightforward to use. [Hashnode]'s
API is a [GraphQL API]. If you are not familiar with GraphQL, you can think of it
as a query language running over HTTP that can traverse and manipulate the data graph.

[Hashnode] API is [well-documented]. Also, it comes with a [GraphiQL]
[playground]. You can use the playground to explore the API and test your
queries.

Most of the queries to the Hashnode API should work without authentication.
However, since we are going to create a (draft) article, we need to authenticate
the API requests. To do so, we need to obtain an API key from the [Hashnode
settings page].

Also, Hashnode needs to know which "publication" we are targeting. You can find
it in the URL of your publication's (or your blog's) dashboard. It looks
something like this:

```txt
https://hashnode.com/0123456789abcdef01234567/dashboard
```

So, the `publication ID` is then `0123456789abcdef01234567`.

I have put the API key and the publication ID in my `.envrc` file:

```sh
export THENEGATION_HASHNODE_APIKEY="MyVerySecretApiKey"
export THENEGATION_HASHNODE_PUBLICATION_ID="0123456789abcdef01234567"
```

## Create a New Article

We will use the following GraphQL mutation operation to create a new article:

```graphql
mutation CreateDraft($input: CreateDraftInput!) {
  createDraft(input: $input) {
    draft {
      id
      slug
      title
    }
  }
}
```

What does this mean?

- `CreateDraft` is the name of the operation.
- `$input` is a variable that will be passed to the operation.
- `createDraft` is the mutation operation.
- `draft` is the object that will be returned by the operation with the `id`,
  `slug`, and `title` fields.

The test `$input` looks like this:

```json
{
  "input": {
    "publicationId": "0123456789abcdef01234567",
    "title": "string",
    "contentMarkdown": "string"
  }
}
```

Then, we can use the following `curl` command to create a draft article:

```sh
curl \
  -X POST \
  -H "content-type: application/json" \
  -H "authorization: ${THENEGATION_HASHNODE_APIKEY}" \
  -d '{
    "query": "mutation CreateDraft($input: CreateDraftInput!) { createDraft(input: $input) { draft { id slug title } } }",
    "variables": {
      "input": {
        "publicationId": "${THENEGATION_HASHNODE_PUBLICATION_ID}",
        "title": "My New Article",
        "contentMarkdown": "This is the content of my new article.",
      }
    }
  }' \
  "https://gql.hashnode.com/"
```

_It worked on my computer!_

## Scripting?

A little bit self-plagiarism is not bad, right? I have taken the script from my
previous post and modified it to work with Hashnode:

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
```

I added this script to my Nix shell with name `dev-cross-post-hashnode`:

```sh
dev-cross-post-hashnode content/posts/2024-08-21_crosspost-api-hashnode.md
```

Done! Going forward, I can cross-post to Hashnode with a single command, almost,
as I still need to make a few adjustment to the draft and then publish on
Hashnode Website, but it is a good start.

<!-- REFERENCES -->

[Dev.to]: https://dev.to
[Hashnode]: https://hashnode.com
[GraphQL API]: https://graphql.org
[well-documented]: https://apidocs.hashnode.com
[GraphiQL]: https://github.com/graphql/graphiql
[playground]: https://gql.hashnode.com
[Hashnode settings page]: https://hashnode.com/settings/developer
