---
title: "Magic JSON in Haskell"
date: 2025-05-23 21:19:46
description: >-
  Maximize your return on investment into writing boilerplate with autodocodec
  Haskell library.
taxonomies:
  tags:
    - Technical Notes
    - Hacking
    - Haskell
---

Among all Haskell libraries I have used, the one I reach for the most is
[autodocodec]. I will explain what it is and what freebies it gives you.

<!-- more -->

I have been thinking about writing this post for a while, but I never got around
to it. There is no right time to talk about it, so it is now.

## JSON in Haskell

It is almost too simple. [aeson] library, the most popular JSON library in
Haskell, defines a JSON value as:

```haskell
data Value
  = Object !Object
  | Array !Array
  | String !Text
  | Number !Scientific
  | Bool !Bool
  | Null
```

Haskell data types can have JSON value representations:

```haskell
data Post = MkPost
  { postTitle :: Text
  , postContent :: Text
  , postPublished :: Bool
  }
  deriving (Show)


instance ToJSON Post where
  toJSON (MkPost title content published) =
    object
      [ "title" .= title
      , "content" .= content
      , "published" .= published
      ]


instance FromJSON Post where
  parseJSON = withObject "Post" $ \o ->
    MkPost
      <$> o .: "title"
      <*> o .: "content"
      <*> o .: "published"
```

This way, we can work with the JSON representation of our data types:

```haskell
>>> encode (MkPost "Hello, World!" "This is a very short post." True)
{"content":"This is a very short post.","published":true,"title":"Hello, World!"}
>>> decode "{\"content\":\"This is a very short post.\",\"published\":true,\"title\":\"Hello, World!\"}" :: Maybe Post
Just (MkPost {postTitle = "Hello, World!", postContent = "This is a very short post.", postPublished = True})
```

If you are coming from JavaScript or TypeScript, this is what you are used to do
with `JSON.parse` and [zod]. And you are right, in Haskell, this is quite
verbose and boilerplate-driven. But it is quite principled and type-safe. For
example, you can parse and map, but cannot perform `IO` operations when defining
your `ToJSON` and `FromJSON` instances. Also, you cannot have multiple JSON
representations of the same data type (so-called type-class instance coherence
in Haskell). You need [newtype] wrappers for that:

```haskell
newtype QuickPost = MkQuickPost Post
  deriving (Show)


instance ToJSON QuickPost where
  toJSON (MkQuickPost (MkPost title content _)) =
    object
      [ "subject" .= title
      , "body" .= content
      , "published" .= True
      ]


instance FromJSON QuickPost where
  parseJSON = withObject "QuickPost" $ \o ->
    MkQuickPost
      <$> (MkPost
              <$> o .: "subject"
              <*> o .: "body"
              <*> o .: "published"
          )
```

We are referring to the same data type, but with a different JSON
representation, without any runtime overhead:

```haskell
>>> encode (MkQuickPost (MkPost "Hello, World!" "This is a very short post." True))
{"body":"This is a very short post.","published":true,"subject":"Hello, World!"}
>>> decode "{\"body\":\"This is a very short post.\",\"published\":true,\"subject\":\"Hello, World!\"}" :: Maybe QuickPost
Just (MkQuickPost (MkPost {postTitle = "Hello, World!", postContent = "This is a very short post.", postPublished = True}))
```

## JSON Field Order

You might have noticed that the order of fields in the JSON output is not the
same as the order of fields in the Haskell data type. This is because [aeson]
does not guarantee the order of fields in the JSON output if you define `ToJSON`
without `toEncoding`. `toEncoding` would be a more efficient and predictable way
to define how we output the JSON representation. If you do not define it,
[aeson] will use `toJSON` to convert the data type to a JSON `Value`, and use
`Value`'s `toEncoding` to output the JSON. This is when the order of fields is
based on what is pulled from the `Map` in the `Object` constructor, which is not
necessarily the same as the order of fields in the Haskell data type.

Let us try to define `toEncoding` for our `Post` data type:

```haskell
instance ToJSON Post where
  toJSON (MkPost title content published) =
    object
      [ "title" .= title
      , "content" .= content
      , "published" .= published
      ]

  toEncoding (MkPost title content published) =
    pairs
      (  "title" .= title
      <> "content" .= content
      <> "published" .= published
      )
```

And the result is:

```haskell
>>> encode (MkPost "Hello, World!" "This is a very short post." True)
{"title":"Hello, World!","content":"This is a very short post.","published":true}
```

## What if I am lazy?

If you are lazy enough, you will not want to define `ToJSON` and `FromJSON`
instances for your data types manually. You will want to use [Generic] or
[TemplateHaskell] to derive them automatically.

I have used the `Generic` approach for a very long time. It is quite simple and
you might even want to centralize your JSON style to enforce it across your data
types:

```haskell
data Post = MkPost
  { postTitle :: Text
  , postContent :: Text
  , postPublished :: Bool
  }
  deriving (Show, Generic, Eq)


instance ToJSON Post where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 4
  }


instance FromJSON Post where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 4
  }
```

... which will give us:

```haskell
>>> encode (MkPost "Hello, World!" "This is a very short post." True)
{"title":"Hello, World!","content":"This is a very short post.","published":true}
>>> decode "{\"content\":\"This is a very short post.\",\"published\":true,\"title\":\"Hello, World!\"}" :: Maybe Post
Just (MkPost {postTitle = "Hello, World!", postContent = "This is a very short post.", postPublished = True})
```

Do not worry much about the `fieldLabelModifier` function. You can create your
own `options` generator that would work for all your data types as long as you
tell it how to drop the prefix from the field names:

```haskell
myOptions :: String -> Options
myOptions prefix = defaultOptions {
  fieldLabelModifier = camelTo2 '_' . drop (length prefix)
}
```

... then:

```haskell
instance ToJSON Post where
  toEncoding = genericToEncoding (myOptions "post")

instance FromJSON Post where
  parseJSON = genericParseJSON (myOptions "post")
```

## What if I am even lazier?

OK, if you have not used Haskell before, by now, you might have already given
up. But this approach of defining type-class instances is quite common in
Haskell. They often involve boilerplate, but they provide significant safety and
discipline.

What if we want to generate the YAML representations? How about JSON schema or
OpenAPI schema?

They all have their own type-classes (`ToSchema`) or piggyback on the existing
ones (for example, [yaml] uses [aeson] for decoding and encoding).

This is where [autodocodec] comes in. It is a library that allows you to
automatically derive instances such as `ToJSON`, `FromJSON`, `ToSchema` and
`FromSchema`. You will write the boilerplate once, and then you will be able to
generate the rest of the instances automatically.

Let us reproduce the previous example using [autodocodec]:

```haskell
data Post = MkPost
  { postTitle :: Text
  , postContent :: Text
  , postPublished :: Bool
  }
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Post)


instance HasCodec Post where
  codec =
    object "Post" $
      MkPost
        <$> requiredField "title" "Title of the post" .= postTitle
        <*> requiredField "content" "Content of the post" .= postContent
        <*> requiredField "published" "Publish status of the post" .= postPublished
```

... and:

```haskell
>>> encode (MkPost "Hello, World!" "This is a very short post." True)
{"title":"Hello, World!","content":"This is a very short post.","published":true}
>>> decode "{\"content\":\"This is a very short post.\",\"published\":true,\"title\":\"Hello, World!\"}" :: Maybe Post
Just (MkPost {postTitle = "Hello, World!", postContent = "This is a very short post.", postPublished = True})
```

We wrote more, but did we get more? Yes, we did. Look at this:

```console
>>> Data.Text.IO.putStrLn $ renderColouredSchemaViaCodec @Post
# Post
title: # required
  # Title of the post
  <string>
content: # required
  # Content of the post
  <string>
published: # required
  # Publish status of the post
  <boolean>
```

With the help of `renderColouredSchemaViaCodec` function from
[autodocodec-yaml], we can generate a coloured schema for our data type. Cool,
right?

Let us do more, with the help of `jsonSchemaViaCodec` function from
[autodocodec-schema]:

```haskell
>>> encode $ jsonSchemaViaCodec @Post
{
  "$comment": "Post",
  "properties": {
    "content": {
      "$comment": "Content of the post",
      "type": "string"
    },
    "published": {
      "$comment": "Publish status of the post",
      "type": "boolean"
    },
    "title": {
      "$comment": "Title of the post",
      "type": "string"
    }
  },
  "required": [
    "published",
    "content",
    "title"
  ],
  "type": "object"
}
```

## Conclusion

I really like [autodocodec]. I am using it heavily in my projects, especially
when I need to generate JSON Schema (for example, to aid configuration file
specifications) or OpenAPIv3 Schema (for [Servant] projects). I recently noticed
that there is even [autodocodec-nix] as an interpreter for Nix expressions.

I am fine with writing boilerplate as long as it gives me type-safety and
discipline. Using [autodocodec], I feel like maximizing my return on investment
into writing boilerplate.

<!-- REFERENCE -->

[Generic]: https://wiki.haskell.org/Generics
[Servant]: https://hackage.haskell.org/package/servant
[TemplateHaskell]: https://wiki.haskell.org/Template_Haskell
[aeson]: https://hackage.haskell.org/package/aeson
[autodocodec-nix]: https://hackage.haskell.org/package/autodocodec-nix
[autodocodec-schema]: https://hackage.haskell.org/package/autodocodec-schema
[autodocodec-yaml]: https://hackage.haskell.org/package/autodocodec-yaml
[autodocodec]: https://hackage.haskell.org/package/autodocodec
[newtype]: https://wiki.haskell.org/Newtype
[yaml]: https://hackage.haskell.org/package/yaml
[zod]: https://zod.dev/
