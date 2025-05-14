---
title: "Hacking Watson with Haskell - Part 1"
date: 2024-08-15 21:33:55
description: Using Haskell to read Watson frames from a JSON file.
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - Hacking
    - Literate Programming
    - Computing
---

[Watson] is a command-line tool that helps you to track your time. It is simple
and powerful, yet it lacks some features that I would like to have. In this blog
post, I will start hacking [Watson] with Haskell.

<!--more-->

## Motivation

I have been using [Watson] since end of 2022. It is a simple and powerful tool
that taught me how I spend my time.

On one hand, it lacks some features such as taking notes, integrating with my
calendar or TODO list, etc... On the other hand, it keeps the data in a very
simple format that can be easily read and manipulated.

I have been thinking about hacking [Watson] for a while. I have some ideas such
as bulk editing, integrating with my pomodoro timer and tracking if my daily
plan and its execution are aligned.

I am planning to do a few blog posts both to document my journey and to share my
progress.

## Program

This blog post is a Literate Haskell program that reads the Watson frames from a
JSON file and prints them to the standard output. That's it for this blog post.

Before we start, how does a Watson `frames` file look like?

A Watson `frames` file is a JSON array of Watson frames. And a Watson frame is a
JSON array with 6 elements:

```json
[
  1629043200, // Start time
  1629046800, // End time
  "project", // Project name
  "frame-id", // Unique frame ID
  ["tag1", "tag2"], // Tags
  1629046800 // Update time
]
```

Let's start with the language extensions:

```haskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
```

We will use one Haskell package in addition to the libraries coming with the
GHC: [aeson]. Here are our imports:

```haskell
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Environment (getArgs)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
```

Our entry point function has 3 jobs to do:

1. Read the path to the frames JSON file from the command line arguments.
2. Attempt to read the frames from the file.
3. Print the frames to the standard output if reading is successful, otherwise
   print an error message.

```haskell
main :: IO ()
main = do
```

First, attempt to read the path from the command line arguments. Note that the
`head` function is a partial function and it will throw an exception if the list
is empty. We are using it here because we are expecting the path to the frames
JSON file as the first argument. And I am lazy:

```haskell
  fp <- head <$> getArgs
```

Then, attempt to read the frames from the file. We are using the `readFrames`
function that we will define later. It returns an `Either` value:

```haskell
  eFrames <- readFrames fp
```

By now, we have a result of type `Either String [Frame]`. We will pattern match
to print the frames to the standard output if reading is successful, otherwise
print an error message:

```haskell
  case eFrames of
    Left err -> putStrLn ("ERROR! " <> err)
    Right frames -> mapM_ print frames
```

That's as far as our `main` function goes. Now, let's define the `Frame` data
type which is a simple record type:

```haskell
data Frame = Frame
  { frameId :: !T.Text
  , frameSince :: !Time.UTCTime
  , frameUntil :: !Time.UTCTime
  , frameProject :: !T.Text
  , frameTags :: ![T.Text]
  , frameUpdatedAt :: !Time.UTCTime
  }
```

... and add the `Show` and `Eq` instances for it:

```haskell
  deriving (Show, Eq)
```

Our program still does not know how to read the `Frame` data type from a JSON
file. We will define an instance of the `FromJSON` type class for it:

```haskell
instance Aeson.FromJSON Frame where
  parseJSON v = do
```

Here, we will ask the parser to parse the JSON value into an array which is
inferred from the pattern match:

```haskell
    arr <- Aeson.parseJSON v
```

Let's pattern match. We need an array with exactly 6 elements:

```haskell
    case arr of
      [fSince, fUntil, fProj, fId, fTags, fUpdated] -> do
```

Now we can continue parsing individual elements. Note that we are using exactly
the same names for the fields as in the `Frame` record so that we can use
`RecordWildCards` extension to _fill_ the record at the end:

```haskell
        frameId <- Aeson.parseJSON fId
        frameSince <- fromEpoch <$> Aeson.parseJSON fSince
        frameUntil <- fromEpoch <$> Aeson.parseJSON fUntil
        frameProject <- Aeson.parseJSON fProj
        frameTags <- Aeson.parseJSON fTags
        frameUpdatedAt <- fromEpoch <$> Aeson.parseJSON fUpdated
        pure $ Frame {..}
```

If there no less or more than 6 elements in the array, we should fail with an
error message:

```haskell
      _ -> fail "Frame: expected an array of 6 elements"
```

Finally, we will define the `fromEpoch` function that converts an epoch time to
a `UTCTime` value:

```haskell
    where
      fromEpoch = posixSecondsToUTCTime . fromIntegral @Int
```

Now, we can define the `readFrames` function that reads the frames from a JSON
file:

```haskell
readFrames :: FilePath -> IO (Either String [Frame])
readFrames fp = do
  frames <- Aeson.eitherDecodeFileStrict fp
  pure $ case frames of
    Left err -> Left ("Failed to parse frames: " <> err)
    Right fs -> Right fs

```

Done!

As usual, let's create a symbolic link to the source code file:

```sh
ln -sr \
  content/posts/2024-08-15_hacking-watson-part-1.md  \
  content/posts/2024-08-15_hacking-watson-part-1.lhs
```

Let's download sample Watson frames JSON file:

```sh
curl -sLD - -o /tmp/frames.json https://raw.githubusercontent.com/TailorDev/Watson/master/tests/resources/autocompletion/frames
```

..., run our program:

```sh
runhaskell -pgmLmarkdown-unlit content/posts/2024-08-15_hacking-watson-part-1.lhs /tmp/frames.json
```

..., and see the output:

```txt
Frame {frameId = "41dcffb7bd74442794b9385c3e8891fc", frameSince = 2018-10-16 09:46:15 UTC, frameUntil = 2018-10-16 10:27:29 UTC, frameProject = "project1", frameTags = ["tag1"], frameUpdatedAt = 2018-10-16 10:27:29 UTC}
Frame {frameId = "e8b53f1dda684672806e0f347d2b11fc", frameSince = 2019-01-04 07:56:31 UTC, frameUntil = 2019-01-04 08:10:27 UTC, frameProject = "project2", frameTags = ["tag2"], frameUpdatedAt = 2019-01-04 08:10:27 UTC}
Frame {frameId = "ef9933131f254b6fa94dda2a85107195", frameSince = 2019-02-13 13:39:59 UTC, frameUntil = 2019-02-13 14:30:00 UTC, frameProject = "project1", frameTags = ["tag1"], frameUpdatedAt = 2019-02-13 14:51:33 UTC}
Frame {frameId = "f4f78aa79744440b9cbd28edef1ba0b0", frameSince = 2019-02-14 13:20:00 UTC, frameUntil = 2019-02-14 13:48:37 UTC, frameProject = "project3-A", frameTags = ["tag2"], frameUpdatedAt = 2019-02-14 13:48:37 UTC}
Frame {frameId = "10c6ff8612c84b239b5141cd04f10415", frameSince = 2019-02-22 10:00:00 UTC, frameUntil = 2019-02-22 10:34:02 UTC, frameProject = "project3-A", frameTags = ["tag2"], frameUpdatedAt = 2019-02-22 10:34:02 UTC}
Frame {frameId = "e113e26dbf8d4db3ba6361a73709ffd6", frameSince = 2019-03-07 10:20:00 UTC, frameUntil = 2019-03-07 11:06:08 UTC, frameProject = "project1", frameTags = ["tag2"], frameUpdatedAt = 2019-03-07 11:06:08 UTC}
Frame {frameId = "d5185c8e811a40efbad43d2ff775f5e8", frameSince = 2019-03-13 15:12:46 UTC, frameUntil = 2019-03-13 15:50:00 UTC, frameProject = "project2", frameTags = ["tag2"], frameUpdatedAt = 2019-03-14 07:50:48 UTC}
Frame {frameId = "379f567a9d584498aa8729a170b8b8ad", frameSince = 2019-03-25 09:45:14 UTC, frameUntil = 2019-03-25 10:28:55 UTC, frameProject = "project3-B", frameTags = ["tag2"], frameUpdatedAt = 2019-03-25 10:28:55 UTC}
Frame {frameId = "f4f7429d70454175bb87ce2254bbd925", frameSince = 2019-04-12 06:30:00 UTC, frameUntil = 2019-04-12 07:00:00 UTC, frameProject = "project4", frameTags = ["tag3"], frameUpdatedAt = 2019-04-12 10:00:10 UTC}
Frame {frameId = "af9fe637030a465ba279abc3c1441b66", frameSince = 2019-04-30 09:09:29 UTC, frameUntil = 2019-04-30 09:30:25 UTC, frameProject = "project3-B", frameTags = ["tag3"], frameUpdatedAt = 2019-04-30 09:30:26 UTC}
```

## Wrap-Up

This was an exciting blog post for me, because it is a new journey to new
absurds.

I needed to read the Watson frames from a JSON file, so that I can start
manipulating them. We now have an internal data definition for the Watson frames
and a program that reads them from a JSON file. We are ready for the next
step...

<!-- REFERENCES -->

[Watson]: http://tailordev.github.io/Watson/
[aeson]: https://hackage.haskell.org/package/aeson
