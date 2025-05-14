---
title: "Hacking Watson with Haskell - Part 2"
date: 2024-08-16 22:50:46
description: Using Haskell to read Watson state from a JSON file.
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - Hacking
    - Literate Programming
    - Computing
---

In the [previous blog post], we read the [Watson] frames from a JSON file. In
this blog post, we will read the [Watson] state file and print it to the
standard output.

<!--more-->

## Watson State

In addition to the frames, [Watson] also stores the state of the frame in a
separate JSON file, called `state`. As far as I understand, there can be 3
possible states at a given time:

1. There is no state file: You have installed [Watson], but you have not started
   tracking time yet.
2. The state file is empty: You have started using [Watson], but currently not
   tracking any time.
3. The state file contains some data: You are tracking time at the moment.

When we say that the state file is empty, actually it is an empty JSON object:

```json
{}
```

When the state file contains some data, it is a JSON object with 3 keys,
`project`, `start` time and `tags` list:

```json
{
  "project": "project1",
  "start": 1629043200,
  "tags": ["tag1", "tag2"]
}
```

Let's work with this state file...

## Program

This blog post is a Literate Haskell program that attempts to read the Watson
state from a JSON file and prints it to the standard output. That's it for this
very blog post.

Let's start with the language extensions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
```

We will use [aeson] package like in the previous post, in addition to the
libraries coming with the GHC. Let's run our imports:

```haskell
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
```

Our entry point function is `main` as usual. Here is what it will do:

1. Read the path to the state JSON file from the command line arguments.
2. Attempt to read the state from the file.
3. Print the current state to the standard output if reading is successful,
   otherwise print an appropriate message.

```haskell
main :: IO ()
main = do
```

First, attempt to read the path from the command line arguments:

```haskell
  fp <- head <$> getArgs
```

Then, attempt to read the state from the file:

```haskell
  mState <- readState fp
```

By now, we have a result of type `Maybe CurrentState`. We will pattern match to
print the state if it is available:

```haskell
  case mState of
    Just state -> print state
    Nothing -> putStrLn "State file can not be parsed."
```

That's all what `main` function does. Now, let's define the `CurrentState` data
type which is a _sum_ type encoding both no-tracking and tracking states:

```haskell
data CurrentState
  = CurrentStatePending
  | CurrentStateRunning
      { currentStateRunningSince :: !Time.UTCTime
      , currentStateRunningProject :: !T.Text
      , currentStateRunningTags :: ![T.Text]
      }
```

... and add the `Show` and `Eq` instances for it:

```haskell
  deriving (Show, Eq)
```

Let's define an instance of the `FromJSON` type class for it. We will match
against a JSON object:

```haskell
instance Aeson.FromJSON CurrentState where
  parseJSON = Aeson.withObject "CurrentState" $ \o -> do
```

If the object is empty, we will return `CurrentStatePending`:

```haskell
    if null o
      then pure CurrentStatePending
```

..., otherwise try to parse the `project`, `start` and `tags` fields:

```haskell
      else CurrentStateRunning
             <$> (fromEpoch <$> o Aeson..: "start")
             <*> o Aeson..: "project"
             <*> o Aeson..: "tags"
```

... where our `fromEpoch` function converts an epoch time to a `UTCTime` value:

```haskell
    where
      fromEpoch = posixSecondsToUTCTime . fromIntegral @Int
```

Now, we can define the `readState` function:

```haskell
readState :: FilePath -> IO (Maybe CurrentState)
readState fp = do
  exists <- doesFileExist fp
  if exists
    then do
      mState <- Aeson.eitherDecodeFileStrict fp
      pure $ case mState of
        Left _ -> Nothing
        Right state -> Just state
    else pure $ Just CurrentStatePending
```

Done!

As usual, let's create a symbolic link to the source code file:

```sh
ln -sr \
  content/posts/2024-08-16_hacking-watson-part-2.md  \
  content/posts/2024-08-16_hacking-watson-part-2.lhs
```

..., run our program:

```sh
runhaskell -pgmLmarkdown-unlit content/posts/2024-08-16_hacking-watson-part-2.lhs ~/.config/watson/state
```

..., and see the output:

```txt
CurrentStateRunning {currentStateRunningSince = 2024-08-16 14:53:57 UTC, currentStateRunningProject = "vst", currentStateRunningTags = ["gh:vst/vst.github.io"]}
```

## Wrap-Up

With this blog post, we completed the _reading_ part of the [Watson] files. In
the next blog post, we will write the state file and start tracking time with
Haskell instead of the [Watson] CLI.

<!-- REFERENCES -->

[Watson]: http://tailordev.github.io/Watson/
[aeson]: https://hackage.haskell.org/package/aeson
[previous blog post]: /posts/hacking-watson-with-haskell-part-1/
