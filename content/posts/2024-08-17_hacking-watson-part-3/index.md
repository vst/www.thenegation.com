---
title: "Hacking Watson with Haskell - Part 3"
date: 2024-08-17 21:02:23
description: Using Haskell to start and stop Watson timer.
taxonomies:
  tags:
    - Technical Note
    - Haskell
    - Hacking
    - Literate Programming
    - Computing
---

In the previous blog posts ([part 1], [part 2]), we managed to read [Watson]
frames and state from its JSON files. In this blog post, we will do something
more useful: start and stop timer.

<!-- more -->

## Program

This blog post is a Literate Haskell program that attempts to start/stop
[Watson] timer. We will build on top of the previous blog posts ([part 1], [part
2]). If you haven't read them, I recommend you to read them first.

Let's start with the language extensions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
```

We will use [aeson] package like in the previous post, in addition to the
libraries coming with the GHC. Also, we will use [directory] and [uuid]
packages. Finally, we will use the infamous [optparse-applicative] library.
Let's declare our imports:

```haskell
import Control.Applicative ((<**>))
import Control.Monad (join)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Options.Applicative qualified as OA
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import System.Environment (getArgs)
```

First of all, we will hardcode filepaths to the Watson JSON files:

```haskell
getFileFrames :: IO FilePath
getFileFrames =
  getXdgDirectory XdgConfig "watson/frames"


getFileState :: IO FilePath
getFileState =
  getXdgDirectory XdgConfig "watson/state"
```

Our program represents time as `UTCTime` although Watson uses epoch time. These
are our conversion functions:

```haskell
fromEpoch :: Int -> Time.UTCTime
fromEpoch =
  posixSecondsToUTCTime . fromIntegral


toEpoch :: Time.UTCTime -> Int
toEpoch =
  floor . utcTimeToPOSIXSeconds
```

## Working with Frames

Let's improve our `Frame` data type and how we read/write it:

```haskell
data Frame = Frame
  { frameId :: !T.Text
  , frameSince :: !Time.UTCTime
  , frameUntil :: !Time.UTCTime
  , frameProject :: !T.Text
  , frameTags :: ![T.Text]
  , frameUpdatedAt :: !Time.UTCTime
  }
  deriving (Show, Eq)


instance Aeson.FromJSON Frame where
  parseJSON v = do
    arr <- Aeson.parseJSON v
    case arr of
      [fSince, fUntil, fProj, fId, fTags, fUpdated] -> do
        frameId <- Aeson.parseJSON fId
        frameSince <- fromEpoch <$> Aeson.parseJSON fSince
        frameUntil <- fromEpoch <$> Aeson.parseJSON fUntil
        frameProject <- Aeson.parseJSON fProj
        frameTags <- Aeson.parseJSON fTags
        frameUpdatedAt <- fromEpoch <$> Aeson.parseJSON fUpdated
        pure $ Frame {..}
      _ -> fail "Frame: expected an array of 6 elements"


instance Aeson.ToJSON Frame where
  toJSON Frame {..} =
    Aeson.toJSON
      [ Aeson.toJSON (toEpoch frameSince)
      , Aeson.toJSON (toEpoch frameUntil)
      , Aeson.toJSON frameProject
      , Aeson.toJSON frameId
      , Aeson.toJSON frameTags
      , Aeson.toJSON (toEpoch frameUpdatedAt)
      ]


readFrames :: FilePath -> IO (Either String [Frame])
readFrames fp = do
  frames <- Aeson.eitherDecodeFileStrict fp
  pure $ case frames of
    Left err -> Left ("Failed to parse frames: " <> err)
    Right fs -> Right fs


writeFrames :: FilePath -> [Frame] -> IO ()
writeFrames =
  Aeson.encodeFile
```

## Working with State

Let's improve our `State` data type and how we read/write it:

```haskell
data CurrentState
  = CurrentStatePending
  | CurrentStateRunning
      { currentStateRunningSince :: !Time.UTCTime
      , currentStateRunningProject :: !T.Text
      , currentStateRunningTags :: ![T.Text]
      }
  deriving (Show, Eq)


instance Aeson.FromJSON CurrentState where
  parseJSON = Aeson.withObject "CurrentState" $ \o -> do
    if null o
      then pure CurrentStatePending
      else
        CurrentStateRunning
          <$> (fromEpoch <$> o Aeson..: "start")
          <*> o Aeson..: "project"
          <*> o Aeson..: "tags"


instance Aeson.ToJSON CurrentState where
  toJSON CurrentStatePending =
    Aeson.object []
  toJSON CurrentStateRunning {..} =
    Aeson.object
      [ "start" Aeson..= toEpoch currentStateRunningSince
      , "project" Aeson..= currentStateRunningProject
      , "tags" Aeson..= currentStateRunningTags
      ]


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


writeState :: FilePath -> CurrentState -> IO ()
writeState =
  Aeson.encodeFile
```

## Main Program

Our main program is a CLI program powered by [optparse-applicative]. It will
offer two subcommands to start and stop the timer. Let's define our options:

```haskell
opts :: OA.Parser (IO ())
opts =
  OA.subparser
    ( OA.command "start" (OA.info (startCommand <**> OA.helper) OA.idm)
        <> OA.command "stop" (OA.info (stopCommand <**> OA.helper) OA.idm)
    )


startCommand :: OA.Parser (IO ())
startCommand =
  start
    <$> OA.strOption (OA.long "project" <> OA.short 'p' <> OA.metavar "PROJECT")
    <*> OA.many (OA.strOption (OA.long "tag" <> OA.short 't' <> OA.metavar "TAG"))


stopCommand :: OA.Parser (IO ())
stopCommand = do
  pure stop
```

Good. Now, let's define the start function. It should be easy: If there is no
timer running (or `state` file exists), we will start the timer. Otherwise, we
will print an error message:

```haskell
start :: T.Text -> [T.Text] -> IO ()
start project tags = do
  fState <- getFileState
  mState <- readState fState
  case mState of
    Just CurrentStateRunning {} ->
      putStrLn "Already running"
    _ -> do
      putStrLn "Starting..."
      now <- Time.getCurrentTime
      writeState fState $ CurrentStateRunning now project tags
```

The stop function is just a bit more involved. We will read the state file and
if the timer is running, we will stop it. We will also write the frame to the
frames file. If the timer is not running, we will print an error message:

```haskell
stop :: IO ()
stop = do
  fState <- getFileState
  mState <- readState fState
  case mState of
    Just CurrentStateRunning {..} -> do
      putStrLn "Stopping..."
      now <- Time.getCurrentTime
      fFrames <- getFileFrames
      frames <- readFrames fFrames
      case frames of
        Left err -> putStrLn err
        Right fs -> do
          frameId <- T.replace "-" "" . UUID.toText <$> UUID.nextRandom
          let frame =
                Frame
                  { frameSince = currentStateRunningSince
                  , frameUntil = now
                  , frameProject = currentStateRunningProject
                  , frameTags = currentStateRunningTags
                  , frameUpdatedAt = now
                  , ..
                  }
          writeFrames fFrames (fs <> [frame])
          writeState fState CurrentStatePending
    _ -> putStrLn "Not running..."
```

Now, we can define our main function:

```haskell
main :: IO ()
main = do
  join $ OA.execParser (OA.info (opts <**> OA.helper) OA.idm)
```

## Wrap-Up

In just 3 blog posts, we managed to read/write Watson JSON files and start/stop
the timer.

From a functionality point of view, we are missing a lot of features. This is
what [Watson] offers:

```sh
$ watson --help
Usage: watson [OPTIONS] COMMAND [ARGS]...

  Watson is a tool aimed at helping you monitoring your time.

  You just have to tell Watson when you start working on your project with the
  `start` command, and you can stop the timer when you're done with the `stop`
  command.

Options:
  --version             Show the version and exit.
  --color / --no-color  (Don't) color output.
  --help                Show this message and exit.

Commands:
  add        Add time to a project with tag(s) that was not tracked live.
  aggregate  Display a report of the time spent on each project...
  cancel     Cancel the last call to the start command.
  config     Get and set configuration options.
  edit       Edit a frame.
  frames     Display the list of all frame IDs.
  help       Display help information
  log        Display each recorded session during the given timespan.
  merge      Perform a merge of the existing frames with a conflicting...
  projects   Display the list of all the existing projects.
  remove     Remove a frame.
  rename     Rename a project or tag.
  report     Display a report of the time spent on each project.
  restart    Restart monitoring time for a previously stopped project.
  start      Start monitoring time for the given project.
  status     Display when the current project was started and the time...
  stop       Stop monitoring time for the current project.
  sync       Get the frames from the server and push the new ones.
  tags       Display the list of all the tags.
```

And this is what we have:

```sh
$ runhaskell -pgmLmarkdown-unlit content/posts/2024-08-17_hacking-watson-part-3.lhs --help
Usage: 2024-08-17_hacking-watson-part-3.lhs COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  start
  stop
```

Also, our start and stop functions do not perform any validation or offer
options such as `--no-gap` or `--at`.

From a _Good Haskell_ point of view, we are missing a lot of things. For
example, we are not dealing with errors properly. We could have defined an error
data type and use it with `MonadError` to make sure that we cover possible error
cases and propagate them properly.

I know someone who is willing to learn Haskell. Maybe I can convince him to work
on this project.

<!-- REFERENCES -->

[Watson]: http://tailordev.github.io/Watson/
[aeson]: https://hackage.haskell.org/package/aeson
[directory]: https://hackage.haskell.org/package/directory
[uuid]: https://hackage.haskell.org/package/uuid
[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative
[part 1]: /posts/hacking-watson-with-haskell-part-1/
[part 2]: /posts/hacking-watson-with-haskell-part-2/
