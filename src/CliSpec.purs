module CliSpec where

import Prelude (Unit, bind, (#), ($), (/=), (<>))

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, cons, difference, filter, null, zip, (!!))
import Data.Array (head, tail, drop, take, reverse, foldl)
import Data.Bifunctor (lmap)
import Data.Eq ((==))
import Data.Foldable (foldMap, find, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over)
import Data.Result (Result(..), result, note, isOk, fromEither)
import Data.Show (show)
import Data.String (Pattern(..), indexOf, split)
import Data.Traversable (for_, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign.Object as Obj
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async (stat)
import Node.FS.Stats (isFile, isDirectory)
import Node.FS.Sync as Sync
import Node.Path as Path
import Node.Process (argv, cwd)

import CliSpec.Types

import Debug as Debug

-- TODO: Automatically disable colors if not supported
makeRed :: String -> String
makeRed str =
  withGraphics (foreground Red) str


errorAndExit :: String -> Effect Unit
errorAndExit message = do
  throw (makeRed message)


parseCliSpec :: String -> Result String CliSpec
parseCliSpec cliSpecJsonStr = do
  let cliSpecRes = fromEither $ jsonParser cliSpecJsonStr

  case cliSpecRes of
    Error msg -> Error msg
    Ok cliSpecJson -> do
      cliSpecJson
        # decodeJson
        # (lmap printJsonDecodeError)
        # fromEither


callCommand
  :: CliSpec
  -> String
  -> Array CliArgument
  -> (String -> String -> Array CliArgument -> Effect Unit)
  ->  Effect Unit
callCommand cliSpec usageString args executor = do
  case args # head of
    Just (CmdArg "help") -> log usageString
    Just (CmdArg "version") -> log cliSpec.version
    Just (CmdArg cmdName) -> do
      let
        commandMb = cliSpec.commands
            # find (\cmd -> cmd.name == cmdName)
        providedArgs = args # drop 1

      case commandMb of
        Nothing -> do
          log $
            makeRed ("ERROR: Unknown command \"" <> cmdName <> "\"")
            <> "\n\n"
            <> usageString

        Just command -> do
          if (length providedArgs :: Int) /= (length command.arguments)
          then do
            log $
              "Usage: " <> command.name
              <> (command.arguments # foldMap (\arg -> " " <> arg))
              <> "\n\n"
              <> command.description
              <> "\n\n"
          else do
            executor cmdName usageString providedArgs

    Just _ -> do
      log $
        makeRed $
          "ERROR: First argument must be a command"
          <> "\n\n"
          <> usageString

    Nothing -> do
      log $
        makeRed $
          "ERROR: No command specified"
          <> "\n\n"
          <> usageString


callCliApp
  :: CliSpec
  -> (String -> String -> Array CliArgument -> Effect Unit)
  -> Effect Unit
callCliApp cliSpec executor = do
  let
    usageString =
      "Usage: " <> cliSpec.name <> " <command> [options]"

  arguments <- argv

  -- -- TODO: Show in help
  -- binName <- case arguments # head of
  --   Nothing -> do
  --     errorAndExit usageString
  --     pure ""
  --   Just name -> pure name

  let binArgs = arguments # tail # fromMaybe []

  -- -- TODO: Show in help
  -- -- E.g. transity, or index.js
  -- appName <- case binArgs # head of
  --   Nothing -> do
  --     errorAndExit usageString
  --     pure ""
  --   Just name -> pure name

  let toolArgs = binArgs # tail # fromMaybe []

  callCommand
    cliSpec
    usageString
    (parseCliArguments toolArgs)
    executor
