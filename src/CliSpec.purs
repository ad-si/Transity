module CliSpec where

import Prelude (Unit, bind, (#), ($), (/=), (<>), (-), (>))

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, head, tail, replicate, foldl, foldMap, fold, find, length)
import Data.Bifunctor (lmap)
import Data.String as Str
import Data.Eq ((==))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Result (Result(..), fromEither)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Node.Process (argv)

import CliSpec.Types


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
    Just (FlagLong "help") -> log usageString
    Just (FlagShort 'h') -> log usageString

    Just (CmdArg "version") -> log cliSpec.version
    Just (FlagLong "version") -> log cliSpec.version
    Just (FlagShort 'v') -> log cliSpec.version

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
      log usageString


-- | Function to repeat a string n times
repeatString :: String -> Int -> String
repeatString str n =
  fold $ replicate n str


callCliApp
  :: CliSpec
  -> (String -> String -> Array CliArgument -> Effect Unit)
  -> Effect Unit
callCliApp cliSpec executor = do
  let
    lengthLongestCmd :: Int
    lengthLongestCmd =
      cliSpec.commands
        # foldl (\acc cmd ->
            if acc > Str.length cmd.name
            then acc
            else Str.length cmd.name
          ) 0

    usageString =
      "USAGE: " <> cliSpec.name <> " <command> [options]"
      <> "\n\n"
      <> cliSpec.description
      <> "\n\n"
      <> "COMMANDS:"
      <> "\n\n"
      <> (cliSpec.commands
            # foldMap (\cmd ->
                cmd.name
                <> (repeatString " " (lengthLongestCmd - Str.length cmd.name))
                <> "  " <> cmd.description <> "\n"
              )
          )

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
    (parseCliArguments 0 toolArgs)
    executor
