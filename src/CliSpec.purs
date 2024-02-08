module CliSpec where

import CliSpec.Types

import Prelude (Unit, bind, discard, pure, unit, (#), ($), (-), (<>), (>), (||))

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import CliSpec.Parser (tokensToCliArguments)
import CliSpec.Tokenizer (tokenizeCliArguments)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, find, fold, foldMap, foldl, head, replicate)
import Data.Bifunctor (lmap)
import Data.Eq ((==))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Result (Result(..), fromEither)
import Data.String as Str
import Effect (Effect)
import Effect.Class.Console (log, error)
import Node.Process (argv, setExitCode)

-- TODO: Automatically disable colors if not supported
makeRed :: String -> String
makeRed str =
  withGraphics (foreground Red) str

makeYellow :: String -> String
makeYellow str =
  withGraphics (foreground Yellow) str

errorAndExit :: String -> Effect (Result String Unit)
errorAndExit message = do
  error (makeRed message)
  setExitCode 1
  pure $ Error message

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
  -> (String -> String -> Array CliArgument -> Effect (Result String Unit))
  -> Effect (Result String Unit)
callCommand (CliSpec cliSpec) usageString args executor = do
  case args # head of
    Nothing -> do
      log "No arguments provided"
      setExitCode 1
      pure (Error "No arguments provided")

    Just firstArg
      | firstArg == FlagShort 'h'
          || firstArg == FlagLong "help"
          || firstArg == CmdArg "help" -> do
          log usageString
          pure $ Ok unit

    Just firstArg
      | firstArg == FlagShort 'v'
          || firstArg == FlagLong "version"
          || firstArg == CmdArg "version" -> do
          log usageString
          pure $ Ok unit

    Just _mainCmd ->
      case args # drop 1 # head of
        Just arg
          | arg == (CmdArg "help")
              || arg == (FlagLong "help")
              || arg == (FlagShort 'h') -> do
              -- TODO: Only show help for subcommand
              log usageString
              pure $ Ok unit

        Just arg
          | arg == (CmdArg "version")
              || arg == (FlagLong "version")
              || arg == (FlagShort 'v') -> do
              -- TODO: Only show version of subcommand (if available)
              log (cliSpec.version # fromMaybe "0")
              pure $ Ok unit

        Just (CmdArg cmdName) -> do
          let
            commandMb = cliSpec.commands
              # fromMaybe []
              # find (\(CliSpec cmd) -> cmd.name == cmdName)
            providedArgs = args # drop 2

          case commandMb of
            Nothing -> do
              let
                errStr =
                  makeRed ("ERROR: Unknown command \"" <> cmdName <> "\"")
                    <> "\n\n"
                    <> usageString
              log errStr
              setExitCode 1
              pure (Error errStr)

            Just (CliSpec _command) -> do
              executor cmdName usageString providedArgs

        Just arg -> do
          let
            errMsg =
              "ERROR: First argument must be a command and not \""
                <> cliArgToString arg
                <> "\"\n\n"
          log $ makeRed $ errMsg <> usageString
          setExitCode 1
          pure $ Error errMsg

        Nothing -> do
          log usageString
          setExitCode 1
          pure $ Error "No arguments provided"

-- | Function to repeat a string n times
repeatString :: String -> Int -> String
repeatString str n =
  fold $ replicate n str

callCliApp
  :: CliSpec
  -> (String -> String -> Array CliArgument -> Effect (Result String Unit))
  -> Effect (Result String Unit)
callCliApp cliSpec@(CliSpec cliSpecRaw) executor = do
  let
    lengthLongestCmd :: Int
    lengthLongestCmd =
      cliSpecRaw.commands
        # fromMaybe []
        # foldl
            ( \acc (CliSpec cmd) ->
                if acc > Str.length cmd.name then acc
                else Str.length cmd.name
            )
            0

    usageString =
      "USAGE: " <> cliSpecRaw.name <> " <command> [options]"
        <> "\n\n"
        <> cliSpecRaw.description
        <> "\n\n"
        <> "COMMANDS:"
        <> "\n\n"
        <>
          ( cliSpecRaw.commands
              # fromMaybe []
              # foldMap
                  ( \(CliSpec cmd) ->
                      cmd.name
                        <>
                          ( repeatString " "
                              (lengthLongestCmd - Str.length cmd.name)
                          )
                        <> "  "
                        <> cmd.description
                        <> "\n"
                  )
          )

  arguments <- argv

  let
    argsNoInterpreter = arguments # drop 1 -- Drop "node"
    cliArgsMb =
      tokensToCliArguments
        cliSpec
        (tokenizeCliArguments argsNoInterpreter)

  case cliArgsMb of
    Error err -> errorAndExit err
    Ok cliArgs -> callCommand cliSpec usageString cliArgs executor
