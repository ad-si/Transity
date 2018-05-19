module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((!!))
import Data.Result (Result(..), note)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process (PROCESS, argv, cwd, exit)
import Transity.Data.Ledger (Ledger)
import Transity.Data.Ledger as Ledger
import Transity.Utils (ColorFlag(..))


usageString :: String
usageString = """
Usage: transity <command> <path to ledger.yaml>

Commands:
  balance       Show a simple balance of all accounts
  transactions  Show all transcations and their transfers
  entries       Show all individual deposits and withdrawals
"""


run :: String -> Ledger -> Result String String
run command ledger =
  case command of
    "balance" -> Ok (Ledger.showBalance ColorYes ledger)
    "transactions" -> Ok (Ledger.showPrettyAligned ColorYes ledger)
    "entries" -> Ok (Ledger.showEntries ColorYes ledger)

    other -> Error ("\"" <> other <> "\" is not a valid command")


parseArguments :: Array String -> Result String (Tuple String String)
parseArguments arguments = do
  commandName <- note usageString (arguments !! 2)
  filePathArg <- note
    ("No path to a ledger file was provided\n\n" <> usageString)
    (arguments !! 3)
  pure (Tuple commandName filePathArg)


loadAndExec
  :: String
  -> Tuple String String
  -> forall e. Eff (exception :: EXCEPTION, fs :: FS | e) (Result String String)

loadAndExec currentDir (Tuple command filePathRel) = do
  let resolve = Path.resolve [currentDir]
  let filePathAbs = resolve filePathRel
  ledgerFileContent <- readTextFile UTF8 filePathAbs
  let
    result = do
      ledger <- Ledger.fromYaml ledgerFileContent
      run command ledger
  pure result


main :: forall eff . Eff
  ( exception :: EXCEPTION
  , console :: CONSOLE
  , fs :: FS
  , process :: PROCESS | eff) Unit
main = do
  arguments <- argv
  currentDir <- cwd

  let
    result = (parseArguments arguments) <#> (loadAndExec currentDir)

  execution <- case result of
    Ok output -> output
    Error message -> pure (Error message)

  case execution of
    Ok output -> log output
    Error message -> do
      error message
      exit 1


-- TODO: Use Monad transformers
-- resultT = ExceptT <<< toEither
-- unResultT = unExceptT >>> either Error Ok
-- main = do
--   result <- runResultT $ join $ map resultT $
--     loadAndExec <$> lift cwd <*> (resultT <<< parseArguments =<< lift argv)
--   case result of
--     Ok output -> log output
--     Error message -> error message
