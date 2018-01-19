module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Result (Result(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process (PROCESS, argv, cwd)
import Prelude (Unit, bind, show, ($))
import Transity.Data.Ledger
  ( fromYaml
  , showPretty
  , showBalance
  ) as Ledger


printTransactions :: forall eff.
  String -> Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    | eff
    ) Unit
printTransactions ledgerFileContent = do
  case Ledger.fromYaml ledgerFileContent of
    Ok ledger -> log $ Ledger.showPretty ledger
    Error error -> log $ show error


printBalance :: forall eff.
  String -> Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    | eff
    ) Unit
printBalance ledgerFileContent = do
  case Ledger.fromYaml ledgerFileContent of
    Ok ledger -> log $ Ledger.showBalance ledger
    Error error -> log $ show error


main :: forall eff . Eff
  ( exception :: EXCEPTION
  , console :: CONSOLE
  , fs :: FS
  , process :: PROCESS | eff) Unit
main = do
  arguments <- argv
  currentDir <- cwd

  let commandName = arguments !! 2
  let filePathArg = arguments !! 3
  let usageString = "Usage: transity <command> <path to ledger.yaml>"

  case commandName, filePathArg of
    Nothing, _ -> log usageString
    _, Nothing -> log
      ( "No path to a ledger file was provided\n\n" <> usageString)

    Just command, Just filePath -> do
      let filePathAbs = Path.resolve [currentDir] filePath
      ledgerFileContent <- readTextFile UTF8 filePathAbs

      case command of
        "transactions" -> printTransactions ledgerFileContent
        "balance" -> printBalance ledgerFileContent
        other -> log $ "\"" <> other <> "\" is not a valid command"
