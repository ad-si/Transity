module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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
    Left error -> log $ show error
    Right ledger -> log $ Ledger.showPretty ledger


printBalance :: forall eff.
  String -> Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    | eff
    ) Unit
printBalance ledgerFileContent = do
  case Ledger.fromYaml ledgerFileContent of
    Left error -> log $ show error
    Right ledger -> log $ Ledger.showBalance ledger


main :: forall eff . Eff
  ( exception :: EXCEPTION
  , console :: CONSOLE
  , fs :: FS
  , process :: PROCESS | eff) Unit
main = do
  arguments <- argv
  currentDir <- cwd
  let commandName = arguments !! 2
  case commandName of
    Nothing -> log "No command was provided"
    Just command -> do
      let filePathArg = arguments !! 3
      case filePathArg of
        Nothing -> log "No path to a journal file was provided"
        Just filePath -> do
          let filePathAbs = Path.resolve [currentDir] filePath
          ledgerFileContent <- readTextFile UTF8 filePathAbs
          case command of
            "transactions" -> printTransactions ledgerFileContent
            "balance" -> printBalance ledgerFileContent
            _ -> printBalance ledgerFileContent
