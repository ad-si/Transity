module Main where

-- import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
-- import Control.Monad.Except.Trans (ExceptT)
-- import Control.Plus (empty)
import Data.Array ((!!))
import Data.Either (Either(..))
-- import Data.Foreign (ForeignError)
-- import Data.Foreign.Class (class Encode, class Decode, encode, decode)
-- import Data.Foreign.Generic (defaultOptions, genericDecodeJSON)
-- import Data.Int (floor)
-- import Data.Identity (Identity)
-- import Data.JSDate (JSDate, LOCALE, parse, isValid)
-- import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
-- import Debug.Trace (spy)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process (PROCESS, argv, cwd)
import Prelude (Unit, bind, show, ($))
import Transity.Data.Ledger
  ( yamlStringToLedger
  , prettyShowLedger
  , showBalance
  )


printTransactions :: forall eff.
  String -> Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    | eff
    ) Unit
printTransactions ledgerFileContent = do
  case yamlStringToLedger ledgerFileContent of
    Left error -> log $ show error
    Right ledger -> log $ prettyShowLedger ledger


printBalance :: forall eff.
  String -> Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    | eff
    ) Unit
printBalance ledgerFileContent = do
  case yamlStringToLedger ledgerFileContent of
    Left error -> log $ show error
    Right ledger -> log $ showBalance ledger


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
