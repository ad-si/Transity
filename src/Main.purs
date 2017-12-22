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
import Transity.Data.Ledger (yamlStringToLedger, prettyShowLedger)


printTransactions :: forall eff.
  String -> Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    , fs :: FS | eff
    ) Unit
printTransactions filePathAbs = do
  ledgerFile <- readTextFile UTF8 filePathAbs
  case yamlStringToLedger ledgerFile of
    Left error -> log $ show error
    Right ledger -> log $ prettyShowLedger ledger


main :: forall eff . Eff
  ( exception :: EXCEPTION
  , console :: CONSOLE
  , fs :: FS
  , process :: PROCESS | eff) Unit
main = do
  arguments <- argv
  currentDir <- cwd
  let filePathArg = arguments !! 2
  case filePathArg of
    Nothing -> log "No path to a journal file was provided"
    Just filePath -> do
      let filePathAbs = Path.resolve [currentDir] filePath
      printTransactions filePathAbs
