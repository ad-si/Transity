module Main where

import Prelude (Unit, bind, discard, pure, (#), ($), (<#>), (<>))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((!!))
import Data.Result (Result(..), note)
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process (PROCESS, argv, cwd, exit)
import Transity.Data.Ledger (Ledger)
import Transity.Data.Ledger as Ledger
import Transity.Plot as Plot
import Transity.Utils (ColorFlag(..))


usageString :: String
usageString = """
Usage: transity <command> <path/to/journal.yaml>

Command             Description
------------------  ------------------------------------------------------------
balance             Simple balance of all accounts
transactions        All transcations and their transfers
entries             All individual deposits & withdrawals space separated
csv                 Entries comma separated
tsv                 Entries tab separated
entries-by-account  All individual deposits & withdrawals grouped by account
gplot               Code and data for gnuplot impulse diagram
                    to visualize transfers of all accounts
gplot-cumul         Code and data for cumuluative gnuplot step chart
                    to visualize balance of all accounts
"""


-- TODO: Move validation to parsing
utcError :: String
utcError =
  "All transfers or their parent transaction must have a valid utc field"


run :: String -> String -> Ledger -> Result String String
run command filePathRel ledger =
  case command of
    "balance"            -> Ok $ Ledger.showBalance ColorYes ledger
    "transactions"       -> Ok $ Ledger.showPrettyAligned ColorYes ledger
    "entries"            -> note utcError $ Ledger.showEntries  " " ledger
    "csv"                -> note utcError $ Ledger.showEntries  "," ledger
    "tsv"                -> note utcError $ Ledger.showEntries  "\t" ledger
    "entries-by-account" -> note utcError $ Ledger.showEntriesByAccount ledger
    "gplot" ->
      (note utcError $ Ledger.showEntriesByAccount ledger)
      <#> (\entries -> Plot.gplotCode $ Plot.configDefault
        # (Plot.GplotConfig `over` (_
            { data = entries
            , title = filePathRel
            })))
    "gplot-cumul" ->
      (note utcError $ Ledger.showEntriesByAccount ledger)
      <#> (\entries -> Plot.gplotCodeCumul $ Plot.configDefault
        # (Plot.GplotConfig `over` (_
            { data = entries
            , title = filePathRel <> " - Cumulative"
            })))

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
      run command filePathRel ledger
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
