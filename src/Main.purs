module Main where

import Prelude (Unit, bind, discard, pure, unit, (#), ($), (<#>), (<>))

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Array ((!!))
import Data.Eq ((==))
import Data.Foldable (foldMap)
import Data.Result (Result(..), note)
import Data.String (Pattern(..), indexOf)
import Data.Traversable (for_)
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Effect
import Effect.Class.Console (log, error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.FS.Async (exists)
import Node.Path as Path
import Node.Process (argv, cwd, exit)

import Transity.Data.Ledger (Ledger(..))
import Transity.Data.Ledger as Ledger
import Transity.Data.Transaction (Transaction(..))
import Transity.Plot as Plot
import Transity.Utils (ColorFlag(..))


type Config = { colorState :: ColorFlag }

config :: Config
config =
  { colorState: ColorYes
  }


usageString :: String
usageString = """
Usage: transity <command> <path/to/journal.yaml>

Command             Description
------------------  ------------------------------------------------------------
balance             Simple balance of all accounts
transactions        All transactions and their transfers
transfers           All transfers with one transfer per line
entries             All individual deposits & withdrawals, space separated
ledger-entries      All entries in Ledger format
csv                 Entries, comma separated
tsv                 Entries, tab separated
entries-by-account  All individual deposits & withdrawals, grouped by account
gplot               Code and data for gnuplot impulse diagram
                    to visualize transfers of all accounts
gplot-cumul         Code and data for cumuluative gnuplot step chart
                    to visualize balance of all accounts
"""


-- TODO: Move validation to parsing
utcError :: String
utcError =
  "All transfers or their parent transaction must have a valid UTC field"



run :: String -> String -> Ledger -> Result String String
run command filePathRel ledger =
  case command of
    "balance"            -> Ok $ Ledger.showBalance ColorYes ledger
    -- "balance-on"         -> Ok $ Ledger.showBalanceOn dateMaybe ColorYes ledger
    "transactions"       -> Ok $ Ledger.showPrettyAligned ColorYes ledger
    "transfers"          -> Ok $ Ledger.showTransfers ColorYes ledger
    "entries"            -> note utcError $ Ledger.showEntries  " " ledger
    "ledger-entries"     -> Ok $ Ledger.entriesToLedger ledger
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


-- | Asynchronously logs all non existent referenced files
checkFilePaths :: String -> Ledger -> Effect (Result String String)
checkFilePaths ledgerFilePath wholeLedger@(Ledger {transactions}) = do
  let
    files = foldMap (\(Transaction tact) -> tact.files) transactions

  for_ files \filePathRel -> do
    filePathAbs <- Path.resolve [ledgerFilePath] filePathRel
    exists filePathAbs $ \doesExist ->
      if doesExist
      then pure unit
      else
        log $ withGraphics
          (foreground Yellow)
          ("Warning: \"" <> filePathAbs <> "\" does not exist")

  pure $ Ok ""


loadAndExec
  :: String
  -> Tuple String String
  -> Effect (Result String String)
loadAndExec currentDir (Tuple command filePathRel) = do
  let resolve = Path.resolve [currentDir]
  filePathAbs <- resolve filePathRel
  ledgerFileContent <- readTextFile UTF8 filePathAbs

  case (Ledger.fromYaml ledgerFileContent) of
    Error msg -> pure $ Error msg
    Ok ledger -> do
      let
        journalDir =
          if indexOf (Pattern "/dev/fd/") filePathAbs == Just 0
          then currentDir
          else Path.dirname filePathAbs
      _ <- checkFilePaths journalDir ledger
      pure $ run command filePathRel ledger


main :: Effect Unit
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
      error (if config.colorState == ColorYes
        then withGraphics (foreground Red) message
        else message)
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
