module Main where

import Prelude
  ( Unit, bind, discard, pure, show, unit
  , (#), ($), (/=), (<#>), (<>), (>)
  )

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import CliSpec (parseCliSpec, callCliApp)
import CliSpec.JsonEmbed as CliSpec.JsonEmbed
import CliSpec.Types (CliArgPrim(..), CliArgument(..))
import Data.Array (concat, cons, difference, filter, fold, null, zip)
import Data.Eq ((==))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Result (Result(..), fromEither, isOk, note)
import Data.String (Pattern(..), indexOf, length)
import Data.Traversable (for_, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Console (warn)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async (stat)
import Node.FS.Stats (isFile, isDirectory)
import Node.FS.Sync as Sync
import Node.Path as Path
import Node.Process (cwd, setExitCode)
import Transity.Data.Config (ColorFlag(..), config)
import Transity.Data.Ledger (Ledger(..), BalanceFilter(..))
import Transity.Data.Ledger as Ledger
import Transity.Data.Transaction (Transaction(..))
import Transity.Plot as Plot
import Transity.Utils (SortOrder(..), makeRed, errorAndExit)
import Transity.Xlsx (writeToZip, entriesAsXlsx)


-- TODO: Move validation to parsing
utcError :: String
utcError =
  "All transfers or their parent transaction must have a valid UTC field"


runSimpleCmd :: String -> String -> Ledger -> Result String String
runSimpleCmd command filePathRel ledger =
  case command of
    "balance"            -> Ok $
      Ledger.showBalance BalanceOnlyOwner ColorYes ledger
    "balance-all"        -> Ok $ Ledger.showBalance BalanceAll ColorYes ledger
    -- "balance-on"         -> Ok $
    --   Ledger.showBalanceOn dateMaybe ColorYes ledger
    "transactions"       -> Ok $ Ledger.showPrettyAligned ColorYes ledger
    "transfers"          -> Ok $ Ledger.showTransfers ColorYes ledger
    "entries"            -> note utcError $ Ledger.showEntries  " " ledger
    "entities"           -> Ok $ Ledger.showEntities CustomSort ledger
    "entities-sorted"    -> Ok $ Ledger.showEntities Alphabetically ledger
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


-- | Asynchronously logs all non existent referenced files
checkFilePaths :: String -> Ledger -> Effect (Result String String)
checkFilePaths ledgerFilePath (Ledger {transactions}) = do
  let
    files = foldMap (\(Transaction tact) -> tact.files) transactions

  for_ files \filePathRel -> do
    filePathAbs <- Path.resolve [ledgerFilePath] filePathRel
    stat filePathAbs $ \statsResult ->
      if isOk $ fromEither statsResult
      then pure unit
      else
        log $ withGraphics
          (foreground Yellow)
          ("Warning: \"" <> filePathAbs <> "\" does not exist")

  pure $ Ok ""


execForLedger
  :: String
  -> String
  -> String
  -> Ledger
  -> Effect (Result String String)
execForLedger currentDir filePathRel command ledger = do
  filePathAbs <- Path.resolve [currentDir] filePathRel
  let
    journalDir =
      if indexOf (Pattern "/dev/fd/") filePathAbs == Just 0
      then currentDir
      else Path.dirname filePathAbs
  _ <- checkFilePaths journalDir ledger

  case command of
    "xlsx" -> do
      launchAff_ $ writeToZip
        Nothing  -- Means stdout
        (entriesAsXlsx ledger)
      pure (Ok "")
    _ ->
      pure $ runSimpleCmd command filePathRel ledger


loadAndExec
  :: String
  -> Array String
  -> Effect (Result String String)
loadAndExec currentDir [command, filePathRel] = do
  filePathAbs <- Path.resolve [currentDir] filePathRel
  ledgerFileContent <- Sync.readTextFile UTF8 filePathAbs

  case (Ledger.fromYaml ledgerFileContent) of
    Error msg -> pure $ Error msg
    Ok ledger -> execForLedger currentDir filePathRel command ledger

loadAndExec _ _ =
  pure $ Error "loadAndExec expects an array with length 2"


executor :: String -> String -> Array CliArgument -> Effect (Result String Unit)
executor cmdName usageString args = do
  case cmdName, args of
    "unused-files",
    [ ValArg (StringArg filesDirPath)
    , ValArg (StringArg ledgerFilePath)
    -- TODO: , ValArgList extraJournalPaths
    ] -> do
      ledgerFilePathAbs <- Path.resolve [] ledgerFilePath
      ledgerFileContent <- Sync.readTextFile UTF8 ledgerFilePathAbs

      case (Ledger.fromYaml ledgerFileContent) of
        Error msg -> errorAndExit config msg
        Ok ledger@(Ledger {transactions}) -> do
          currentDir <- cwd
          let
            journalDir =
              if indexOf (Pattern "/dev/fd/") ledgerFilePathAbs == Just 0
              then currentDir
              else Path.dirname ledgerFilePathAbs

          _ <- checkFilePaths journalDir ledger

          filesDir <- Path.resolve [] filesDirPath
          foundFiles <- getAllFiles filesDir
          let
            ledgerFilesRel = foldMap
              (\(Transaction tact) -> tact.files)
              transactions
          ledgerFilesAbs <- sequence $ ledgerFilesRel
              <#> (\fileRel -> Path.resolve [journalDir] fileRel)

          let
            unusedFiles = difference foundFiles ledgerFilesAbs
            makeGreen = withGraphics (foreground Green)
            makeYellow = withGraphics (foreground Yellow)

          if null unusedFiles
          then
            log $ makeGreen $ "No unused files found in " <> filesDir
          else do
            warn $ makeYellow $ "Warning: "
              <> "Following files are not referenced in the journal"

            for_ unusedFiles $ \filePathAbs ->
              log $ makeYellow $ "- " <> filePathAbs

          pure $ Ok unit

    "unused-files",
    _ -> do
      let errStr = "ERROR: Wrong number of arguments for unused-files"
      log $ makeRed config errStr <> "\n\n" <> usageString
      setExitCode 1
      pure $ Error errStr

    _,
    [ValArg (StringArg journalPathRel)] -> do
          currentDir <- cwd
          result <- loadAndExec currentDir [cmdName, journalPathRel]

          case result of
            Ok output ->
              if length output > 0
              then do
                log output
                pure $ Ok unit
              else
                pure $ Ok unit
            Error message ->
              errorAndExit config message

    _,
    [ ValArg (StringArg journalPathRel)
    , ValArgList extraJournalPaths
    ] -> do
          currentDir <- cwd

          let
            journalPaths :: Result String (Array String)
            journalPaths = sequence $
                [Ok journalPathRel] <>
                (extraJournalPaths
                    <#> (\valArg -> case valArg of
                          (StringArg path) -> Ok path
                          _ -> Error $ "Invalid argument type: " <> show valArg
                    )
                )

            combineJournals :: Array String -> Effect (Result String Ledger)
            combineJournals paths = do
              paths
                <#> (\filePathRel -> do
                        filePathAbs <- Path.resolve [currentDir] filePathRel
                        ledgerFileContent <- Sync.readTextFile UTF8 filePathAbs

                        pure $ Ledger.fromYaml ledgerFileContent
                    )
                # sequence
                <#> sequence
                <#> (\ledgerRes -> ledgerRes <#> fold )

          case journalPaths of
            Error message -> errorAndExit config message
            Ok paths -> do
              combineRes <- combineJournals paths
              case combineRes of
                Error message -> errorAndExit config message
                Ok ledger -> do
                  result <- execForLedger
                    currentDir
                    journalPathRel -- TODO: Must incorporate all paths
                    cmdName
                    ledger

                  case result of
                    Ok output -> do
                      log output
                      pure $ Ok unit
                    Error message ->
                      errorAndExit config message

    _,
    _ -> do
      log usageString
      setExitCode 1
      pure $ Ok unit


getAllFiles :: String -> Effect (Array String)
getAllFiles directoryPath =
  let
    addFiles :: String -> Effect (Array String)
    addFiles dirPath = do
      entriesRel <- Sync.readdir dirPath
      let
        cleanEntriesRel = filter (_ /= ".DS_Store") entriesRel
        entriesAbs = cleanEntriesRel <#> (\entry -> dirPath <> "/" <> entry)
      statEntries <- sequence $ entriesAbs <#> Sync.stat

      let
        pathStatsTuples = zip entriesAbs statEntries
        fileTuples = filter (\tuple -> isFile $ snd tuple) pathStatsTuples
        dirTuples = filter (\tuple -> isDirectory $ snd tuple) pathStatsTuples
        files = fileTuples <#> fst

      if null dirTuples
      then
        pure $ files
      else do
        filesNested <- sequence $ dirTuples
          <#> (\(Tuple dir _) -> addFiles dir)
        pure (concat $ cons files filesNested)
  in
    addFiles directoryPath


main :: Effect Unit
main = do
  _ <- case parseCliSpec CliSpec.JsonEmbed.fileContent of
    Error msg -> errorAndExit config msg
    Ok cliSpec -> callCliApp cliSpec executor

  pure unit
