module Main where

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
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
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync as Sync
import Node.Path as Path
import Node.Process (cwd, setExitCode)
import Oclis (ExecutorContext, callCliApp)
import Oclis.Types (CliArgPrim(..), CliArgument(..))
import Prelude
  ( Unit
  , bind
  , discard
  , pure
  , show
  , unit
  , (#)
  , ($)
  , (/=)
  , (<#>)
  , (<>)
  , (>)
  , (>>=)
  )
import Transity.Data.Config (ColorFlag(..), config)
import Transity.Data.Ledger
  ( BalanceFilter(..)
  , Ledger(..)
  , verifyAccounts
  , verifyLedgerBalances
  )
import Transity.Data.Ledger as Ledger
import Transity.Data.Transaction (Transaction(..))
import Transity.Plot as Plot
import Transity.Utils (SortOrder(..), errorAndExit)
import Transity.Xlsx (entriesAsXlsx, writeToZip)

-- TODO: Move validation to parsing
utcError :: String
utcError =
  "All transfers or their parent transaction must have a valid UTC field"

runSimpleCmd :: String -> String -> Ledger -> Result String String
runSimpleCmd command filePathRel ledger =
  case command of
    "balance" -> Ok $
      Ledger.showBalance BalanceOnlyOwner ColorYes ledger
    "balance-all" -> Ok $ Ledger.showBalance BalanceAll ColorYes ledger
    -- "balance-on"         -> Ok $
    --   Ledger.showBalanceOn dateMaybe ColorYes ledger
    "transactions" -> Ok $ Ledger.showPrettyAligned ColorYes ledger
    "transfers" -> Ok $ Ledger.showTransfers ColorYes ledger
    "entries" -> note utcError $ Ledger.showEntries " " ledger
    "entities" -> Ok $ Ledger.showEntities CustomSort ledger
    "entities-sorted" -> Ok $ Ledger.showEntities Alphabetically ledger
    "ledger-entries" -> Ok $ Ledger.entriesToLedger ledger
    "csv" -> note utcError $ Ledger.showEntries "," ledger
    "tsv" -> note utcError $ Ledger.showEntries "\t" ledger
    "entries-by-account" -> note utcError $ Ledger.showEntriesByAccount ledger
    "gplot" ->
      (note utcError $ Ledger.showEntriesByAccount ledger)
        <#>
          ( \entries -> Plot.gplotCode $ Plot.configDefault
              #
                ( Plot.GplotConfig `over`
                    ( _
                        { data = entries
                        , title = filePathRel
                        }
                    )
                )
          )
    "gplot-cumul" ->
      (note utcError $ Ledger.showEntriesByAccount ledger)
        <#>
          ( \entries -> Plot.gplotCodeCumul $ Plot.configDefault
              #
                ( Plot.GplotConfig `over`
                    ( _
                        { data = entries
                        , title = filePathRel <> " - Cumulative"
                        }
                    )
                )
          )

    other -> Error ("\"" <> other <> "\" is not a valid command")

-- | Asynchronously logs all non existent referenced files
checkFilePaths :: String -> Ledger -> Effect (Result String String)
checkFilePaths ledgerFilePath (Ledger { transactions }) = do
  let
    files = foldMap (\(Transaction tact) -> tact.files) transactions

  for_ files \filePathRel -> do
    filePathAbs <- Path.resolve [ ledgerFilePath ] filePathRel
    stat filePathAbs $ \statsResult ->
      if isOk $ fromEither statsResult
      then pure unit
      else
        log $ withGraphics
          (foreground Yellow)
          ("Warning: \"" <> filePathAbs <> "\" does not exist")

  pure $ Ok ""

execForLedger ::
  String ->
  String ->
  String ->
  Ledger ->
  Effect (Result String String)
execForLedger currentDir filePathRel command ledger = do
  filePathAbs <- Path.resolve [ currentDir ] filePathRel
  let
    journalDir =
      if indexOf (Pattern "/dev/fd/") filePathAbs == Just 0
      then currentDir
      else Path.dirname filePathAbs
  _ <- checkFilePaths journalDir ledger

  case command of
    "xlsx" -> do
      launchAff_ $ writeToZip
        Nothing -- Means stdout
        (entriesAsXlsx ledger)
      pure (Ok "")
    _ ->
      pure $ runSimpleCmd command filePathRel ledger

loadAndExec ::
  String ->
  Array String ->
  Effect (Result String String)
loadAndExec currentDir [ command, filePathRel ] = do
  filePathAbs <- Path.resolve [ currentDir ] filePathRel
  ledgerFileContent <- Sync.readTextFile UTF8 filePathAbs

  case (Ledger.fromYaml ledgerFileContent) of
    Error msg -> pure $ Error msg
    Ok ledger -> execForLedger currentDir filePathRel command ledger

loadAndExec _ _ =
  pure $ Error "loadAndExec expects an array with length 2"

getJournalPaths :: String -> Array CliArgPrim -> Result String (Array String)
getJournalPaths journalPathRel extraJournalPaths = sequence $
  [ Ok journalPathRel ] <>
    ( extraJournalPaths
        <#>
          ( \valArg -> case valArg of
              (TextArg path) -> Ok path
              _ -> Error $ "Invalid argument type: " <> show valArg
          )
    )

combineJournals :: String -> Array String -> Effect (Result String Ledger)
combineJournals currentDir paths = do
  paths
    <#>
      ( \filePathRel -> do
          filePathAbs <- Path.resolve [ currentDir ] filePathRel
          ledgerFileContent <- Sync.readTextFile UTF8 filePathAbs

          pure $ Ledger.fromYaml ledgerFileContent
      )
    # sequence
    <#> sequence
    <#> (\ledgerRes -> ledgerRes <#> fold)

buildLedgerAndRun ::
  String ->
  String ->
  Array CliArgPrim ->
  (Ledger -> Effect (Result String Unit)) ->
  Effect (Result String Unit)
buildLedgerAndRun currentDir journalPathRel extraJournalPaths callback = do
  let journalPaths = getJournalPaths journalPathRel extraJournalPaths

  case journalPaths of
    Error message -> errorAndExit config message
    Ok paths -> do
      combineRes <- combineJournals currentDir paths
      case
        combineRes
          >>= verifyAccounts
          >>= verifyLedgerBalances
        of
        Error msg -> pure $ Error msg
        Ok ledger -> ledger # callback

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
      then pure $ files
      else do
        filesNested <- sequence $ dirTuples
          <#> (\(Tuple dir _) -> addFiles dir)
        pure (concat $ cons files filesNested)
  in
    addFiles directoryPath

checkUnusedFiles ::
  String -> String -> Array CliArgPrim -> Effect (Result String Unit)
checkUnusedFiles filesDirPath jourPathRel extraJournalPaths = do
  currentDir <- cwd
  buildLedgerAndRun currentDir jourPathRel extraJournalPaths $
    \ledger@(Ledger { transactions }) -> do
      let
        journalDir =
          if indexOf (Pattern "/dev/fd/") jourPathRel == Just 0
          then currentDir
          else Path.dirname jourPathRel

      _ <- checkFilePaths journalDir ledger

      filesDir <- Path.resolve [] filesDirPath
      foundFiles <- getAllFiles filesDir
      let
        ledgerFilesRel = foldMap
          (\(Transaction tact) -> tact.files)
          transactions
      ledgerFilesAbs <- sequence $ ledgerFilesRel
        <#> (\fileRel -> Path.resolve [ journalDir ] fileRel)

      let
        unusedFiles = difference foundFiles ledgerFilesAbs
        makeGreen = withGraphics (foreground Green)
        makeYellow = withGraphics (foreground Yellow)

      if null unusedFiles
      then log $ makeGreen $ "No unused files found in " <> filesDir
      else do
        warn $ makeYellow $ "Warning: "
          <> "Following files are not referenced in the journal"

        for_ unusedFiles $ \filePathAbs ->
          warn $ makeYellow $ "- " <> filePathAbs

      pure $ Ok unit

executor :: ExecutorContext -> Effect (Result String Unit)
executor context = do
  case context.command, context.arguments of
    Just "unused-files",
    [ ValArg (TextArg filesDirPath)
    , ValArg (TextArg jourPathRel)
    , ValArgList extraJournalPaths
    ] -> checkUnusedFiles filesDirPath jourPathRel extraJournalPaths

    Just "unused-files",
    [ ValArg (TextArg filesDirPath)
    , ValArg (TextArg jourPathRel)
    ] -> checkUnusedFiles filesDirPath jourPathRel []

    Just cmdName,
    [ ValArg (TextArg journalPathRel) ] -> do
      currentDir <- cwd
      result <- loadAndExec currentDir [ cmdName, journalPathRel ]

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

    Just cmdName,
    [ ValArg (TextArg jourPathRel)
    , ValArgList extraJournalPaths
    ] -> do
      currentDir <- cwd
      buildLedgerAndRun currentDir jourPathRel extraJournalPaths $
        \ledger -> do
          result <- execForLedger
            currentDir
            jourPathRel -- TODO: Must incorporate all paths
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
      log context.usageString
      setExitCode 1
      pure $ Ok unit

main :: Effect Unit
main = do
  callCliApp executor
