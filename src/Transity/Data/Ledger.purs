module Transity.Data.Ledger where

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Core (Json, stringify, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat, groupBy, length, sort, sortBy, uncons, (!!))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Foldable (all, find, foldMap)
import Data.Function (flip)
import Data.Generic.Rep (class Generic)
import Data.HeytingAlgebra (not)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype, unwrap)
import Data.Rational as Rational
import Data.Result (Result(..), fromEither, toEither)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String
  ( Pattern(..)
  , Replacement(..)
  , joinWith
  , replace
  , replaceAll
  , split
  )
import Data.String.Common (toLower)
import Data.String.Utils (startsWith)
import Data.Traversable (fold, foldr, intercalate, sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Foreign (renderForeignError)
import Prelude
  ( class Eq
  , class Monoid
  , class Semigroup
  , class Show
  , bind
  , compare
  , identity
  , map
  , pure
  , show
  , (#)
  , ($)
  , (&&)
  , (+)
  , (/=)
  , (<#>)
  , (<>)
  , (==)
  , (||)
  )
import Transity.Data.Account (Account(..))
import Transity.Data.Account as Account
import Transity.Data.Amount (Amount(..), Commodity, isZero)
import Transity.Data.Amount as Amount
import Transity.Data.CommodityMap
  ( CommodityMap
  , addAmountToMap
  , isCommodityMapZero
  , isCommodityZero
  , subtractAmountFromMap
  )
import Transity.Data.Config (ColorFlag(..))
import Transity.Data.Entity (Entity(..), toTransfers)
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transaction as Transaction
import Transity.Data.Transfer (Transfer(..), negateTransfer)
import Transity.Utils
  ( SortOrder(..)
  , dateShowPretty
  , dateShowPrettyLong
  , getFieldMaybe
  , getObjField
  , mergeWidthRecords
  , resultWithJsonDecodeError
  , stringifyJsonDecodeError
  , utcToIsoDateString
  , utcToIsoString
  , widthRecordZero
  )

-- | List of all transactions
newtype Ledger = Ledger
  { owner :: Maybe String
  , entities :: Maybe (Array Entity)
  , transactions :: Array Transaction
  }

derive instance genericLedger :: Generic Ledger _
derive newtype instance eqLedger :: Eq Ledger
derive instance newtypeLedger :: Newtype Ledger _

instance showLedger :: Show Ledger where
  show = genericShow

instance decodeLedger :: DecodeJson Ledger where
  decodeJson json = toEither
    $ resultWithJsonDecodeError
    $ decodeJsonLedger json

instance monoidLedger :: Monoid Ledger where
  mempty = Ledger
    { owner: Nothing
    , entities: Nothing
    , transactions: []
    }

instance semigroupLedger :: Semigroup Ledger where
  append (Ledger l1) (Ledger l2) =
    Ledger
      { owner: l1.owner
      , entities: l1.entities <> l2.entities
      , transactions: l1.transactions <> l2.transactions
      }

data BalanceFilter
  = BalanceOnly String
  | BalanceOnlyOwner
  | BalanceAll

decodeJsonLedger :: Json -> Result String Ledger
decodeJsonLedger json = do
  object <- maybe (Error "Ledger is not an object") Ok (toObject json)
  owner <- object `getFieldMaybe` "owner"
  entities <- object `getFieldMaybe` "entities"
  transactions <- object `getObjField` "transactions"
  pure $ Ledger { owner, entities, transactions }

verifyAccounts :: Ledger -> Result String Ledger
verifyAccounts wholeLedger@(Ledger ledger) =
  let
    definedAccounts = Set.fromFoldable $ concat
      $ (fromMaybe [] ledger.entities) <#>
          ( \(Entity { id, accounts }) -> [ id ] <>
              ( (fromMaybe [] accounts) <#>
                  (\(Account account) -> id <> ":" <> account.id)
              )
          )
    usedAccounts =
      (ledger.transactions <#> \(Transaction { transfers }) -> transfers)
        # concat
        # map (\(Transfer { from, to }) -> [ from, to ])
        # concat
        # Set.fromFoldable

    undefinedAccounts :: Array String
    undefinedAccounts = Set.toUnfoldable $
      usedAccounts `Set.difference` definedAccounts
  in
    case undefinedAccounts of
      [] -> Ok wholeLedger
      _ -> Error $
        "Following accounts were not declared, "
          <> "but still used for transfers:\n\n"
          <> "entities:"
          <> joinWith "" (undefinedAccounts <#> ("\n  - id: " <> _))
          <> "\n\n"
          <> "Please add or rename the missing accounts "
          <> "to the entities section to fix this error"

isAmountInMapZero :: BalanceMap -> String -> Commodity -> Boolean
isAmountInMapZero balanceMap accountId commodity =
  let
    comMap = Map.lookup accountId balanceMap
  in
    fromMaybe false $
      comMap <#> (flip isCommodityZero) commodity

verifyBalances :: BalanceMap -> Array Transfer -> Result String Unit
verifyBalances balanceMap balancingTransfers =
  case uncons balancingTransfers of
    Just { head: transfHead@(Transfer tfHeadRec), tail: transfTail } ->
      let
        newBal = balanceMap `addTransfer` transfHead
        getCommodity { amount: Amount _ commodity } = commodity
        targetCom = getCommodity tfHeadRec
      in
        if tfHeadRec.note == Just "___BALANCE___" then
          if not $ isAmountInMapZero newBal tfHeadRec.from targetCom then Error
            ( "ERROR:\nThe verification balance of account '" <> tfHeadRec.from
                <> "' on '"
                <> (fromMaybe "" $ tfHeadRec.utc <#> dateShowPretty)
                <> "'\nis off by "
                <>
                  ( Map.lookup tfHeadRec.from newBal
                      # fromMaybe Map.empty
                      # Map.values
                      # find (\(Amount _ commodity) -> commodity == targetCom)
                      <#> (Amount.negate >>> Amount.showPretty)
                      # fromMaybe "ERROR: Amount is missing"
                  )
                <> " from the calculated balance."
            )
          else verifyBalances balanceMap transfTail
        else
          verifyBalances newBal transfTail
    Nothing ->
      Ok unit

entitiesToTransfers :: Maybe (Array Entity) -> Array Transfer
entitiesToTransfers entities =
  (fromMaybe [] entities)
    <#> toTransfers
    # fold
    -- Label the balancing transfers to tell them apart from normal transfers
    -- FIXME: Really hacky and should be solved with a wrapper datatype
    <#> (\(Transfer tf) -> Transfer tf { note = Just "___BALANCE___" })

verifyLedgerBalances :: Ledger -> Result String Ledger
verifyLedgerBalances wholeLedger@(Ledger { transactions, entities }) =
  let
    balancingTransfers = entitiesToTransfers entities
    transxTransfers = Transaction.toTransfers transactions
    combined = (balancingTransfers <> transxTransfers)
      # sortBy
          ( \(Transfer transfA) (Transfer transfB) ->
              compare transfA.utc transfB.utc
          )
    result = verifyBalances Map.empty combined
  in
    if entities == Nothing || entities == Just [] then Ok wholeLedger
    else
      case result of
        Ok _ -> Ok wholeLedger
        Error error -> Error error

fromJson :: String -> Result String Ledger
fromJson json = do
  jsonObj <- fromEither $ jsonParser json
  ledger <- stringifyJsonDecodeError $ fromEither $ decodeJson jsonObj
  pure ledger

fromYaml :: String -> Result String Ledger
fromYaml yaml = do
  let
    result = yaml
      # parseYAMLToJson
      # runExcept
      # fromEither

  case result of
    Error error ->
      Error $ "Could not parse YAML: " <> foldMap renderForeignError error
    Ok json ->
      stringifyJsonDecodeError $ fromEither $ decodeJson json

showPretty :: Ledger -> String
showPretty = showPrettyAligned ColorNo

showPrettyAligned :: ColorFlag -> Ledger -> String
showPrettyAligned colorFlag (Ledger l) =
  let
    transactionsPretty = map
      (Transaction.showPrettyAligned colorFlag)
      l.transactions
  in
    ""
      <> "Journal for \""
      <> (l.owner # fromMaybe "UNKNOWN")
      <> "\"\n"
      <> "=" `power` 80
      <> "\n"
      <> fold transactionsPretty

showTransfers :: ColorFlag -> Ledger -> String
showTransfers colorFlag (Ledger l) =
  let
    transactionsPretty = l.transactions
      <#> Transaction.showTransfersWithDate colorFlag
      # fold
  in
    ""
      <> "Journal for \""
      <> (l.owner # fromMaybe "UNKNOWN")
      <> "\"\n"
      <> "=" `power` 80
      <> "\n"
      <> transactionsPretty

type BalanceMap = Map.Map Account.Id CommodityMap

isBalanceMapZero :: BalanceMap -> Boolean
isBalanceMapZero balanceMap =
  (Map.values balanceMap)
    # all isCommodityMapZero

addTransaction :: BalanceMap -> Transaction -> BalanceMap
addTransaction balanceMap (Transaction { transfers }) =
  foldr (flip addTransfer) balanceMap transfers

addTransfer :: BalanceMap -> Transfer -> BalanceMap
addTransfer balanceMap (Transfer { to, from, amount }) =
  let
    receiverArray = split (Pattern ":") to
    receiverWithDefault = case length receiverArray of
      1 -> to <> ":_default_"
      _ -> to
    senderArray = split (Pattern ":") from
    senderWithDefault = case length senderArray of
      1 -> from <> ":_default_"
      _ -> from
    updatedFromAccount = Map.alter
      ( \maybeValue -> case maybeValue of
          Nothing ->
            Just ((Map.empty :: CommodityMap) `subtractAmountFromMap` amount)
          Just commodityMap ->
            Just (commodityMap `subtractAmountFromMap` amount)
      )
      senderWithDefault
      balanceMap
  in
    Map.alter
      ( \maybeValue -> case maybeValue of
          Nothing ->
            Just ((Map.empty :: CommodityMap) `addAmountToMap` amount)
          Just commodityMap ->
            Just (commodityMap `addAmountToMap` amount)
      )
      receiverWithDefault
      updatedFromAccount

subtractTransfer :: BalanceMap -> Transfer -> BalanceMap
subtractTransfer balanceMap transfer =
  let
    transferNegated = negateTransfer transfer
  in
    balanceMap `addTransfer` transferNegated

showEntities :: SortOrder -> Ledger -> String
showEntities sortOrder (Ledger ledger) =
  let
    showId :: String -> String
    showId id =
      "  - id: " <> id <> "\n"

    showName :: String -> String
    showName name =
      "    name: " <> name <> "\n"

    showNote :: String -> String
    showNote note =
      "    note: " <> note <> "\n"

    showUTC :: DateTime -> String
    showUTC utc =
      "    utc: " <> (utc # dateShowPrettyLong) <> "\n"

    showTags :: Array String -> String
    showTags tags = "    tags: "
      <>
        (
          -- TODO: Can tags contain quote characters?
          tags # show # replaceAll (Pattern "\"") (Replacement "")
        )
      <> "\n"

    showAccount :: Account -> String
    showAccount (Account acc) =
      "\n      - " <> stringify (encodeJson acc)

    showAccounts :: Array Account -> String
    showAccounts accounts =
      "    accounts: " <> (accounts <#> showAccount # fold) <> "\n"

    showEntity :: Entity -> String
    showEntity (Entity entity) =
      (entity.id # showId)
        <> (entity.name <#> showName # fromMaybe "")
        <> (entity.note <#> showNote # fromMaybe "")
        <> (entity.utc <#> showUTC # fromMaybe "")
        <> (entity.tags <#> showTags # fromMaybe "")
        <> (entity.accounts <#> showAccounts # fromMaybe "")
        <> "\n"
  in
    case ledger.entities of
      Nothing ->
        "Journal does not contain any entities"

      Just entities ->
        "entities:\n" <>
          ( entities
              #
                ( case sortOrder of
                    CustomSort -> identity
                    Alphabetically ->
                      sortBy
                        ( \(Entity entityA) (Entity entityB) ->
                            compare (toLower entityA.id) (toLower entityB.id)
                        )
                )
              <#> showEntity
              # fold
          )

showBalance :: BalanceFilter -> ColorFlag -> Ledger -> String
showBalance balFilter colorFlag (Ledger ledger) =
  let
    balanceMap :: Map.Map String (Map.Map Commodity Amount)
    balanceMap = foldr (flip addTransaction) Map.empty ledger.transactions

    mapToEmpty ::
      Tuple String (Map.Map Commodity Amount) ->
      String ->
      Tuple String (Map.Map Commodity Amount)
    mapToEmpty accTuple@(Tuple accId _) account =
      if accId == account || (accId # startsWith (account <> ":")) then accTuple
      else (Tuple "" Map.empty)

    balancesArray :: Array (Tuple String (Map.Map Commodity Amount))
    balancesArray = balanceMap
      #
        ( Map.toUnfoldable ::
            BalanceMap ->
            Array (Tuple Account.Id CommodityMap)
        )
      <#>
        ( \accTuple -> case balFilter of
            BalanceAll -> accTuple
            BalanceOnlyOwner ->
              case ledger.owner of
                Just owner -> mapToEmpty accTuple owner
                _ -> accTuple
            BalanceOnly account -> mapToEmpty accTuple account
        )
      -- Don't show commodities with an amount of 0
      <#>
        ( \(Tuple accId comMap) ->
            Tuple accId
              ( comMap # Map.mapMaybe
                  ( \amount ->
                      if isZero amount then Nothing
                      else Just amount
                  )
              )
        )
      # Array.filter (\(Tuple _ comMap) -> comMap /= Map.empty)

    normAccId accId =
      replace (Pattern ":_default_") (Replacement "") accId

    accWidthRecs ::
      Array
        { account :: Int
        , commodity :: Int
        , fraction :: Int
        , integer :: Int
        }
    accWidthRecs = balancesArray
      <#>
        ( \(Tuple accId comMap) ->
            Account.toWidthRecord (normAccId accId) comMap
        )

    widthRecord ::
      { account :: Int
      , commodity :: Int
      , fraction :: Int
      , integer :: Int
      }
    widthRecord = foldr mergeWidthRecords widthRecordZero accWidthRecs

    marginLeft = 2

    showTuple (Tuple accId comMap) =
      ( Account.showPrettyAligned
          colorFlag
          widthRecord { account = widthRecord.account + marginLeft }
          (normAccId accId)
          comMap
      )
  in
    balancesArray
      <#> showTuple
      # fold

-- | Serializes the journal to a command line printable version
-- | (lines of columns).
getEntries :: Ledger -> Maybe (Array (Array String))
getEntries (Ledger { transactions, entities }) = do
  let
    getQunty (Amount quantity _) = show $ Rational.toNumber quantity
    getCmdty (Amount _ commodity) = unwrap commodity

    splitTransfer :: Transfer -> Maybe (Array (Array String))
    splitTransfer (Transfer tfer) =
      let
        fromAmnt = Amount.negate tfer.amount
        getFromAndTo date =
          [ [ date, tfer.from, getQunty fromAmnt, getCmdty fromAmnt ]
          , [ date, tfer.to, getQunty tfer.amount, getCmdty tfer.amount ]
          ]
      in
        (tfer.utc <#> utcToIsoString) <#> getFromAndTo

  splitted <-
    do
      transactions
      <#>
        ( \(Transaction tact) -> tact.transfers
            <#>
              ( \(Transfer tfer) -> Transfer
                  (tfer { utc = tfer.utc <|> tact.utc })
              )
        )
      # concat
      <#> splitTransfer
      # sequence

  let
    initialEntries = entitiesToInitialTransfers entities <#>
      \(Transfer t) ->
        let
          isoString = fromMaybe "INVALID DATE" $ t.utc <#> utcToIsoString
        in
          [ [ isoString
            , replace (Pattern ":_default_") (Replacement "") t.from
            , getQunty t.amount
            , getCmdty t.amount
            ]
          ]

  pure $ (splitted <> initialEntries) # concat

entitiesToInitialTransfers :: Maybe (Array Entity) -> Array Transfer
entitiesToInitialTransfers entities =
  (fromMaybe [] entities)
    <#> toTransfers
    # fold
    # Array.filter
        ( \(Transfer tf) ->
            Amount.isZero tf.amount && tf.from /= "_void_"
        )

maybeToArr :: forall a. Maybe a -> Array a
maybeToArr m = case m of
  Just val -> [ val ]
  Nothing -> []

-- | Serialize the journal to the Ledger format.
entriesToLedger :: Ledger -> String
entriesToLedger (Ledger { transactions }) =
  let
    print ::
      DateTime ->
      Maybe String ->
      Account.Id ->
      Account.Id ->
      Amount ->
      String
    print dt maybeNote from to amount =
      let
        date = dt # utcToIsoDateString
        note = maybe "" identity maybeNote
      in
        date <> " " <> note <> "\n"
          <> "  "
          <> to
          <> "  "
          <> (Amount.showPretty amount)
          <> "\n"
          <> "  "
          <> from
          <> "\n"

    result = do
      Transaction { utc, note, transfers } <- transactions
      xutc <- maybeToArr utc
      Transfer { to, from, amount } <- transfers
      pure $ print xutc note from to amount

  in
    result # joinWith "\n"

showEntries :: String -> Ledger -> Maybe String
showEntries separator ledger = do
  entries <- getEntries ledger

  pure $ entries
    # sort
    <#> joinWith separator
    # joinWith "\n"

showEntriesByAccount :: Ledger -> Maybe String
showEntriesByAccount ledger = do
  let
    accountCommodity array =
      fromMaybe "" (array !! 1) <> " " <> fromMaybe "" (array !! 3)
    compareAccComm a b = accountCommodity a `compare` accountCommodity b
    isEqualAccComm a b = accountCommodity a == accountCommodity b
    accCommOfGroup array = "\""
      <> (accountCommodity $ fromMaybe [] (array !! 0))
      <> "\""

  entries <- getEntries ledger

  pure $ entries
    # sortBy compareAccComm
    # groupBy isEqualAccComm
    <#> Array.fromFoldable -- Convert each NonEmpty to Array
    <#> sort -- Sort each entry by date
    <#> (\array -> [ [ accCommOfGroup array ] ] <> array)
    # intercalate [ [ "\n" ] ] -- Add space between account entries
    <#> joinWith " " -- Join fields for each row
    # joinWith "\n"
