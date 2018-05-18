module Transity.Data.Ledger where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat)
import Data.Foldable (fold, foldr)
import Data.Foreign (renderForeignError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (power)
import Data.Result (Result(..), toEither, fromEither)
import Data.String (joinWith)
import Data.Tuple (Tuple, snd)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Transity.Data.Account (Account(..))
import Transity.Data.Account as Account
import Transity.Data.Entity (Entity(..))
import Transity.Data.CommodityMap
  (CommodityMap, addAmountToMap, subtractAmountFromMap)
import Data.Set as Set
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transaction as Transaction
import Transity.Data.Transfer (Transfer(..))
import Transity.Utils
  ( widthRecordZero
  , mergeWidthRecords
  , getObjField
  , getFieldMaybe
  )


-- | List of all transactions
newtype Ledger = Ledger
  { owner :: String
  , entities :: Maybe (Array Entity)
  , transactions :: Array Transaction
  }

derive instance genericLedger :: Generic Ledger _

instance showLedger :: Show Ledger where
  show = genericShow

instance decodeLedger :: DecodeJson Ledger where
  decodeJson json = toEither $ decodeJsonLedger json


decodeJsonLedger :: Json -> Result String Ledger
decodeJsonLedger json = do
  object       <- maybe (Error "Ledger is not an object") Ok (toObject json)
  owner        <- object `getObjField` "owner"
  entities     <- object `getFieldMaybe` "entities"
  transactions <- object `getObjField` "transactions"
  pure $ Ledger {owner, entities, transactions}


verifyAccounts :: Ledger -> Result String Ledger
verifyAccounts wholeLedger@(Ledger ledger) =
  let
    definedAccounts = Set.fromFoldable $ concat
      $ (fromMaybe [] ledger.entities) <#>
        (\(Entity {id, accounts}) -> [id] <>
          ((fromMaybe [] accounts) <#> (\(Account aId _) -> id <> ":" <> aId))
        )
    usedAccounts =
      (ledger.transactions <#> \(Transaction {transfers}) -> transfers)
      # concat
      # map (\(Transfer {from, to}) -> [from, to])
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


fromJson :: String -> Result String Ledger
fromJson json = do
  jsonObj <- fromEither $ jsonParser json
  ledger <- fromEither $ decodeJson jsonObj
  pure ledger >>= verifyAccounts


fromYaml :: String -> Result String Ledger
fromYaml yaml =
  let
    result = yaml
      # parseYAMLToJson
      # runExcept
      # fromEither
    unverified = case result of
      Error error -> Error
        ( "Could not parse YAML: "
          <> fold (map renderForeignError error)
        )
      Ok json -> fromEither $ decodeJson json
  in
    unverified >>= verifyAccounts


showPretty :: Ledger -> String
showPretty = showPrettyAligned false


showPrettyAligned :: Boolean -> Ledger -> String
showPrettyAligned colorize (Ledger l) =
  let
    transactionsPretty = map
      (Transaction.showPrettyAligned colorize)
      l.transactions
  in ""
    <> "Ledger for \"" <> l.owner <> "\"\n"
    <> "=" `power` 80 <> "\n"
    <> fold transactionsPretty


type BalanceMap = Map.Map Account.Id Account


addTransaction :: Transaction -> BalanceMap -> BalanceMap
addTransaction (Transaction {transfers}) balanceMap =
  foldr addTransfer balanceMap transfers


addTransfer :: Transfer -> BalanceMap -> BalanceMap
addTransfer (Transfer {to, from, amount}) balanceMap =
  let
    updatedFromAccount = Map.alter
      (\maybeValue -> case maybeValue of
        Nothing -> Just (
            Account from ((Map.empty :: CommodityMap)
            `subtractAmountFromMap`
            amount)
          )
        Just (Account _ comMapNow) -> Just (
          Account from (comMapNow `subtractAmountFromMap` amount)
        )
      )
      from
      balanceMap
  in
    Map.alter
      (\maybeValue -> case maybeValue of
        Nothing -> Just (
            Account to ((Map.empty :: CommodityMap)
            `addAmountToMap`
             amount)
          )
        Just (Account _ comMapNow) -> Just (
            Account to (comMapNow `addAmountToMap` amount)
          )
      )
      to
      updatedFromAccount


showBalance :: Boolean -> Ledger -> String
showBalance colorize (Ledger ledger) =
  let
    balanceMap = foldr addTransaction Map.empty ledger.transactions
    accountsArray = balanceMap
      # (Map.toAscUnfoldable :: BalanceMap -> Array (Tuple Account.Id Account))
      # map snd
    accWidthRecs = accountsArray
      # map Account.toWidthRecord
    widthRecord = foldr mergeWidthRecords widthRecordZero accWidthRecs
    marginLeft = 2
  in
    accountsArray
      # map (Account.showPrettyAligned
          colorize
          widthRecord
            { account = widthRecord.account + marginLeft }
        )
      # fold
