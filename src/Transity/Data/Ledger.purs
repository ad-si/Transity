module Transity.Data.Ledger where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple))
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude (class Show, bind, pure, ($), (<>), (#))
import Text.Format (format, width)
import Transity.Data.Account
  ( Account(Account)
  , AccountId
  , CommodityMap
  , addAmountToMap
  , subtractAmountFromMap
  , prettyShowCommodityMap
  )
import Transity.Data.Transaction
  ( Transaction(Transaction)
  , prettyShowTransaction
  )
import Transity.Utils (getObjField, indentSubsequent)


-- | List of all transactions
newtype Ledger = Ledger
  { owner :: String
  , transactions :: Array Transaction
  }

derive instance genericLedger :: Generic Ledger _

instance showLedger :: Show Ledger where
  show = genericShow

instance decodeLedger :: DecodeJson Ledger where
  decodeJson :: Json -> Either String Ledger
  decodeJson json = do
    object <- maybe (Left "Ledger is not an object") Right (toObject json)
    owner <- getObjField object "owner"
    transactions <- getObjField object "transactions"
    pure $ Ledger {owner, transactions}


prettyShowLedger :: Ledger -> String
prettyShowLedger (Ledger l) =
  let prettyTransactions = map prettyShowTransaction l.transactions
  in
    "Ledger by " <> l.owner <> "\n"
    <> fold prettyTransactions


type BalanceMap = Map.Map AccountId Account


addTransaction :: Transaction -> BalanceMap -> BalanceMap
addTransaction (Transaction {to, from, amount}) balanceMap =
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



showBalance :: Ledger -> String
showBalance (Ledger ledger) =
  let
    indentation = 60
  in
    foldr addTransaction (Map.empty :: BalanceMap) ledger.transactions
      # (Map.toUnfoldable :: BalanceMap -> Array (Tuple AccountId Account))
      # map (\(Tuple accountId (Account _ commodityMap)) ->
        format (width indentation) accountId
        <> indentSubsequent indentation (prettyShowCommodityMap commodityMap)
        <> "\n")
      # fold


jsonStringToLedger :: String -> Either String Ledger
jsonStringToLedger jsonString = do
  json <- jsonParser jsonString
  ledger <- decodeJson json
  pure ledger


yamlStringToLedger :: String -> Either String Ledger
yamlStringToLedger yamlString =
  case runExcept $ parseYAMLToJson yamlString of
    Left error -> Left "Could not parse YAML"
    Right json -> decodeJson json
