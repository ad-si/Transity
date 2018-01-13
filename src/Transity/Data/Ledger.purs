module Transity.Data.Ledger
  ( Ledger(Ledger)
  , fromJson
  , fromYaml
  , showPretty
  , showBalance
  )
where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.Foreign (renderForeignError)
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.Tuple (Tuple(Tuple))
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude (class Show, bind, pure, ($), (<>), (#))
import Transity.Data.Account
  ( Account(..)
  , CommodityMap
  , addAmountToMap
  , subtractAmountFromMap
  )
import Transity.Data.Account (Id, showPretty) as Account
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Transaction (showPretty) as Transaction
import Transity.Data.Transfer (Transfer(..))
import Transity.Utils (getObjField)


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


fromJson :: String -> Either String Ledger
fromJson json = do
  jsonObj <- jsonParser json
  ledger <- decodeJson jsonObj
  pure ledger


fromYaml :: String -> Either String Ledger
fromYaml yaml =
  case runExcept $ parseYAMLToJson yaml of
    Left error -> Left
      ( "Could not parse YAML: "
        <> fold (map renderForeignError error)
      )
    Right json -> decodeJson json


showPretty :: Ledger -> String
showPretty (Ledger l) =
  let transactionsPretty = map Transaction.showPretty l.transactions
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



showBalance :: Ledger -> String
showBalance (Ledger ledger) =
  let
    indentation = 60
  in
    foldr addTransaction (Map.empty :: BalanceMap) ledger.transactions
      # (Map.toAscUnfoldable :: BalanceMap -> Array (Tuple Account.Id Account))
      # map (\(Tuple _ account) -> Account.showPretty account)
      # fold
