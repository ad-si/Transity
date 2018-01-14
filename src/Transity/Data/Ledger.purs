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
import Data.Foldable (fold, foldr)
import Data.Foreign (renderForeignError)
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.Result (Result(..), toEither, fromEither)
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
  decodeJson json = toEither $ decodeJsonLedger json


decodeJsonLedger :: Json -> Result String Ledger
decodeJsonLedger json = do
  object <- maybe (Error "Ledger is not an object") Ok (toObject json)
  owner <- getObjField object "owner"
  transactions <- getObjField object "transactions"
  pure $ Ledger {owner, transactions}


fromJson :: String -> Result String Ledger
fromJson json = do
  jsonObj <- fromEither $ jsonParser json
  ledger <- fromEither $ decodeJson jsonObj
  pure ledger


fromYaml :: String -> Result String Ledger
fromYaml yaml =
  let
    result = yaml
      # parseYAMLToJson
      # runExcept
      # fromEither
  in
    case result of
      Error error -> Error
        ( "Could not parse YAML: "
          <> fold (map renderForeignError error)
        )
      Ok json -> fromEither $ decodeJson json


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
