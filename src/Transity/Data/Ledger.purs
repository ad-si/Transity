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
import Data.Tuple (Tuple, fst, snd)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude (class Show, bind, pure, ($), (<>), (#))
import Text.Format (format, width)
import Transity.Data.Amount (Amount, prettyShowAmount, subtractAmount, negateAmount)
import Transity.Data.Transaction (Transaction(Transaction), prettyShowTransaction)
import Transity.Utils (getObjField)


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


type BalanceMap = Map.Map String Amount


addTransaction :: Transaction -> BalanceMap -> BalanceMap
addTransaction (Transaction {to, from, amount}) balanceMap =
  let
    balanceMap' = Map.alter
      (\maybeValue -> case maybeValue of
        Nothing -> Just (negateAmount amount)
        Just amountNow -> Just (amountNow `subtractAmount` amount)
      )
      from
      balanceMap
  in
    -- TODO: How does alter exactly work??
    Map.alter
      (\maybeValue -> case maybeValue of
        Nothing -> Just amount
        Just amountNow -> Just (amountNow <> amount)
      )
      to
      balanceMap'



showBalance :: Ledger -> String
showBalance (Ledger ledger) =
  foldr addTransaction (Map.empty :: BalanceMap) ledger.transactions
    # (Map.toUnfoldable :: BalanceMap -> Array (Tuple String Amount))
    # map (\tuple -> format (width 60) (fst tuple)
      <> prettyShowAmount (snd tuple)
      <> "\n")
    # fold

-- format (width 10 <> precision 3) value
--   <> " "
--   <> format (width 3) commodity


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
