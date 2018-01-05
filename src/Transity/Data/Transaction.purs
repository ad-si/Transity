module Transity.Data.Transaction
  ( Transaction(Transaction)
  , prettyShowTransaction
  , jsonStringToTransaction
  , yamlStringToTransaction
  )
where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing,Just), maybe, fromMaybe)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude (($), (<>), bind, class Show, pure)
import Text.Format (format, width)
import Transity.Data.Account (AccountId)
import Transity.Data.Amount (Amount, prettyShowAmount)
import Transity.Data.Transfer (Transfer)
import Transity.Utils (getObjField, stringToDateTime, utcToIsoString)


jsonStringToTransaction :: String -> Either String Transaction
jsonStringToTransaction string = do
  json <- jsonParser string
  transaction <- decodeJson json
  pure transaction


yamlStringToTransaction :: String -> Either String Transaction
yamlStringToTransaction yamlString =
  case runExcept $ parseYAMLToJson yamlString of
    Left error -> Left "Could not parse YAML"
    Right json -> decodeJson json


-- newtype FilePath = FilePath String

newtype Transaction = Transaction
  { id :: Maybe String
  , date :: Maybe DateTime
  , note :: Maybe String
  , receipt :: Maybe String
  , transfers :: Array Transfer
  }

derive instance genericTransaction :: Generic Transaction _

instance showTransaction :: Show Transaction where
  show = genericShow

instance decodeTransaction :: DecodeJson Transaction where
  decodeJson :: Json -> Either String Transaction
  decodeJson json = do
    object <- maybe (Left "Transaction is not an object") Right (toObject json)

    id        <- getObjField object "id"
    date      <- getObjField object "date"
    note      <- getObjField object "note"
    receipt   <- getObjField object "receipt"
    transfers <- getObjField object "transfers"

    pure $ Transaction
      { id
      , date: case date of
                Nothing -> Nothing
                Just dateString -> Just (stringToDateTime dateString)
      , note
      , receipt
      , transfers
      }

prettyShowTransaction :: Transaction -> String
prettyShowTransaction (Transaction t) =
  -- utcToIsoString (fromMaybe "NO DATE" t.date)
  -- <> " | " <>
  format (width 30) (fromMaybe "NO ID" t.id)
  <> " "
  <> format (width 30) (fromMaybe "NO NOTE" t.note)
  <> " "
  <> format (width 30) (fromMaybe "NO RECEIPT" t.receipt)
  <> "\n"
