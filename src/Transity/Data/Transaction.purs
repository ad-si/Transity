module Transity.Data.Transaction
  ( Transaction(Transaction)
  , prettyShowTransaction
  , jsonStringToTransaction
  , yamlStringToTransaction
  )
where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude
  ( ($)
  , (<>)
  , bind
  , class Show
  , pure
  )
import Text.Format (format, width)
import Transity.Data.Amount (Amount, prettyShowAmount)
import Transity.Data.Account (AccountId)
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


newtype Transaction = Transaction
  { entryDate :: DateTime
  , valueDate :: DateTime
  , from :: AccountId
  , to :: AccountId
  , amount :: Amount
  -- , desc :: String
  }

derive instance genericTransaction :: Generic Transaction _

instance showTransaction :: Show Transaction where
  show = genericShow

instance decodeTransaction :: DecodeJson Transaction where
  decodeJson :: Json -> Either String Transaction
  decodeJson json = do
    object <- maybe (Left "Transaction is not an object") Right (toObject json)

    entryDate <- (getObjField object "entryDate" :: Either String String)
    valueDate <- (getObjField object "valueDate" :: Either String String)
    from      <- getObjField object "from"
    to        <- getObjField object "to"
    amount    <- getObjField object "amount"
    -- desc      <- getObjField object "desc"

    pure $ Transaction
      { entryDate: stringToDateTime entryDate
      , valueDate: stringToDateTime valueDate
      , from
      , to
      , amount
      -- , desc
      }


prettyShowTransaction :: Transaction -> String
prettyShowTransaction (Transaction t) =
  (utcToIsoString t.valueDate)
  <> " | "
  <> format (width 15) t.from
  <> " => "
  <> format (width 15) t.to
  <> " "
  <> (prettyShowAmount t.amount)
  <> " | "
  -- <> t.desc
  <> "\n"
