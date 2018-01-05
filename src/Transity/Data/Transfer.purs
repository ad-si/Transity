module Transity.Data.Transfer
  ( Transfer(Transfer)
  , prettyShowTransfer
  , jsonStringToTransfer
  , yamlStringToTransfer
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


jsonStringToTransfer :: String -> Either String Transfer
jsonStringToTransfer string = do
  json <- jsonParser string
  transfer <- decodeJson json
  pure transfer


yamlStringToTransfer :: String -> Either String Transfer
yamlStringToTransfer yamlString =
  case runExcept $ parseYAMLToJson yamlString of
    Left error -> Left "Could not parse YAML"
    Right json -> decodeJson json


newtype Transfer = Transfer
  { entryDate :: DateTime
  , valueDate :: DateTime
  , from :: AccountId
  , to :: AccountId
  , amount :: Amount
  -- , desc :: String
  }

derive instance genericTransfer :: Generic Transfer _

instance showTransfer :: Show Transfer where
  show = genericShow

instance decodeTransfer :: DecodeJson Transfer where
  decodeJson :: Json -> Either String Transfer
  decodeJson json = do
    object <- maybe (Left "Transfer is not an object") Right (toObject json)

    entryDate <- (getObjField object "entryDate" :: Either String String)
    valueDate <- (getObjField object "valueDate" :: Either String String)
    from      <- getObjField object "from"
    to        <- getObjField object "to"
    amount    <- getObjField object "amount"
    -- desc      <- getObjField object "desc"

    pure $ Transfer
      { entryDate: stringToDateTime entryDate
      , valueDate: stringToDateTime valueDate
      , from
      , to
      , amount
      -- , desc
      }


prettyShowTransfer :: Transfer -> String
prettyShowTransfer (Transfer t) =
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
