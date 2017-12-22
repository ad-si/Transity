module Transity.Data.Ledger where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude
  ( class Show
  , bind
  , pure
  , ($)
  , (<>)
  )
import Transity.Data.Transaction ( Transaction , prettyShowTransaction )
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
  "Ledger by " <> l.owner <> "\n"
  <> (foldr
    (\trans out -> out <> (prettyShowTransaction trans))
    ""
    l.transactions
  )


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
