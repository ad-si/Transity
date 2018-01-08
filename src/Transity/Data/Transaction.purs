module Transity.Data.Transaction
  ( Transaction(Transaction)
  , showPretty
  , fromJson
  , fromYaml
  )
where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (toObject, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getFieldOptional)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Foreign (renderForeignError)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing,Just), maybe, fromMaybe)
import Data.Monoid (power)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude (($), (<>), bind, class Show, map, pure)
import Text.Format (format, width)
import Transity.Data.Transfer (Transfer)
import Transity.Data.Transfer (showPretty) as Transfer
import Transity.Utils
  ( getObjField
  , stringToDateTime
  , dateShowPretty
  , indentSubsequent
  )


-- newtype FilePath = FilePath String

newtype Transaction = Transaction
  { id :: Maybe String
  , utc :: Maybe DateTime
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

    id        <- object `getFieldOptional` "id"
    utc      <- object `getFieldOptional` "utc"
    note      <- object `getFieldOptional` "note"
    receipt   <- object `getFieldOptional` "receipt"
    transfers <- object `getObjField` "transfers"

    pure $ Transaction
      { id
      , utc: case utc of
          Nothing -> Nothing
          Just dateString -> Just (stringToDateTime dateString)
      , note
      , receipt
      , transfers
      }


fromJson :: String -> Either String Transaction
fromJson string = do
  json <- jsonParser string
  transaction <- decodeJson json
  pure transaction


fromYaml :: String -> Either String Transaction
fromYaml yaml =
  case runExcept $ parseYAMLToJson yaml of
    Left error -> Left
      ( "Could not parse YAML: "
        <> fold (map renderForeignError error)
      )
    Right json -> decodeJson json


showPretty :: Transaction -> String
showPretty (Transaction tact) =
  let
    transfersPretty = map Transfer.showPretty tact.transfers
    offsetDate = 16
    offsetIndentation = 4
  in
    fromMaybe (" " `power` offsetDate) (map dateShowPretty tact.utc)
    <> " - " <> format (width 30) (fromMaybe "NO NOTE" tact.note)
    <> " (id " <> fromMaybe "NO ID" tact.id <> ")"
    <> indentSubsequent offsetIndentation ("\n" <> fold transfersPretty)
    <> "\n"
