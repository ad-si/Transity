module Transity.Data.Transfer
  ( Transfer(Transfer)
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
import Data.Result (Result(..), toEither, fromEither)
import Data.Eq ((==))
import Data.Foreign (renderForeignError)
import Data.Foldable (fold)
import Data.Function ((#))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe)
import Data.Monoid (power)
import Data.Rational (fromInt)
import Data.Show (show)
import Data.String (length)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Prelude
  ( ($)
  , (<>)
  , bind
  , class Show
  , pure
  , map
  )
import Text.Format (format, width)
import Transity.Data.Amount (Amount(..))
import Transity.Data.Amount (showPretty) as Amount
import Transity.Data.Account (Id) as Account
import Transity.Utils
  ( getFieldVerbose
  , stringToDateTime
  , dateShowPretty
  )


newtype Transfer = Transfer
  { utc :: Maybe DateTime
  , from :: Account.Id
  , to :: Account.Id
  , amount :: Amount
  , note :: Maybe String
  }

derive instance genericTransfer :: Generic Transfer _

instance showTransfer :: Show Transfer where
  show = genericShow

instance decodeTransfer :: DecodeJson Transfer where
  decodeJson json = toEither $ decodeJsonTransfer json


decodeJsonTransfer :: Json -> Result String Transfer
decodeJsonTransfer json = do
  object <- maybe (Error "Transfer is not an object") Ok (toObject json)

  from <- object `getFieldVerbose` "from"
  to <- object `getFieldVerbose` "to"
  amount <- object `getFieldVerbose` "amount"

  utc <- fromEither $ object `getFieldOptional` "utc"
  note <- fromEither $ object `getFieldOptional` "note"

  transfer <- verifyTransfer (show json) (Transfer
      { utc: map stringToDateTime utc
      , from
      , to
      , amount
      , note
      }
    )

  pure transfer


verifyTransfer :: String -> Transfer -> Result String Transfer
verifyTransfer json transfer@(Transfer transRec) =
  let
    (Amount quantity _) = transRec.amount
  in
    if (length transRec.from == 0)
      then Error $ "Field 'from' in " <> json <>  " must not be empty" else
    if (length transRec.to == 0)
      then Error $ "Field 'to' in " <> json <>  " must not be empty" else
    if (quantity == (fromInt 0))
      then Error $ "Field 'amount' in " <> json <>  " must not be 0"
    else Ok transfer


fromJson :: String -> Result String Transfer
fromJson string = do
  json <- fromEither $ jsonParser string
  transfer <- fromEither $ decodeJson json
  pure transfer


fromYaml :: String -> Result String Transfer
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


showPretty :: Transfer -> String
showPretty (Transfer trans) =
  let
    datePretty = map dateShowPretty trans.utc
    offsetDate = 19
  in
    case datePretty of
      Nothing -> " " `power` offsetDate
      Just aDate -> aDate <> " | "
    <> format (width 15) trans.from
    <> " -> "
    <> format (width 15) trans.to
    <> " "
    <> Amount.showPretty trans.amount
    <> " | "
    <> fromMaybe "" trans.note
    <> "\n"
