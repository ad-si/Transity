module Transity.Data.Transfer where

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, stringify, toObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getFieldOptional)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime (DateTime)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype)
import Data.Rational ((%))
import Data.Result (Result(..), fromEither, toEither)
import Data.Ring (negate)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Foreign (renderForeignError)
import JS.BigInt (fromInt)
import Prelude
  ( class Eq
  , class Ord
  , class Show
  , bind
  , compare
  , map
  , pure
  , (#)
  , ($)
  , (<>)
  , (==)
  , (>>=)
  )
import Text.Format (format, width)
import Transity.Data.Account (Id) as Account
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount as Amount
import Transity.Data.Config (ColorFlag(..))
import Transity.Utils
  ( dateShowPretty
  , getFieldVerbose
  , ratioZero
  , resultWithJsonDecodeError
  , stringToDateTime
  , stringifyJsonDecodeError
  )

newtype Transfer = Transfer
  { utc :: Maybe DateTime
  , from :: Account.Id
  , to :: Account.Id
  , amount :: Amount
  , note :: Maybe String
  }

derive instance genericTransfer :: Generic Transfer _
derive instance newtypeTransfer :: Newtype Transfer _

instance eqTransfer :: Eq Transfer where
  eq (Transfer a) (Transfer b) = a.utc == b.utc

instance ordTransfer :: Ord Transfer where
  compare (Transfer a) (Transfer b) = compare a.utc b.utc

instance showTransfer :: Show Transfer where
  show = genericShow

instance decodeTransfer :: DecodeJson Transfer where
  decodeJson json =
    toEither $ resultWithJsonDecodeError $ decodeJsonTransfer json

decodeJsonTransfer :: Json -> Result String Transfer
decodeJsonTransfer json = do
  object <- maybe (Error "Transfer is not an object") Ok (toObject json)

  from <- object `getFieldVerbose` "from"
  to <- object `getFieldVerbose` "to"
  amount <- object `getFieldVerbose` "amount"

  utc <- stringifyJsonDecodeError
    $ fromEither
    $ object `getFieldOptional` "utc"
  note <- stringifyJsonDecodeError
    $ fromEither
    $ object `getFieldOptional` "note"

  transfer <- verifyTransfer (stringify json)
    ( Transfer
        { utc: utc >>= stringToDateTime
        , from
        , to
        , amount
        , note
        }
    )

  pure transfer

transferZero :: Transfer
transferZero = Transfer
  { utc: Nothing
  , from: ""
  , to: ""
  , amount: Amount ratioZero (Commodity "")
  , note: Nothing
  }

negateTransfer :: Transfer -> Transfer
negateTransfer (Transfer transferRec) =
  let
    negateAmount (Amount qnt com) = Amount (negate qnt) com
  in
    Transfer transferRec { amount = negateAmount transferRec.amount }

verifyTransfer :: String -> Transfer -> Result String Transfer
verifyTransfer json transfer@(Transfer transRec) =
  let
    (Amount quantity _) = transRec.amount
  in
    if (length transRec.from == 0) then Error $ "Field 'from' in " <> json <>
      " must not be empty"
    else if (length transRec.to == 0) then Error $ "Field 'to' in " <> json <>
      " must not be empty"
    else if (quantity == (fromInt 0 % fromInt 1)) then Error $
      "Field 'amount' in " <> json <> " must not be 0"
    else Ok transfer

fromJson :: String -> Result String Transfer
fromJson string = do
  json <- fromEither $ jsonParser string
  transfer <- stringifyJsonDecodeError $ fromEither $ decodeJson json
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
      Ok json -> stringifyJsonDecodeError $ fromEither $ decodeJson json

showPretty :: Transfer -> String
showPretty = showPrettyAligned ColorNo 15 15 5 3 10

showPrettyColorized :: Transfer -> String
showPrettyColorized = showPrettyAligned ColorYes 15 15 5 3 10

-- | - From account name width
-- | - To account name width
-- | - Integer part width
-- | - Fraction part width
-- | - Commodity width

showPrettyAligned ::
  ColorFlag -> Int -> Int -> Int -> Int -> Int -> Transfer -> String
showPrettyAligned colorFlag fromW toW intW fracW comW (Transfer trans) =
  let
    datePretty = map dateShowPretty trans.utc
    offsetDate = 19
  in
    case datePretty of
      Just aDate -> aDate <> " | "
      _ -> " " `power` offsetDate
      <> format (width fromW) trans.from
      <> " -> "
      <> format (width toW) trans.to
      <> " : "
      <> Amount.showPrettyAligned colorFlag intW fracW comW trans.amount
      <> " | "
      <> fromMaybe "" trans.note
      <> "\n"
