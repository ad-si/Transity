module Main where

-- import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
-- import Control.Monad.Except.Trans (ExceptT)
-- import Control.Plus (empty)
import Data.Argonaut.Core (toObject, toString, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (take, foldr)
import Data.Bounded (bottom)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
-- import Data.Foreign (ForeignError)
-- import Data.Foreign.Class (class Encode, class Decode, encode, decode)
-- import Data.Foreign.Generic (defaultOptions, genericDecodeJSON)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format) as Fmt
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
-- import Data.Int (floor)
-- import Data.Identity (Identity)
-- import Data.JSDate (JSDate, LOCALE, parse, isValid)
-- import Data.List.Types (NonEmptyList)
import Data.List (fromFoldable)
import Data.Maybe (fromMaybe, maybe)
import Data.String (split, Pattern(Pattern))
import Data.StrMap (StrMap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.YAML.Foreign.Decode (parseYAMLToJson)
-- import Debug.Trace (spy)
import Global (readFloat)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Path as Path
import Node.FS.Sync as Sync
import Prelude
  ( class Show
  , Unit
  , bind
  , pure
  , show
  , ($)
  , (#)
  , (<>)
  )
import Text.Format (format, width, precision)


foreign import parseToUnixTime :: String -> Number


utcToIsoString :: DateTime -> String
utcToIsoString utc =
  let
    formatter :: Fmt.Formatter
    formatter = fromFoldable
      [ Fmt.YearFull, (Fmt.Placeholder "-")
      , Fmt.MonthTwoDigits, (Fmt.Placeholder "-")
      , Fmt.DayOfMonthTwoDigits, (Fmt.Placeholder "T")
      , Fmt.Hours24, (Fmt.Placeholder ":")
      , Fmt.MinutesTwoDigits, (Fmt.Placeholder ":")
      , Fmt.SecondsTwoDigits
      ]
  in
    Fmt.format formatter utc


getObjField
  :: forall a. DecodeJson a
  => StrMap Json -> String -> Either String a
getObjField object name = case (getField object name) of
  Left error -> Left $ "'" <> name <> "' could not be parsed: " <> error
  Right success -> Right success


type Commodity = String
data Amount = Amount Number Commodity

derive instance genericAmount :: Generic Amount _

instance showAmount :: Show Amount where
  show = genericShow

instance decodeAmount :: DecodeJson Amount where
  decodeJson :: Json -> Either String Amount
  decodeJson json = do
    amount <- maybe (Left "Amount is not a string") Right (toString json)
    let amountFrags = split (Pattern " ") amount
    case take 2 amountFrags of
      [value, currency] -> Right $ Amount (readFloat value) currency
      _ -> Left "Amount does not contain a value and a commodity"


prettyShowAmount :: Amount -> String
prettyShowAmount (Amount value commodity) =
  format (width 8 <> precision 3) value
  <> " "
  <> format (width 3) commodity


newtype Transaction = Transaction
  { entryDate :: DateTime
  , valueDate :: DateTime
  , from :: String
  , to :: String
  , amount :: Amount
  , desc :: String
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
    desc      <- getObjField object "desc"

    pure $ Transaction
      { entryDate: stringToDateTime entryDate
      , valueDate: stringToDateTime valueDate
      , from, to, amount, desc}


prettyShowTransaction :: Transaction -> String
prettyShowTransaction (Transaction t) =
  (utcToIsoString t.valueDate) <> " | "
    <> format (width 15) t.from <> " => " <> format (width 15) t.to <> " "
    <> (prettyShowAmount t.amount) <> " | " <> t.desc <> "\n"


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


stringToDateTime :: String -> DateTime
stringToDateTime string = string
  # parseToUnixTime
  # Milliseconds
  # instant
  # fromMaybe bottom
  # toDateTime
  -- $ fromMaybe bottom (instant $ Milliseconds $ parseToUnixTime string)

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


yamlStringToLedger :: String -> Either String Ledger
yamlStringToLedger yamlString =
  case runExcept $ parseYAMLToJson yamlString of
    Left error -> Left "Could not parse YAML"
    Right json -> decodeJson json


main
  :: forall e
  . Eff (exception :: EXCEPTION, console :: CONSOLE, fs :: FS | e) Unit
main = do
  ledgerFile <- Sync.readTextFile UTF8 $ Path.concat ["tests", "ledger.yaml"]
  case yamlStringToLedger ledgerFile of
    Left error -> log $ show error
    Right ledger -> log $ prettyShowLedger ledger
