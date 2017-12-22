module Transity.Data.Amount
  ( Amount(Amount)
  , prettyShowAmount
  , Commodity
  )
where

import Data.Argonaut.Core (toString, Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Array (take)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Global (readFloat)
import Prelude
  ( class Show
  , bind
  , ($)
  , (<>)
  )
import Data.String (split, Pattern(Pattern))
import Text.Format (format, width, precision)


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
