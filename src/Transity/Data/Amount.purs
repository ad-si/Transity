module Transity.Data.Amount
  ( Amount(Amount)
  , Commodity
  , negateAmount
  , prettyShowAmount
  , subtractAmount
  )
where

import Data.Argonaut.Core (toString, Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Array (take)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.Monoid (class Monoid)
import Data.Ring (negate, (-))
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring ((+))
import Global (readFloat)
import Prelude
  ( class Show
  , bind
  , ($)
  , (/=)
  )
import Data.String (split, Pattern(Pattern))
import Text.Format (format, width, precision)


type Commodity = String
data Amount = Amount Number Commodity

instance semigroupAmount :: Semigroup Amount where
  append (Amount numA comA) (Amount numB comB)
    | comA /= comB = Amount 0.0 "INVALID COMPUTATION"
    | otherwise = Amount (numA + numB) comA

instance monoidAmount :: Monoid Amount where
  mempty = Amount 0.0 ""


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


subtractAmount :: Amount -> Amount -> Amount
subtractAmount (Amount numA comA) (Amount numB comB)
  | comA /= comB = Amount 0.0 "INVALID COMPUTATION"
  | otherwise = Amount (numA - numB) comA


negateAmount :: Amount -> Amount
negateAmount (Amount num com) = Amount (negate num) com


prettyShowAmount :: Amount -> String
prettyShowAmount (Amount value commodity) =
  format (width 10 <> precision 3) value
  <> " "
  <> format (width 3) commodity
