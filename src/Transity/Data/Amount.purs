module Transity.Data.Amount
  ( Amount(Amount)
  , Commodity(Commodity)
  , negate
  , showPretty
  , subtract
  )
where

import Data.Argonaut.Core (toString, Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Array (take)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord)
import Data.Rational (Rational, fromInt, toNumber, (%))
import Data.Ring ((-))
import Data.Ring (negate) as Ring
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring ((+))
import Data.String (split, Pattern(Pattern))
import Prelude
  ( class Show
  , bind
  , ($)
  , (/=)
  )
import Text.Format (format, width, precision)
import Transity.Utils (digitsToRational, padEnd)


newtype Commodity = Commodity String

derive instance genericCommodity :: Generic Commodity _
derive newtype instance eqCommodity :: Eq Commodity
derive newtype instance ordCommodity :: Ord Commodity

instance showCommodity :: Show Commodity where
  show = genericShow



data Amount = Amount Rational Commodity

derive instance genericAmount :: Generic Amount _
derive instance eqAmount :: Eq Amount
derive instance ordAmount :: Ord Amount

instance semigroupAmount :: Semigroup Amount where
  append (Amount numA (Commodity comA)) (Amount numB (Commodity comB))
    | comA /= comB = Amount (fromInt 0) (Commodity "INVALID COMPUTATION")
    | otherwise = Amount (numA + numB) (Commodity comA)

instance monoidAmount :: Monoid Amount where
  mempty = Amount (fromInt 0) (Commodity "")

instance showAmount :: Show Amount where
  show = genericShow

instance decodeAmount :: DecodeJson Amount where
  decodeJson :: Json -> Either String Amount
  decodeJson json = do
    amount <- maybe (Left "Amount is not a string") Right (toString json)
    let amountFrags = split (Pattern " ") amount
    case take 2 amountFrags of
      [value, currency] -> case digitsToRational value of
        Nothing -> Left "Amount does not contain a valid value"
        Just quantity -> Right $ Amount quantity (Commodity currency)
      _ -> Left "Amount does not contain a value and a commodity"


subtract :: Amount -> Amount -> Amount
subtract (Amount numA (Commodity comA)) (Amount numB (Commodity comB))
  | comA /= comB = Amount (0 % 1) (Commodity "INVALID COMPUTATION")
  | otherwise = Amount (numA - numB) (Commodity comA)


negate :: Amount -> Amount
negate (Amount num com) = Amount (Ring.negate num) com


showPretty :: Amount -> String
showPretty (Amount value (Commodity commodity)) =
  format (width 10 <> precision 3) (toNumber value)
  <> " "
  <> padEnd 8 commodity
