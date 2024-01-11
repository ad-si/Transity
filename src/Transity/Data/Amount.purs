module Transity.Data.Amount
where

import Control.Bind (bind)
import Data.Argonaut.Core (toString, fromString, Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (take)
import Data.Boolean (otherwise)
import Data.Eq (class Eq, (/=), (==))
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (class Monoid)
import Data.Newtype
import Data.Ord (class Ord)
import Data.Rational (Rational)
import Data.Rational (toNumber) as Rational
import Data.Result (Result(..), toEither)
import Data.Ring ((-))
import Data.Ring (negate) as Ring
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring ((+))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), length, split)
import Data.Tuple (Tuple(..))

import Transity.Data.Config (ColorFlag(..))
import Transity.Utils
  ( digitsToRational
  , padEnd
  , alignNumber
  , lengthOfNumParts
  , WidthRecord
  , widthRecordZero
  , ratioZero
  )


-- | Economic good or service that has full or substantial fungibility
-- | E.g. €, cows, minutes, …

newtype Commodity = Commodity String

derive instance newtypeCommodity :: Newtype Commodity _
derive instance genericCommodity :: Generic Commodity _
derive newtype instance eqCommodity :: Eq Commodity
derive newtype instance ordCommodity :: Ord Commodity

instance showCommodity :: Show Commodity where
  show = genericShow

instance encodeCommodity :: EncodeJson Commodity where
  encodeJson a = genericEncodeJson a

instance decodeCommodity :: DecodeJson Commodity where
  decodeJson json = toEither $ decodeJsonCommodity json

decodeJsonCommodity :: Json -> Result JsonDecodeError Commodity
decodeJsonCommodity json =
  maybe
    (Error $ TypeMismatch "Commodity is not a string")
    (\x -> Ok (Commodity x))
    (toString json)


--| E.g. "20 €", "10 cows", or "20 minutes"
--| `amount = Amount (fromInt 20) (Commodity "€")

data Amount = Amount Rational Commodity

derive instance genericAmount :: Generic Amount _
derive instance eqAmount :: Eq Amount
derive instance ordAmount :: Ord Amount

instance semigroupAmount :: Semigroup Amount where
  append (Amount numA (Commodity comA)) (Amount numB (Commodity comB))
    | comA /= comB =
        Amount ratioZero (Commodity "INVALID COMPUTATION")
    | otherwise = Amount (numA + numB) (Commodity comA)

instance monoidAmount :: Monoid Amount where
  mempty = Amount ratioZero (Commodity "")

instance showAmount :: Show Amount where
  show = genericShow

instance encodeAmount :: EncodeJson Amount where
  encodeJson _ = fromString "TODO" -- genericEncodeJson a

instance decodeAmount :: DecodeJson Amount where
  decodeJson json = toEither $ decodeJsonAmount json


parseAmount :: String -> Result JsonDecodeError Amount
parseAmount string = do
  let amountFrags = split (Pattern " ") string
  case take 2 amountFrags of
    [value, currency] -> case digitsToRational value of
      Nothing -> Error $ TypeMismatch "Amount does not contain a valid value"
      Just quantity ->
        Ok $ Amount quantity (Commodity currency)
    _ -> Error $ TypeMismatch "Amount does not contain a value and a commodity"


decodeJsonAmount :: Json -> Result JsonDecodeError Amount
decodeJsonAmount json = do
  amount <- maybe
    (Error $ TypeMismatch "Amount is not a string")
    Ok (toString json)
  parseAmount amount


subtract :: Amount -> Amount -> Amount
subtract (Amount numA (Commodity comA)) (Amount numB (Commodity comB))
  | comA /= comB = Amount ratioZero (Commodity "INVALID COMPUTATION")
  | otherwise = Amount (numA - numB) (Commodity comA)


negate :: Amount -> Amount
negate (Amount num com) =
  Amount (Ring.negate num) com


isZero :: Amount -> Boolean
isZero (Amount quantity _) =
  quantity == ratioZero


toWidthRecord :: Amount -> WidthRecord
toWidthRecord (Amount quantity (Commodity commodity)) =
  let
    Tuple intPart fracPart = lengthOfNumParts (Rational.toNumber quantity)
  in
    widthRecordZero
      { integer = intPart
      , fraction = fracPart
      , commodity = length commodity
      }


showPretty :: Amount -> String
showPretty = showPrettyAligned ColorNo 0 0 0


--| Specify the width (in characters) of the integer part,
--| the width of the fractional part (including decimal point),
--| the width of commodity part
--| and receive a pretty printed amount.

showPrettyAligned :: ColorFlag -> Int -> Int -> Int -> Amount -> String
showPrettyAligned colorFlag intWid fracWid comWid (Amount val (Commodity com)) =
  alignNumber colorFlag intWid fracWid (Rational.toNumber val)
  <> " "
  <> padEnd comWid com
