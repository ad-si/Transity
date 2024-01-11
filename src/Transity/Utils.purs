module Transity.Utils where

import Prelude (
  class Eq, bind, map, max, pure, show, Unit,
  (#), ($), (+), (-), (/=), (<#>), (<>), (==), (>=), (>>=), (>>>)
)

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional)
import Data.Argonaut.Decode.Error (printJsonDecodeError, JsonDecodeError(..))
import Data.Array (elem, all, (!!))
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Result (Result(..), fromEither)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format) as Fmt
import Effect (Effect)
import Effect.Exception (throw)
import JS.BigInt (fromInt, fromString, pow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)
import Data.Monoid (power)
import Data.Nullable (Nullable, toMaybe)
import Data.Rational (Rational, (%))
import Data.String
  ( indexOf
  , length
  , split
  , replaceAll
  , Pattern(..)
  , Replacement(..)
  )
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Foreign.Object (Object)

import Transity.Data.Config


-- | Flag to switch between different ways of sorting the output
data SortOrder = CustomSort | Alphabetically

derive instance eqSortOrder :: Eq SortOrder


foreign import parseToUnixTimeImpl :: String -> Nullable Number

parseToUnixTime :: String -> Maybe Number
parseToUnixTime = parseToUnixTimeImpl >>> toMaybe


resultWithJsonDecodeError :: forall a.
  Result String a -> Result JsonDecodeError a
resultWithJsonDecodeError result =
  case result of
    Error str -> Error $ TypeMismatch str
    Ok value -> Ok value


stringifyJsonDecodeError :: forall a.
  Result JsonDecodeError a -> Result String a
stringifyJsonDecodeError result =
  case result of
    Error err -> Error $ printJsonDecodeError err
    Ok value -> Ok value


getObjField :: forall a. DecodeJson a
  => Object Json -> String -> Result String a
getObjField object name =
  let
    value = fromEither $ object `getField` name
  in
    case value of
      Error error -> Error $ "'" <> name
                              <> "' could not be parsed: \n  "
                              <> printJsonDecodeError error
      Ok success -> Ok success


getFieldMaybe :: forall a. DecodeJson a
  => Object Json -> String -> Result String (Maybe a)
getFieldMaybe object name =
  stringifyJsonDecodeError $ fromEither $ getFieldOptional object name


getFieldVerbose
  :: forall a. DecodeJson a
  => Object Json -> String -> Result String a
getFieldVerbose object name =
  let
    value = fromEither $ object `getField` name
  in
    case value of
      Error error -> Error $
        "'" <> name <> "' could not be parsed in TODO "
        <> {-(stringify object) <>-} " because of following error: \n  "
        <> printJsonDecodeError error
      Ok success -> Ok success


stringToDateTime :: String -> Maybe DateTime
stringToDateTime string = string
  # parseToUnixTime
  <#> Milliseconds
  >>= instant
  <#> toDateTime


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

utcToIsoDateString :: DateTime -> String
utcToIsoDateString utc =
  let
    formatter :: Fmt.Formatter
    formatter = fromFoldable
      [ Fmt.YearFull, (Fmt.Placeholder "-")
      , Fmt.MonthTwoDigits, (Fmt.Placeholder "-")
      , Fmt.DayOfMonthTwoDigits
      ]
  in
    Fmt.format formatter utc

dateShowPretty :: DateTime -> String
dateShowPretty datetime =
  let
    formatter :: Fmt.Formatter
    formatter = fromFoldable
      [ Fmt.YearFull, (Fmt.Placeholder "-")
      , Fmt.MonthTwoDigits, (Fmt.Placeholder "-")
      , Fmt.DayOfMonthTwoDigits, (Fmt.Placeholder " ")
      , Fmt.Hours24, (Fmt.Placeholder ":")
      , Fmt.MinutesTwoDigits
      ]
  in
    Fmt.format formatter datetime

dateShowPrettyLong :: DateTime -> String
dateShowPrettyLong datetime =
  let
    formatter :: Fmt.Formatter
    formatter = fromFoldable
      [ Fmt.YearFull, (Fmt.Placeholder "-")
      , Fmt.MonthTwoDigits, (Fmt.Placeholder "-")
      , Fmt.DayOfMonthTwoDigits, (Fmt.Placeholder " ")
      , Fmt.Hours24, (Fmt.Placeholder ":")
      , Fmt.MinutesTwoDigits, (Fmt.Placeholder ":")
      , Fmt.SecondsTwoDigits
      ]
  in
    Fmt.format formatter datetime


indentSubsequent :: Int -> String -> String
indentSubsequent indentation string  =
  replaceAll
    (Pattern "\n")
    (Replacement ("\n" <> (" " `power` indentation)))
    string


testNumberChar :: Char -> Boolean
testNumberChar char =
  let
    digitArray =
      [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
      , '.', '-', '+'
      ]
  in elem char digitArray


digitsToRational :: String -> Maybe Rational
digitsToRational stringOfDigits =
  let
    isNumChar = all testNumberChar (toCharArray stringOfDigits)
  in case isNumChar of
    false -> Nothing
    true -> do
      let
        numeratorStr = replaceAll (Pattern ".") (Replacement "") stringOfDigits
        numStrLength = length numeratorStr
        index = fromMaybe numStrLength (indexOf (Pattern ".") stringOfDigits)
        denominator = (fromInt 10) `pow` (fromInt $ numStrLength - index)
      numerator <- fromString numeratorStr
      pure (numerator % denominator)


ratioZero :: Rational
ratioZero = 0 % 1


getPadding :: Int -> String -> String
getPadding targetLength string =
  fromCharArray $ replicate (targetLength - length string) ' '


padStart :: Int -> String -> String
padStart targetLength string =
  getPadding targetLength string <> string


padEnd :: Int -> String -> String
padEnd targetLength string =
  string <> getPadding targetLength string


alignNumber :: ColorFlag -> Int -> Int -> Number -> String
alignNumber colorFlag intWidth fracWidth number =
  let
    ifSet flag color = if flag == ColorYes
      then foreground color
      else foreground White
    colorMap =
      { positive: ifSet colorFlag Green
      , negative: ifSet colorFlag Red
      , neutral:  ifSet colorFlag BrightBlack
      }
    fragments = split (Pattern ".") (show number)
    intPart = case fragments !! 0 of
      Just int -> padStart intWidth int
      _ -> " " `power` intWidth
    emptyFrac = " " `power` fracWidth
    fracPart = case fragments !! 1 of
      Just frac | frac /= "0" -> padEnd fracWidth ("." <> frac)
      _ -> emptyFrac
  in
    -- TODO: Fix after https://github.com/hdgarrood/purescript-ansi/issues/7
    if colorFlag == ColorNo
    then intPart <> fracPart
    else
      if number >= 0.0
      then
        withGraphics colorMap.positive intPart
        <> withGraphics colorMap.neutral fracPart
      else
        withGraphics colorMap.negative intPart
        <> withGraphics colorMap.neutral fracPart


makeRed :: Config -> String -> String
makeRed conf str =
  if config.colorState == ColorYes
  then withGraphics (foreground Red) str
  else str


errorAndExit :: Config -> String -> Effect Unit
errorAndExit conf message = do
  throw (makeRed conf message)


-- | Decimal point is included in fraction => +1
lengthOfNumParts :: Number -> Tuple Int Int
lengthOfNumParts number =
  let
    fragments = split (Pattern ".") (show number)
    first = fromMaybe 0 (map length (fragments !! 0))
    second = case fragments !! 1 of
      Just frac | frac /= "0" -> (length frac) + 1
      _ -> 0
  in
    Tuple first second


type WidthRecord =
  { account :: Int
  , integer :: Int
  , fraction :: Int
  , commodity :: Int
  }


widthRecordZero :: WidthRecord
widthRecordZero =
  { account: 0
  , integer: 0
  , fraction: 0
  , commodity: 0
  }


mergeWidthRecords :: WidthRecord -> WidthRecord -> WidthRecord
mergeWidthRecords recA recB =
  recA
    { account   = max recA.account   recB.account
    , integer   = max recA.integer   recB.integer
    , fraction  = max recA.fraction  recB.fraction
    , commodity = max recA.commodity recB.commodity
    }
