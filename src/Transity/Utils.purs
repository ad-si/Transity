module Transity.Utils
  ( getObjField
  , parseToUnixTime
  , stringToDateTime
  , utcToIsoString
  , indentSubsequent
  , digitsToRational
  )
where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField)
import Data.Array (elem, all)
import Data.Bounded (bottom)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format) as Fmt
import Data.Int (fromString, pow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Monoid (power)
import Data.Rational (Rational, (%))
import Data.String
  ( indexOf
  , length
  , replaceAll
  , toCharArray
  , Pattern(Pattern)
  , Replacement(Replacement)
  )
import Data.StrMap (StrMap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Prelude
  ( ($)
  , (<>)
  , (#)
  , (-)
  , bind
  , pure
  )


foreign import parseToUnixTime :: String -> Number


getObjField
  :: forall a. DecodeJson a
  => StrMap Json -> String -> Either String a
getObjField object name = case (getField object name) of
  Left error -> Left $ "'" <> name <> "' could not be parsed: " <> error
  Right success -> Right success


stringToDateTime :: String -> DateTime
stringToDateTime string = string
  # parseToUnixTime
  # Milliseconds
  # instant
  # fromMaybe bottom
  # toDateTime


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


indentSubsequent :: Int -> String -> String
indentSubsequent indentation string  =
  replaceAll
    (Pattern "\n")
    (Replacement ("\n" <> (" " `power` indentation)))
    string


testNumberChar :: Char -> Boolean
testNumberChar char =
  let digitArray = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']
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
        denominator =  10 `pow` (numStrLength - index)
      numerator <- fromString numeratorStr
      pure (numerator % denominator)

