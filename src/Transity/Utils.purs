module Transity.Utils
  ( getObjField
  , getFieldVerbose
  , getFieldMaybe
  , parseToUnixTime
  , stringToDateTime
  , utcToIsoString
  , dateShowPretty
  , indentSubsequent
  , digitsToRational
  , padStart
  , padEnd
  )
where

import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core (Json, JObject)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional)
import Data.Array (elem, all)
import Data.Bounded (bottom)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Result (Result(..), fromEither)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format) as Fmt
import Data.Int (fromString, pow)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Monoid (power)
import Data.Rational (Rational, (%))
import Data.Show (show)
import Data.String
  ( indexOf
  , length
  , replaceAll
  , toCharArray
  , fromCharArray
  , Pattern(Pattern)
  , Replacement(Replacement)
  )
import Data.StrMap (StrMap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Unfoldable (replicate)
import Prelude
  ( ($)
  , (<>)
  , (#)
  , (-)
  , bind
  , pure
  )


foreign import parseToUnixTime :: String -> Number


getObjField :: forall a. DecodeJson a => JObject -> String -> Result String a
getObjField object name =
  let
    value = fromEither $ object `getField` name
  in
    case value of
      Error error ->
        Error $ "'" <> name <> "' could not be parsed: \n  " <> error
      Ok success -> Ok success


getFieldMaybe :: forall a. DecodeJson a
  => JObject -> String -> Result String (Maybe a)
getFieldMaybe object name =
  fromEither $ getFieldOptional object name


getFieldVerbose
  :: forall a. DecodeJson a
  => StrMap Json -> String -> Result String a
getFieldVerbose object name =
  let
    value = fromEither $ object `getField` name
  in
    case value of
      Error error -> Error $
        "'" <> name <> "' could not be parsed in " <>
        (show object) <> " because of following error: \n  " <> error
      Ok success -> Ok success


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


getPadding :: Int -> String -> String
getPadding targetLength string =
  fromCharArray $ replicate (targetLength - length string) ' '


padStart :: Int -> String -> String
padStart targetLength string =
  getPadding targetLength string <> string


padEnd :: Int -> String -> String
padEnd targetLength string =
  string <> getPadding targetLength string
