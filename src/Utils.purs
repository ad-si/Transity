module Transity.Utils
  ( getObjField
  , parseToUnixTime
  , stringToDateTime
  , utcToIsoString
  , indentSubsequent
  )
where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField)
import Data.Bounded (bottom)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format) as Fmt
import Data.List (fromFoldable)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.String (replaceAll, Pattern(Pattern), Replacement(Replacement))
import Data.StrMap (StrMap)
import Prelude
  ( ($)
  , (<>)
  , (#)
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
