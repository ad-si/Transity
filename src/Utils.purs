module Transity.Utils
  ( getObjField
  , parseToUnixTime
  , stringToDateTime
  )
where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField)
import Data.Bounded (bottom)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
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
