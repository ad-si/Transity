module Transity.Utils
  ( getObjField
  )
where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators (getField)
import Data.Either (Either(..))
import Data.StrMap (StrMap)
import Prelude
  ( ($)
  , (<>)
  )

getObjField
  :: forall a. DecodeJson a
  => StrMap Json -> String -> Either String a
getObjField object name = case (getField object name) of
  Left error -> Left $ "'" <> name <> "' could not be parsed: " <> error
  Right success -> Right success
