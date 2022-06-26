module Transity.Data.Balance where

import Prelude (class Show, class Eq, bind, pure, ($), (<#>))

import Data.Argonaut.Core (Json, toObject, fromString, jsonEmptyArray)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Result (Result(Ok, Error), toEither, note)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object as Object
import Transity.Data.Amount (parseAmount)
import Transity.Data.CommodityMap (CommodityMap, fromAmounts)
import Transity.Utils
  ( getObjField
  , stringToDateTime
  , resultWithJsonDecodeError
  , stringifyJsonDecodeError
  , utcToIsoString
  )


data Balance = Balance DateTime CommodityMap


derive instance genericBalance :: Generic Balance _
derive instance eqBalance :: Eq Balance

instance showBalance :: Show Balance where
  show = genericShow

instance encodeBalance :: EncodeJson Balance where
  encodeJson (Balance dateTime _ {- TODO: commodityMap -}) =
    A.fromObject $ Object.fromFoldable
      -- TODO: Hide seconds if 00
      [ Tuple "utc" (fromString $ utcToIsoString dateTime)
      , Tuple "amounts" jsonEmptyArray {- TODO: commodityMap -}
      ]

instance decodeBalance :: DecodeJson Balance where
  decodeJson json = toEither
    $ resultWithJsonDecodeError $ decodeJsonBalance json


decodeJsonBalance :: Json -> Result String Balance
decodeJsonBalance json = do
  object  <- maybe (Error "Balance is not an object") Ok (toObject json)
  utc     <- object `getObjField` "utc"
  (amounts :: Array String) <- object `getObjField` "amounts"

  amountList <- sequence $ amounts <#> parseAmount <#> stringifyJsonDecodeError

  utcResult <- note "UTC string is invalid" $ stringToDateTime utc
  let balance = Balance utcResult (fromAmounts amountList)
  pure balance

