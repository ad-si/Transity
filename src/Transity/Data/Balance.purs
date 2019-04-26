module Transity.Data.Balance where

import Prelude (class Show, class Eq, bind, pure, ($), (<#>))

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.DateTime (DateTime)
import Data.Maybe (maybe)
import Data.Result (Result(Ok, Error), toEither, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (sequence)
import Transity.Data.Amount (parseAmount)
import Transity.Data.CommodityMap (CommodityMap, fromAmounts)
import Transity.Utils (getObjField, stringToDateTime)


data Balance = Balance DateTime CommodityMap


derive instance genericBalance :: Generic Balance _
derive instance eqBalance :: Eq Balance

instance showBalance :: Show Balance where
  show = genericShow

instance decodeBalance :: DecodeJson Balance where
  decodeJson json = toEither $ decodeJsonBalance json


decodeJsonBalance :: Json -> Result String Balance
decodeJsonBalance json = do
  object  <- maybe (Error "Balance is not an object") Ok (toObject json)
  utc     <- object `getObjField` "utc"
  (amounts :: Array String) <- object `getObjField` "amounts"

  amountList <- sequence $ amounts <#> parseAmount

  utcResult <- note "UTC string is invalid" $ stringToDateTime utc
  let balance = Balance utcResult (fromAmounts amountList)
  pure balance

