module Transity.Data.Account where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (power)
import Data.Result (Result(Ok, Error), toEither)
import Data.Generic.Rep (class Generic)
import Data.String (length)
import Text.Format (format, width)
import Transity.Data.Amount (Amount)
import Transity.Data.CommodityMap
  ( CommodityMap
  , commodityMapZero
  , addAmountToMap
  , subtractAmountFromMap
  )
import Transity.Data.CommodityMap as CommodityMap
import Transity.Utils
  ( WidthRecord
  , widthRecordZero
  , indentSubsequent
  , getObjField
  , getFieldMaybe
  , ColorFlag(..)
  )

--| A physical account which can contain one or several commodities.
--| E.g. wallet, bank account, barn.
--| You shouldn't misuse this for abstract concepts like expenses or income.
--| To assign a meaning to a transaction you can use notes and tags.
--|
--| The Id must be a `:` separated String where the first element
--| is the owner of the account. E.g. `john:evil-bank:savings`

type Id = String
data Account = Account Id CommodityMap

derive instance genericAccount :: Generic Account _

instance showAccount :: Show Account where
  show = showPretty

instance decodeAccount :: DecodeJson Account where
  decodeJson json = toEither $ decodeJsonAccount json

decodeJsonAccount :: Json -> Result String Account
decodeJsonAccount json = do
  object       <- maybe (Error "Account is not an object") Ok (toObject json)
  id           <- object `getObjField` "id"
  commodityMap <- object `getFieldMaybe` "commodityMap"
  pure $ Account id (fromMaybe commodityMapZero commodityMap)


addAmount :: Account -> Amount -> Account
addAmount (Account id comMap) amount =
  let newMap = comMap `addAmountToMap` amount
  in Account id newMap


subtractAmount :: Account -> Amount -> Account
subtractAmount (Account id comMap) amount =
  let newMap = comMap `subtractAmountFromMap` amount
  in Account id newMap


toWidthRecord :: Account -> WidthRecord
toWidthRecord (Account name commodityMap) =
  let
    widthRecord = CommodityMap.toWidthRecord commodityMap
  in
    widthRecord {
      account = max (length name) widthRecord.account
    }


showPretty :: Account -> String
showPretty = showPrettyAligned ColorNo widthRecordZero


showPrettyAligned :: ColorFlag -> WidthRecord -> Account -> String
showPrettyAligned colorFlag widthRec (Account accId comMap) =
  let
    gap = 2
    accountWidth = max widthRec.account (length accId)
    accName = format (width accountWidth) accId
    accColor = if colorFlag == ColorYes
      then foreground Blue
      else []
  in
    -- TODO: Fix after https://github.com/hdgarrood/purescript-ansi/issues/7
    (if colorFlag == ColorYes
        then withGraphics accColor accName
        else accName)
    <> " " `power` gap
    <> indentSubsequent (accountWidth + gap)
        (CommodityMap.showPrettyAligned
          colorFlag
          widthRec.integer
          widthRec.fraction
          widthRec.commodity
          comMap)
    <> "\n"
