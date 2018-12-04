module Transity.Data.Account where

import Prelude (class Show, bind, max, pure, show, ($), (+), (<>), (<#>), (==), discard)

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Map (fromFoldable)
import Data.Monoid (power)
import Data.Result (Result(Ok, Error), toEither, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (length)
import Data.Traversable (sequence)
import Debug.Trace
import Text.Format (format, width)
import Transity.Data.Amount (Amount, parseAmount)
import Transity.Data.CommodityMap
  ( CommodityMap
  , commodityMapZero
  , fromAmounts
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
  , stringToDateTime
  , ColorFlag(..)
  )


data Balance = Balance DateTime CommodityMap

derive instance genericBalance :: Generic Balance _

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


--| A physical account which can contain one or several commodities.
--| E.g. wallet, bank account, barn.
--| You shouldn't misuse this for abstract concepts like expenses or income.
--| To assign a meaning to a transaction you can use notes and tags.
--|
--| The Id must be a `:` separated String where the first element
--| is the owner of the account. E.g. `john:evil-bank:savings`

type Id = String
newtype Account = Account
  { id :: Id
  , commodityMap :: CommodityMap
  , balances :: Maybe (Array Balance)
  }

derive instance genericAccount :: Generic Account _

instance showAccount :: Show Account where
  show = genericShow

instance decodeAccount :: DecodeJson Account where
  decodeJson json = toEither $ decodeJsonAccount json

decodeJsonAccount :: Json -> Result String Account
decodeJsonAccount json = do
  object       <- maybe (Error "Account is not an object") Ok (toObject json)
  id           <- object `getObjField` "id"
  commodityMap <- object `getFieldMaybe` "commodityMap"
  balances     <- object `getFieldMaybe` "balances"
  pure $ Account
    { id
    , commodityMap: (fromMaybe commodityMapZero commodityMap)
    , balances
    }

zero :: Account
zero = Account
  { id: ""
  , commodityMap: commodityMapZero
  , balances: Nothing
  }


addAmount :: Account -> Amount -> Account
addAmount (Account account) amount =
  let newMap = account.commodityMap `addAmountToMap` amount
  in Account account {commodityMap = newMap}


subtractAmount :: Account -> Amount -> Account
subtractAmount (Account account) amount =
  let newMap = account.commodityMap `subtractAmountFromMap` amount
  in Account account {commodityMap = newMap}


toWidthRecord :: Account -> WidthRecord
toWidthRecord (Account account) =
  let
    widthRecord = CommodityMap.toWidthRecord account.commodityMap
  in
    widthRecord {
      account = max (length account.id) widthRecord.account
    }


showPretty :: Account -> String
showPretty = showPrettyAligned ColorNo widthRecordZero


showPrettyAligned :: ColorFlag -> WidthRecord -> Account -> String
showPrettyAligned colorFlag widthRec (Account account) =
  let
    gap = 2
    accountWidth = max widthRec.account (length account.id)
    accName = format (width accountWidth) account.id
    accColor = if colorFlag == ColorYes
      then foreground Blue
      else foreground White
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
          account.commodityMap)
    <> "\n"
