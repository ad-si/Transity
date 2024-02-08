module Transity.Data.Account where

import Prelude (class Eq, class Show, bind, max, pure, ($), (+), (<>), (==))

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Data.Argonaut.Core (Json, fromString, toObject)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype)
import Data.Result (Result(Ok, Error), toEither)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Text.Format (format, width)
import Transity.Data.Amount (Amount)
import Transity.Data.Balance (Balance)
import Transity.Data.CommodityMap
  ( CommodityMap
  , commodityMapZero
  , addAmountToMap
  , subtractAmountFromMap
  )
import Transity.Data.CommodityMap as CommodityMap
import Transity.Data.Config (ColorFlag(..))
import Transity.Utils
  ( WidthRecord
  , widthRecordZero
  , indentSubsequent
  , getObjField
  , getFieldMaybe
  , resultWithJsonDecodeError
  )

-- | A physical account which can contain one or several commodities.
-- | E.g. wallet, bank account, barn.
-- | You shouldn't misuse this for abstract concepts like expenses or income.
-- | To assign a meaning to a transaction you can use notes and tags.
-- |
-- | The Id must be a `:` separated String where the first element
-- | is the owner of the account. E.g. `john:evil-bank:savings`

type Id = String
newtype Account = Account
  { id :: Id
  , commodityMap :: CommodityMap
  , balances :: Maybe (Array Balance)
  }

derive instance genericAccount :: Generic Account _
derive instance newtypeAccount :: Newtype Account _
derive newtype instance eqAccount :: Eq Account

instance showAccount :: Show Account where
  show = genericShow

instance encodeAccount :: EncodeJson Account where
  encodeJson (Account accRec) =
    A.fromObject $ Object.fromFoldable
      [ Tuple "id" (fromString accRec.id)
      , Tuple "commodityMap" $ encodeJson accRec.commodityMap
      , Tuple "balances" $ encodeJson accRec.balances
      ]

instance decodeAccount :: DecodeJson Account where
  decodeJson json =
    toEither $ resultWithJsonDecodeError $ decodeJsonAccount json

decodeJsonAccount :: Json -> Result String Account
decodeJsonAccount json = do
  object <- maybe
    (Error $ "Account is not an object")
    Ok
    (toObject json)
  id <- object `getObjField` "id"
  commodityMap <- object `getFieldMaybe` "commodityMap"
  balances <- object `getFieldMaybe` "balances"
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
  let
    newMap = account.commodityMap `addAmountToMap` amount
  in
    Account account { commodityMap = newMap }

subtractAmount :: Account -> Amount -> Account
subtractAmount (Account account) amount =
  let
    newMap = account.commodityMap `subtractAmountFromMap` amount
  in
    Account account { commodityMap = newMap }

toWidthRecord :: Id -> CommodityMap -> WidthRecord
toWidthRecord accountId commodityMap =
  let
    widthRecord = CommodityMap.toWidthRecord commodityMap
  in
    widthRecord
      { account = max (length accountId) widthRecord.account
      }

showPretty :: Id -> CommodityMap -> String
showPretty = showPrettyAligned ColorNo widthRecordZero

showPrettyAligned :: ColorFlag -> WidthRecord -> Id -> CommodityMap -> String
showPrettyAligned colorFlag widthRec accountId commodityMap =
  let
    gap = 2
    accountWidth = max widthRec.account (length accountId)
    accName = format (width accountWidth) accountId
    accColor =
      if colorFlag == ColorYes then foreground Blue
      else foreground White
  in
    -- TODO: Fix after https://github.com/hdgarrood/purescript-ansi/issues/7
    ( if colorFlag == ColorYes then withGraphics accColor accName
      else accName
    )
      <> " " `power` gap
      <> indentSubsequent (accountWidth + gap)
        ( CommodityMap.showPrettyAligned
            colorFlag
            widthRec.integer
            widthRec.fraction
            widthRec.commodity
            commodityMap
        )
      <> "\n"
