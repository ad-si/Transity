module Transity.Data.Account
  ( Account(Account)
  , Id
  , addAmountToMap
  , subtractAmountFromMap
  , addAmount
  , subtractAmount
  , showPretty

  , CommodityMap
  , commodityMapShowPretty
  )
where

import Data.Functor (map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Semigroup ((<>))
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Prelude ((#))
import Text.Format (format, width)
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount (subtract , negate , showPretty) as Amount
import Transity.Utils (indentSubsequent)


type Id = String
type CommodityMap = Map.Map Commodity Amount


addAmountToMap :: CommodityMap -> Amount -> CommodityMap
addAmountToMap commodityMap amount@(Amount value (Commodity commodity)) =
  Map.alter
    (\maybeValue -> case maybeValue of
      Nothing -> Just amount
      Just amountNow -> Just (amountNow <> amount)
    )
    (Commodity commodity)
    commodityMap


subtractAmountFromMap :: CommodityMap -> Amount -> CommodityMap
subtractAmountFromMap commodityMap amount@(Amount value (Commodity commodity)) =
  Map.alter
    (\maybeValue -> case maybeValue of
      Nothing -> Just (Amount.negate amount)
      Just amountNow -> Just (amountNow `Amount.subtract` amount)
    )
    (Commodity commodity)
    commodityMap


commodityMapShowPretty :: CommodityMap -> String
commodityMapShowPretty commodityMap =
  commodityMap
    # (Map.toAscUnfoldable :: CommodityMap -> Array (Tuple Commodity Amount))
    # map (\(Tuple _ amount) -> Amount.showPretty amount)
    # joinWith "\n"


data Account = Account Id CommodityMap


addAmount :: Account -> Amount -> Account
addAmount (Account id comMap) amount =
  let newMap = comMap `addAmountToMap` amount
  in Account id newMap


subtractAmount :: Account -> Amount -> Account
subtractAmount (Account id comMap) amount =
  let newMap = comMap `subtractAmountFromMap` amount
  in Account id newMap


showPretty :: Int -> Account -> String
showPretty indentation (Account accountId commodityMap) =
  format (width indentation) accountId
  <> indentSubsequent indentation (commodityMapShowPretty commodityMap)
  <> "\n"
