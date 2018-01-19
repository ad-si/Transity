module Transity.Data.Account
where

import Data.Functor (map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (power)
import Data.Ord (max)
import Data.Ring ((+))
import Data.Semigroup ((<>))
import Data.String (joinWith, length)
import Data.Tuple (Tuple(Tuple))
import Prelude ((#))
import Text.Format (format, width)
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount as Amount
import Transity.Data.CommodityMap
  (CommodityMap, addAmountToMap, subtractAmountFromMap)
import Transity.Data.CommodityMap as CommodityMap
import Transity.Utils
  ( indentSubsequent
  , WidthRecord
  , widthRecordZero
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
showPretty (Account accountId commodityMap) =
  let
    accPretty = accountId <> "  "
    accLength = length accPretty
  in
    accPretty
    <> indentSubsequent accLength (CommodityMap.showPretty commodityMap)
    <> "\n"


showPrettyAligned :: Int -> Int -> Int -> Int -> Account -> String
showPrettyAligned accountW intW fracW comW (Account accountId commodityMap) =
  let
    gap = 2
  in
    format (width accountW) accountId
    <> " " `power` gap
    <> indentSubsequent (accountW + gap)
        (CommodityMap.showPrettyAligned intW fracW comW commodityMap)
    <> "\n"
