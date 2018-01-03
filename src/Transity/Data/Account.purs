module Transity.Data.Account
  ( Account(Account)
  , AccountId
  , addAmountToMap
  , subtractAmountFromMap
  , addAmountToAccount
  , subtractAmountFromAccount

  , CommodityMap
  , prettyShowCommodityMap
  )
where

import Data.Functor (map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Semigroup ((<>))
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Prelude ((#))
import Transity.Data.Amount as Amount
import Transity.Data.Amount
  ( Amount(Amount)
  , Commodity(Commodity)
  , negateAmount
  , prettyShowAmount
  )


type AccountId = String
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
      Nothing -> Just (negateAmount amount)
      Just amountNow -> Just (amountNow `Amount.subtractAmount` amount)
    )
    (Commodity commodity)
    commodityMap


prettyShowCommodityMap :: CommodityMap -> String
prettyShowCommodityMap commodityMap =
  commodityMap
    # (Map.toUnfoldable :: CommodityMap -> Array (Tuple Commodity Amount))
    # map (\(Tuple _ amount) -> prettyShowAmount amount)
    # joinWith "\n"


data Account = Account AccountId CommodityMap


addAmountToAccount :: Account -> Amount -> Account
addAmountToAccount (Account id comMap) amount =
  let newMap = comMap `addAmountToMap` amount
  in Account id newMap


subtractAmountFromAccount :: Account -> Amount -> Account
subtractAmountFromAccount (Account id comMap) amount =
  let newMap = comMap `subtractAmountFromMap` amount
  in Account id newMap
