module Transity.Data.CommodityMap
where

import Data.Array (fromFoldable)
import Data.Foldable (foldr, all)
import Data.Functor (map, (<#>))
import Data.Function (flip, ($))
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Semigroup ((<>))
-- import Data.String (Pattern(..), joinWith, split, length)
import Data.String (joinWith)
-- import Data.Show (show)
import Data.Tuple (Tuple(..), snd)
import Prelude ((#))
import Transity.Data.Amount (Amount(..), Commodity(..))
import Transity.Data.Amount as Amount
import Transity.Utils
  ( WidthRecord
  , widthRecordZero
  , mergeWidthRecords
  , ColorFlag(..)
  )


type CommodityMap = Map.Map Commodity Amount


commodityMapZero :: CommodityMap
commodityMapZero = Map.empty :: CommodityMap


fromAmounts :: Array Amount -> CommodityMap
fromAmounts =
  foldr (flip addAmountToMap) commodityMapZero


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


isCommodityMapZero :: CommodityMap -> Boolean
isCommodityMapZero comMap =
  (Map.values comMap)
  # fromFoldable
  # all Amount.isZero


isCommodityZero :: CommodityMap -> Commodity -> Boolean
isCommodityZero comMap commodity =
  let
    amountMaybe = Map.lookup commodity comMap
  in
    fromMaybe false $
      amountMaybe <#> Amount.isZero


-- TODO: Verify commodity map
--       by checking that the commodity of the key and the value match


showPretty :: CommodityMap -> String
showPretty = showPrettyAligned ColorNo 0 0 0


--| Specify the width (in characters) of the integer part,
--| the width of the fractional part
--| (both exluding the decimal point) and receive a pretty printed
--| multi line string.

showPrettyAligned :: ColorFlag -> Int -> Int -> Int -> CommodityMap -> String
showPrettyAligned colorFlag intWidth fracWidth comWidth commodityMap =
  commodityMap
    # (Map.toUnfoldable :: CommodityMap -> Array (Tuple Commodity Amount))
    # map (\(Tuple _ amount) ->
        Amount.showPrettyAligned colorFlag intWidth fracWidth comWidth amount)
    # joinWith "\n"


toWidthRecord :: CommodityMap -> WidthRecord
toWidthRecord commodityMap =
  commodityMap
    # (Map.toUnfoldable :: CommodityMap -> Array (Tuple Commodity Amount))
    # map snd
    # map Amount.toWidthRecord
    # foldr mergeWidthRecords widthRecordZero



