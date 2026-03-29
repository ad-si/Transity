use std::collections::BTreeMap;
use crate::data::amount::{Amount, WidthRecord};
use crate::data::commodity::Commodity;
use crate::data::config::ColorFlag;

/// A map from commodity to amount (one entry per commodity).
pub type CommodityMap = BTreeMap<Commodity, Amount>;

pub fn commodity_map_zero() -> CommodityMap {
    BTreeMap::new()
}

pub fn add_amount_to_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| {
            existing.quantity = existing.quantity.clone() + amount.quantity.clone();
        })
        .or_insert_with(|| amount.clone());
}

pub fn subtract_amount_from_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| {
            existing.quantity = existing.quantity.clone() - amount.quantity.clone();
        })
        .or_insert_with(|| amount.negate());
}

pub fn is_commodity_map_zero(map: &CommodityMap) -> bool {
    map.values().all(|a| a.is_zero())
}

pub fn to_width_record(map: &CommodityMap) -> WidthRecord {
    map.values()
        .map(|a| a.to_width_record())
        .fold(WidthRecord::default(), |acc, w| acc.merge(&w))
}

pub fn show_pretty(map: &CommodityMap) -> String {
    show_pretty_aligned(ColorFlag::ColorNo, 0, 0, 0, map)
}

pub fn show_pretty_aligned(
    _color: ColorFlag,
    int_width: usize,
    frac_width: usize,
    com_width: usize,
    map: &CommodityMap,
) -> String {
    map.values()
        .map(|a| a.show_pretty_aligned(int_width, frac_width, com_width))
        .collect::<Vec<_>>()
        .join("\n")
}
