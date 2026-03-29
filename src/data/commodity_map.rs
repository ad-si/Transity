use std::collections::BTreeMap;
use crate::data::amount::{Amount, Commodity, WidthRecord, amount_to_width_record};

/// A map from commodity name to Amount, preserving insertion/key order (BTreeMap = sorted).
pub type CommodityMap = BTreeMap<Commodity, Amount>;

pub fn add_amount_to_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| *existing = existing.add(amount))
        .or_insert_with(|| amount.clone());
}

pub fn subtract_amount_from_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| *existing = existing.subtract(amount))
        .or_insert_with(|| amount.negate());
}

pub fn commodity_map_to_width_record(map: &CommodityMap) -> WidthRecord {
    map.values()
        .map(amount_to_width_record)
        .fold(WidthRecord::zero(), |acc, wr| acc.merge(&wr))
}

/// Show commodity map aligned, joining amounts with "\n".
pub fn show_commodity_map_aligned(
    int_w: usize,
    frac_w: usize,
    com_w: usize,
    map: &CommodityMap,
) -> String {
    use crate::data::amount::show_amount_aligned;
    map.values()
        .map(|amount| show_amount_aligned(int_w, frac_w, com_w, amount))
        .collect::<Vec<_>>()
        .join("\n")
}
