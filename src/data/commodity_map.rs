use std::collections::BTreeMap;

use crate::data::amount::Amount;
use crate::data::commodity::Commodity;
use crate::data::config::ColorFlag;
use crate::utils::{merge_width_records, WidthRecord};

pub type CommodityMap = BTreeMap<Commodity, Amount>;

pub fn add_amount_to_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| *existing += amount.clone())
        .or_insert_with(|| amount.clone());
}

pub fn subtract_amount_from_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| {
            // commodities match by construction (keyed on amount.commodity)
            *existing = existing.subtract(amount).expect("commodity mismatch in map");
        })
        .or_insert_with(|| amount.negate());
}

pub fn is_commodity_map_zero(map: &CommodityMap) -> bool {
    map.values().all(|a| a.is_zero())
}

pub fn is_commodity_zero(map: &CommodityMap, commodity: &Commodity) -> bool {
    map.get(commodity).map(|a| a.is_zero()).unwrap_or(false)
}

pub fn show_pretty_aligned(
    map: &CommodityMap,
    color: ColorFlag,
    int_w: usize,
    frac_w: usize,
    com_w: usize,
) -> String {
    map.values()
        .map(|amount| amount.show_pretty_aligned(color, int_w, frac_w, com_w))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn to_width_record(map: &CommodityMap) -> WidthRecord {
    map.values()
        .map(|a| a.to_width_record())
        .fold(WidthRecord::default(), merge_width_records)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_commodity_map_add() {
        let mut map = CommodityMap::new();
        let a = Amount::parse("10 €").unwrap();
        let b = Amount::parse("5 €").unwrap();
        add_amount_to_map(&mut map, &a);
        add_amount_to_map(&mut map, &b);
        let result = map.get(&Commodity::new("€")).unwrap();
        assert!(!result.is_zero());
        assert_eq!(result.show_pretty(), "15 €");
    }

    #[test]
    fn test_commodity_map_subtract() {
        let mut map = CommodityMap::new();
        let a = Amount::parse("10 €").unwrap();
        add_amount_to_map(&mut map, &a);
        subtract_amount_from_map(&mut map, &a);
        let result = map.get(&Commodity::new("€")).unwrap();
        assert!(result.is_zero());
    }

    #[test]
    fn test_is_commodity_map_zero() {
        let mut map = CommodityMap::new();
        assert!(is_commodity_map_zero(&map));
        let a = Amount::parse("10 €").unwrap();
        add_amount_to_map(&mut map, &a);
        assert!(!is_commodity_map_zero(&map));
    }

    #[test]
    fn test_is_commodity_zero() {
        let mut map = CommodityMap::new();
        let com = Commodity::new("€");
        assert!(!is_commodity_zero(&map, &com)); // missing → false
        let a = Amount::parse("0 €").unwrap();
        add_amount_to_map(&mut map, &a);
        assert!(is_commodity_zero(&map, &com));
    }
}
