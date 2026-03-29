use std::collections::BTreeMap;

use num_rational::BigRational;
use num_traits::Zero;

use crate::data::amount::Amount;
use crate::data::commodity::Commodity;

pub type CommodityMap = BTreeMap<Commodity, Amount>;

fn get_or_zero<'a>(map: &'a mut CommodityMap, commodity: &Commodity) -> &'a mut Amount {
    map.entry(commodity.clone())
        .or_insert_with(|| Amount::new(BigRational::zero(), commodity.clone()))
}

pub fn add_amount(map: &mut CommodityMap, amount: &Amount) {
    get_or_zero(map, &amount.commodity).quantity += &amount.quantity;
}

pub fn subtract_amount(map: &mut CommodityMap, amount: &Amount) {
    get_or_zero(map, &amount.commodity).quantity -= &amount.quantity;
}
