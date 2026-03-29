use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::data::amount::Amount;
use crate::data::commodity_map::{add_amount_to_map, CommodityMap, commodity_map_zero};

/// A balance snapshot: a point-in-time record of amounts held.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Balance {
    pub utc: DateTime<Utc>,
    /// The list of amounts in the YAML; converted to a CommodityMap on the fly.
    #[serde(default)]
    pub amounts: Vec<Amount>,
}

impl Balance {
    pub fn to_commodity_map(&self) -> CommodityMap {
        let mut map = commodity_map_zero();
        for amount in &self.amounts {
            add_amount_to_map(&mut map, amount);
        }
        map
    }
}
