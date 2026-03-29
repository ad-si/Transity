use chrono::{DateTime, Utc};
use serde::{de, Deserialize, Deserializer, Serialize};

use crate::data::amount::Amount;
use crate::data::commodity_map::{add_amount_to_map, CommodityMap};

#[derive(Debug, Clone, Serialize)]
pub struct Balance {
    pub utc: DateTime<Utc>,
    pub commodity_map: CommodityMap,
}

impl Balance {
    pub fn new(utc: DateTime<Utc>, amounts: Vec<Amount>) -> Self {
        let mut commodity_map = CommodityMap::new();
        for amount in amounts {
            add_amount_to_map(&mut commodity_map, &amount);
        }
        Balance { utc, commodity_map }
    }
}

/// Intermediate struct for deserialization from YAML/JSON.
#[derive(Deserialize)]
struct BalanceRaw {
    utc: String,
    amounts: Vec<Amount>,
}

impl<'de> Deserialize<'de> for Balance {
    fn deserialize<D: Deserializer<'de>>(d: D) -> std::result::Result<Self, D::Error> {
        let raw = BalanceRaw::deserialize(d)?;
        let utc: DateTime<Utc> = raw.utc.parse().map_err(de::Error::custom)?;
        Ok(Balance::new(utc, raw.amounts))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balance_deserialize() {
        let yaml = r#"
utc: "2017-02-17T00:00:00Z"
amounts:
  - "200 €"
  - "1 evil_machine"
"#;
        let balance: Balance = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(balance.commodity_map.len(), 2);
        assert!(balance.commodity_map.contains_key(&crate::data::commodity::Commodity::new("€")));
        assert!(balance
            .commodity_map
            .contains_key(&crate::data::commodity::Commodity::new("evil_machine")));
    }

    #[test]
    fn test_balance_utc() {
        let yaml = r#"
utc: "2017-02-17T00:00:00Z"
amounts:
  - "100 USD"
"#;
        let balance: Balance = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(balance.utc.to_rfc3339(), "2017-02-17T00:00:00+00:00");
    }
}
