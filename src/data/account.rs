use serde::{Deserialize, Serialize};

use crate::data::amount::{Amount, WidthRecord};
use crate::data::balance::Balance;
use crate::data::commodity_map::{
    self, CommodityMap, add_amount_to_map, commodity_map_zero, subtract_amount_from_map,
    to_width_record,
};
use crate::data::config::ColorFlag;
use crate::utils::indent_subsequent;

/// A physical account that can hold one or more commodities.
/// The `id` is a colon-separated path, e.g. `john:evil-bank:savings`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Account {
    pub id: String,
    #[serde(default)]
    pub commodity_map: CommodityMap,
    pub balances: Option<Vec<Balance>>,
}

impl Account {
    pub fn new(id: impl Into<String>) -> Self {
        Account {
            id: id.into(),
            commodity_map: commodity_map_zero(),
            balances: None,
        }
    }

    pub fn add_amount(&mut self, amount: &Amount) {
        add_amount_to_map(&mut self.commodity_map, amount);
    }

    pub fn subtract_amount(&mut self, amount: &Amount) {
        subtract_amount_from_map(&mut self.commodity_map, amount);
    }

    pub fn to_width_record(&self) -> WidthRecord {
        let map_rec = to_width_record(&self.commodity_map);
        WidthRecord {
            account: self.id.len().max(map_rec.account),
            ..map_rec
        }
    }

    pub fn show_pretty(&self) -> String {
        show_pretty_aligned(ColorFlag::ColorNo, &WidthRecord::default(), &self.id, &self.commodity_map)
    }

    pub fn show_pretty_aligned(&self, color: ColorFlag, width_rec: &WidthRecord) -> String {
        show_pretty_aligned(color, width_rec, &self.id, &self.commodity_map)
    }
}

fn show_pretty_aligned(
    color: ColorFlag,
    width_rec: &WidthRecord,
    account_id: &str,
    commodity_map: &CommodityMap,
) -> String {
    const GAP: usize = 2;
    let account_width = width_rec.account.max(account_id.len());
    let acc_name = format!("{:>width$}", account_id, width = account_width);

    let acc_name_colored = match color {
        ColorFlag::ColorYes => {
            use colored::Colorize;
            acc_name.blue().to_string()
        }
        ColorFlag::ColorNo => acc_name,
    };

    let commodity_str = commodity_map::show_pretty_aligned(
        color,
        width_rec.integer,
        width_rec.fraction,
        width_rec.commodity,
        commodity_map,
    );

    format!(
        "{}{}{}\n",
        acc_name_colored,
        " ".repeat(GAP),
        indent_subsequent(account_width + GAP, &commodity_str)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::amount::Amount;
    use num_bigint::BigInt;
    use num_rational::BigRational;
    use crate::data::commodity::Commodity;

    fn make_amount(qty: i64, com: &str) -> Amount {
        Amount::new(
            BigRational::new(BigInt::from(qty), BigInt::from(1)),
            Commodity::new(com),
        )
    }

    #[test]
    fn add_and_subtract_amount() {
        let mut acc = Account::new("wallet");
        acc.add_amount(&make_amount(10, "€"));
        acc.add_amount(&make_amount(5, "€"));
        assert_eq!(
            acc.commodity_map[&Commodity::new("€")].quantity,
            BigRational::new(BigInt::from(15), BigInt::from(1))
        );

        acc.subtract_amount(&make_amount(3, "€"));
        assert_eq!(
            acc.commodity_map[&Commodity::new("€")].quantity,
            BigRational::new(BigInt::from(12), BigInt::from(1))
        );
    }

    #[test]
    fn to_width_record_uses_max() {
        let mut acc = Account::new("a_long_account_name");
        acc.add_amount(&make_amount(100, "€"));
        let wr = acc.to_width_record();
        assert!(wr.account >= "a_long_account_name".len());
    }

    #[test]
    fn show_pretty_produces_string() {
        let mut acc = Account::new("wallet");
        acc.add_amount(&make_amount(42, "€"));
        let s = acc.show_pretty();
        assert!(s.contains("wallet"));
        assert!(s.contains("42"));
        assert!(s.contains("€"));
    }

    #[test]
    fn deserialize_from_yaml() {
        let yaml = r#"
id: wallet
balances:
  - utc: '2015-04-02T20:11:45Z'
    amounts:
      - "10 €"
"#;
        let acc: Account = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(acc.id, "wallet");
        assert!(acc.balances.is_some());
        assert_eq!(acc.balances.unwrap().len(), 1);
    }
}
