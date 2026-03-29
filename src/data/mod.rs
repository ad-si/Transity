use chrono::{NaiveDate, NaiveDateTime};
use num_rational::Rational64;
use num_traits::ToPrimitive;
use serde::{Deserialize, Deserializer};

#[derive(Debug, Clone, PartialEq)]
pub struct Amount {
    pub quantity: Rational64,
    pub commodity: String,
}

impl Amount {
    pub fn negate(&self) -> Amount {
        Amount {
            quantity: -self.quantity,
            commodity: self.commodity.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.quantity == Rational64::new(0, 1)
    }

    pub fn to_f64(&self) -> f64 {
        self.quantity.to_f64().unwrap_or(0.0)
    }
}

fn parse_amount(s: &str) -> Result<Amount, String> {
    let parts: Vec<&str> = s.splitn(2, ' ').collect();
    if parts.len() != 2 {
        return Err(format!("Invalid amount: {}", s));
    }
    let qty_str = parts[0].trim();
    let commodity = parts[1].trim().to_string();

    // Parse as decimal — convert to rational
    let quantity = parse_rational(qty_str)
        .ok_or_else(|| format!("Invalid quantity: {}", qty_str))?;

    Ok(Amount { quantity, commodity })
}

fn parse_rational(s: &str) -> Option<Rational64> {
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        let without_dot = s.replace('.', "");
        let numer: i64 = without_dot.parse().ok()?;
        let denom: i64 = 10_i64.pow(decimals as u32);
        Some(Rational64::new(numer, denom))
    } else {
        let numer: i64 = s.parse().ok()?;
        Some(Rational64::new(numer, 1))
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        parse_amount(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Balance {
    pub utc: String,
    pub amounts: Vec<Amount>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Account {
    pub id: String,
    pub name: Option<String>,
    pub note: Option<String>,
    pub balances: Option<Vec<Balance>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Entity {
    pub id: String,
    pub name: Option<String>,
    pub note: Option<String>,
    pub utc: Option<String>,
    pub accounts: Option<Vec<Account>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Transfer {
    pub utc: Option<String>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    pub note: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Transaction {
    pub id: Option<String>,
    pub utc: Option<String>,
    pub note: Option<String>,
    #[serde(default)]
    pub files: Vec<String>,
    pub transfers: Vec<Transfer>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Ledger {
    pub owner: Option<String>,
    pub entities: Option<Vec<Entity>>,
    pub transactions: Vec<Transaction>,
}

impl Ledger {
    pub fn from_yaml(yaml: &str) -> anyhow::Result<Ledger> {
        let ledger: Ledger = serde_yaml::from_str(yaml)?;
        Ok(ledger)
    }
}

/// Returns the initial entity balance transfers — zero-amount rows representing
/// entity account initialization points (matching PureScript's
/// `entitiesToInitialTransfers`).
pub fn entities_to_initial_transfers(entities: &Option<Vec<Entity>>) -> Vec<Transfer> {
    let Some(entities) = entities else {
        return vec![];
    };

    let mut result = Vec::new();
    for entity in entities {
        let Some(accounts) = &entity.accounts else {
            continue;
        };
        for account in accounts {
            let account_id = format!("{}:{}", entity.id, account.id);
            let Some(balances) = &account.balances else {
                continue;
            };
            for balance in balances {
                for amount in &balance.amounts {
                    if amount.is_zero() {
                        result.push(Transfer {
                            utc: Some(balance.utc.clone()),
                            from: account_id.clone(),
                            to: "_void_".to_string(),
                            amount: amount.clone(),
                            note: None,
                        });
                    }
                }
            }
        }
    }
    result
}

/// Parse a UTC string from the YAML (e.g. "2017-02-17" or "2017-02-17 10:00")
/// and return a normalised ISO datetime string without the Z suffix.
pub fn utc_to_iso_string(utc: &str) -> String {
    for fmt in &["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"] {
        if let Ok(dt) = NaiveDateTime::parse_from_str(utc, fmt) {
            return dt.format("%Y-%m-%dT%H:%M:%S").to_string();
        }
    }
    if let Ok(d) = NaiveDate::parse_from_str(utc, "%Y-%m-%d") {
        return d.format("%Y-%m-%dT00:00:00").to_string();
    }
    utc.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_amount_euro() {
        let a = parse_amount("42.50 €").unwrap();
        assert_eq!(a.commodity, "€");
        assert!((a.to_f64() - 42.5).abs() < 1e-9);
    }

    #[test]
    fn utc_string_full() {
        let s = utc_to_iso_string("2018-01-04 17:05");
        assert_eq!(s, "2018-01-04T17:05:00");
    }

    #[test]
    fn utc_string_date_only() {
        let s = utc_to_iso_string("2017-04-22");
        assert_eq!(s, "2017-04-22T00:00:00");
    }
}
