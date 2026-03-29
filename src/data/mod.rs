use std::collections::HashMap;

use chrono::NaiveDateTime;
use num_rational::Rational64;
use serde::{Deserialize, Deserializer};
use anyhow::{Result, bail};

/// Parse a string like "66.05 €" or "100 USD" into (quantity, commodity)
fn parse_amount_str(s: &str) -> Result<(Rational64, String)> {
    let parts: Vec<&str> = s.splitn(2, ' ').collect();
    if parts.len() != 2 {
        bail!("Invalid amount format: '{}'", s);
    }
    let quantity = parse_rational(parts[0])?;
    Ok((quantity, parts[1].to_string()))
}

fn parse_rational(s: &str) -> Result<Rational64> {
    if let Some(dot_pos) = s.find('.') {
        let decimal_places = s.len() - dot_pos - 1;
        let denom = 10i64.pow(decimal_places as u32);
        let without_dot = s.replace('.', "");
        let numer: i64 = without_dot.parse()?;
        Ok(Rational64::new(numer, denom))
    } else {
        let numer: i64 = s.parse()?;
        Ok(Rational64::new(numer, 1))
    }
}

/// A commodity (currency or asset type), e.g. "€", "USD", "BTC"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Commodity(pub String);

/// An amount with a commodity, e.g. 66.05 €
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount {
    pub quantity: Rational64,
    pub commodity: Commodity,
}

impl Amount {
    pub fn is_zero(&self) -> bool {
        self.quantity == Rational64::new(0, 1)
    }

    pub fn negate(&self) -> Amount {
        Amount {
            quantity: -self.quantity,
            commodity: self.commodity.clone(),
        }
    }

    pub fn show_pretty(&self) -> String {
        let n = *self.quantity.numer();
        let d = *self.quantity.denom();
        let value = n as f64 / d as f64;
        let s = if d == 1 {
            format!("{}", n)
        } else {
            // Find minimum decimal places needed to represent exactly
            let places = (0u32..=10)
                .find(|&p| (self.quantity * Rational64::new(10i64.pow(p), 1)).is_integer())
                .unwrap_or(10);
            format!("{:.prec$}", value, prec = places as usize)
        };
        format!("{} {}", s, self.commodity.0)
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        let (quantity, commodity) = parse_amount_str(&s)
            .map_err(serde::de::Error::custom)?;
        Ok(Amount { quantity, commodity: Commodity(commodity) })
    }
}

/// CommodityMap: maps commodity -> amount
pub type CommodityMap = HashMap<Commodity, Amount>;

pub fn add_amount_to_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| {
            existing.quantity += amount.quantity;
        })
        .or_insert_with(|| amount.clone());
}

pub fn subtract_amount_from_map(map: &mut CommodityMap, amount: &Amount) {
    map.entry(amount.commodity.clone())
        .and_modify(|existing| {
            existing.quantity -= amount.quantity;
        })
        .or_insert_with(|| amount.negate());
}

pub fn is_commodity_zero(map: &CommodityMap, commodity: &Commodity) -> bool {
    match map.get(commodity) {
        None => false,
        Some(amount) => amount.is_zero(),
    }
}

/// A balance snapshot at a point in time
#[derive(Debug, Clone, Deserialize)]
pub struct Balance {
    #[serde(deserialize_with = "deserialize_datetime")]
    pub utc: NaiveDateTime,
    pub amounts: Vec<Amount>,
}

fn deserialize_datetime<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<NaiveDateTime, D::Error> {
    let s = String::deserialize(deserializer)?;
    parse_datetime_str(&s).map_err(serde::de::Error::custom)
}

/// An account within an entity
#[derive(Debug, Clone, Deserialize)]
pub struct Account {
    pub id: String,
    pub balances: Option<Vec<Balance>>,
}

/// A transfer of an amount from one account to another
#[derive(Debug, Clone)]
pub struct Transfer {
    pub utc: Option<NaiveDateTime>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    pub note: Option<String>,
}

impl<'de> Deserialize<'de> for Transfer {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct TransferRaw {
            #[serde(default, deserialize_with = "deserialize_opt_datetime")]
            utc: Option<NaiveDateTime>,
            from: String,
            to: String,
            amount: Amount,
            note: Option<String>,
        }
        let raw = TransferRaw::deserialize(deserializer)?;
        Ok(Transfer {
            utc: raw.utc,
            from: raw.from,
            to: raw.to,
            amount: raw.amount,
            note: raw.note,
        })
    }
}

fn deserialize_opt_datetime<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Option<NaiveDateTime>, D::Error> {
    let opt: Option<String> = Option::deserialize(deserializer)?;
    match opt {
        None => Ok(None),
        Some(s) => parse_datetime_str(&s)
            .map(Some)
            .map_err(serde::de::Error::custom),
    }
}

fn parse_datetime_str(s: &str) -> Result<NaiveDateTime> {
    for fmt in &["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"] {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Ok(dt);
        }
    }
    // Date-only strings are treated as midnight
    if let Ok(date) = chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        return Ok(date.and_hms_opt(0, 0, 0).unwrap());
    }
    bail!("Cannot parse datetime: '{}'", s)
}

/// A transaction containing one or more transfers
#[derive(Debug, Clone, Deserialize)]
pub struct Transaction {
    #[serde(default, deserialize_with = "deserialize_opt_datetime")]
    pub utc: Option<NaiveDateTime>,
    pub note: Option<String>,
    pub transfers: Vec<Transfer>,
}

impl Transaction {
    /// Return transfers with UTC promoted from transaction if not set on transfer
    pub fn to_transfers(&self) -> Vec<Transfer> {
        self.transfers.iter().map(|t| Transfer {
            utc: t.utc.or(self.utc),
            from: t.from.clone(),
            to: t.to.clone(),
            amount: t.amount.clone(),
            note: t.note.clone(),
        }).collect()
    }
}

/// An entity (person, company, etc.) with optional accounts and balances
#[derive(Debug, Clone, Deserialize)]
pub struct Entity {
    pub id: String,
    pub accounts: Option<Vec<Account>>,
}

impl Entity {
    /// Convert entity balance snapshots into synthetic transfers (from account to _void_)
    pub fn to_transfers(&self) -> Vec<Transfer> {
        let accounts = match &self.accounts {
            None => return vec![],
            Some(a) => a,
        };

        let mut transfers = vec![];
        for account in accounts {
            let full_id = format!("{}:{}", self.id, account.id);
            let balances = match &account.balances {
                None => continue,
                Some(b) => b,
            };
            for balance in balances {
                let com_map: CommodityMap = {
                    let mut m = CommodityMap::new();
                    for amount in &balance.amounts {
                        add_amount_to_map(&mut m, amount);
                    }
                    m
                };
                for amount in com_map.values() {
                    transfers.push(Transfer {
                        utc: Some(balance.utc),
                        from: full_id.clone(),
                        to: "_void_".to_string(),
                        amount: amount.clone(),
                        note: None,
                    });
                }
            }
        }
        transfers
    }
}

/// The top-level ledger
#[derive(Debug, Clone, Deserialize)]
pub struct Ledger {
    pub owner: Option<String>,
    pub entities: Option<Vec<Entity>>,
    pub transactions: Vec<Transaction>,
}

impl Ledger {
    pub fn from_yaml(yaml: &str) -> Result<Ledger> {
        let ledger: Ledger = serde_yaml::from_str(yaml)?;
        Ok(ledger)
    }
}
