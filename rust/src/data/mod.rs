use chrono::NaiveDateTime;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{One, ToPrimitive, Zero};
use serde::Deserialize;

// ── Amount ────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount {
    pub quantity: BigRational,
    pub commodity: String,
}

impl Amount {
    pub fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }

    pub fn negate(&self) -> Amount {
        Amount {
            quantity: -self.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }
}

/// Format a BigRational as a float string matching PureScript's
/// `show (Rational.toNumber qty)`.
///
/// Rules:
/// - Whole numbers always get `.0` suffix  (`500.0`, `0.0`, `-1.0`)
/// - Decimals are printed with minimum digits needed  (`33.95`, `0.434114`)
pub fn format_quantity(qty: &BigRational) -> String {
    let f = big_rational_to_f64(qty);
    format_f64(f)
}

pub fn big_rational_to_f64(r: &BigRational) -> f64 {
    r.numer().to_f64().unwrap_or(0.0) / r.denom().to_f64().unwrap_or(1.0)
}

/// Format f64 like JavaScript/PureScript `show`:
/// whole numbers get a `.0` suffix; decimals use the shortest representation.
pub fn format_f64(f: f64) -> String {
    if f.fract() == 0.0 {
        format!("{:.1}", f)
    } else {
        format!("{}", f)
    }
}

// ── Ledger YAML model ─────────────────────────────────────────────────────────

/// A parsed amount string like "500 €" or "0.434114 BTC".
fn deserialize_amount<'de, D>(deserializer: D) -> Result<Amount, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    parse_amount_str(&s).map_err(serde::de::Error::custom)
}

pub fn parse_amount_str(s: &str) -> Result<Amount, String> {
    let parts: Vec<&str> = s.splitn(2, ' ').collect();
    if parts.len() != 2 {
        return Err(format!("Invalid amount: {}", s));
    }
    let qty = parse_rational(parts[0])?;
    Ok(Amount {
        quantity: qty,
        commodity: parts[1].to_string(),
    })
}

fn parse_rational(s: &str) -> Result<BigRational, String> {
    if let Some(dot_pos) = s.find('.') {
        let int_part = &s[..dot_pos];
        let frac_part = &s[dot_pos + 1..];
        let decimals = frac_part.len() as u32;
        let denom = BigInt::from(10u64).pow(decimals);

        let negative = int_part.starts_with('-');
        let int_abs: BigInt = int_part
            .trim_start_matches('-')
            .parse()
            .map_err(|e| format!("parse int: {}", e))?;
        let frac: BigInt = frac_part
            .parse()
            .map_err(|e| format!("parse frac: {}", e))?;

        let numer = if negative {
            -(int_abs * &denom + frac)
        } else {
            int_abs * &denom + frac
        };
        Ok(BigRational::new(numer, denom))
    } else {
        let n: BigInt = s.parse().map_err(|e| format!("parse int: {}", e))?;
        Ok(BigRational::new(n, BigInt::one()))
    }
}

// ── YAML deserialization helpers ──────────────────────────────────────────────

fn parse_datetime(s: &str) -> Option<NaiveDateTime> {
    for fmt in &["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"] {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(dt);
        }
    }
    chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d")
        .ok()
        .and_then(|d| d.and_hms_opt(0, 0, 0))
}

fn de_datetime_opt<'de, D>(deserializer: D) -> Result<Option<NaiveDateTime>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let opt: Option<String> = Option::deserialize(deserializer)?;
    Ok(opt.and_then(|s| parse_datetime(&s)))
}

fn de_datetime<'de, D>(deserializer: D) -> Result<Option<NaiveDateTime>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    Ok(parse_datetime(&s))
}

fn de_amounts<'de, D>(deserializer: D) -> Result<Vec<Amount>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let strings: Vec<String> = Vec::deserialize(deserializer)?;
    strings
        .into_iter()
        .map(|s| parse_amount_str(&s).map_err(serde::de::Error::custom))
        .collect()
}

// ── Data structures ───────────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize)]
pub struct Balance {
    #[serde(deserialize_with = "de_datetime")]
    pub utc: Option<NaiveDateTime>,
    #[serde(default, deserialize_with = "de_amounts")]
    pub amounts: Vec<Amount>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Account {
    pub id: String,
    #[serde(default)]
    pub balances: Vec<Balance>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Entity {
    pub id: String,
    #[serde(default)]
    pub accounts: Vec<Account>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Transfer {
    #[serde(default, deserialize_with = "de_datetime_opt")]
    pub utc: Option<NaiveDateTime>,
    pub from: String,
    pub to: String,
    #[serde(deserialize_with = "deserialize_amount")]
    pub amount: Amount,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Transaction {
    #[serde(default, deserialize_with = "de_datetime_opt")]
    pub utc: Option<NaiveDateTime>,
    pub transfers: Vec<Transfer>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Ledger {
    #[serde(default)]
    pub entities: Vec<Entity>,
    #[serde(default)]
    pub transactions: Vec<Transaction>,
}

impl Ledger {
    pub fn from_yaml(yaml: &str) -> Result<Ledger, String> {
        serde_yaml::from_str(yaml).map_err(|e| e.to_string())
    }
}
