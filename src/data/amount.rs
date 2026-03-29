use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Zero;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::data::commodity::Commodity;

/// An amount: a rational quantity paired with a commodity.
/// E.g. `20 €`, `10 cows`, `30 minutes`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Amount {
    pub quantity: BigRational,
    pub commodity: Commodity,
}

impl Amount {
    pub fn new(quantity: BigRational, commodity: Commodity) -> Self {
        Amount { quantity, commodity }
    }

    pub fn zero(commodity: Commodity) -> Self {
        Amount { quantity: BigRational::zero(), commodity }
    }

    pub fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }

    pub fn negate(&self) -> Self {
        Amount { quantity: -self.quantity.clone(), commodity: self.commodity.clone() }
    }

    pub fn parse(s: &str) -> anyhow::Result<Self> {
        let parts: Vec<&str> = s.splitn(2, ' ').collect();
        if parts.len() != 2 {
            anyhow::bail!("Amount does not contain a value and a commodity: {:?}", s);
        }
        let quantity = parse_rational(parts[0])
            .ok_or_else(|| anyhow::anyhow!("Invalid quantity: {:?}", parts[0]))?;
        let commodity = Commodity::new(parts[1].trim());
        Ok(Amount { quantity, commodity })
    }

    pub fn to_width_record(&self) -> WidthRecord {
        let (int_part, frac_part) = length_of_num_parts(&self.quantity);
        WidthRecord {
            account: 0,
            integer: int_part,
            fraction: frac_part,
            commodity: self.commodity.as_str().len(),
        }
    }

    pub fn show_pretty(&self) -> String {
        self.show_pretty_aligned(0, 0, 0)
    }

    pub fn show_pretty_aligned(&self, int_wid: usize, frac_wid: usize, com_wid: usize) -> String {
        let num_str = rational_to_display_string(&self.quantity);
        let parts: Vec<&str> = num_str.splitn(2, '.').collect();
        let int_part = format!("{:>width$}", parts[0], width = int_wid);
        let frac_part = if parts.len() > 1 && parts[1] != "0" {
            format!("{:<width$}", format!(".{}", parts[1]), width = frac_wid)
        } else {
            " ".repeat(frac_wid)
        };
        let com_part = format!("{:<width$}", self.commodity.as_str(), width = com_wid);
        format!("{}{} {}", int_part, frac_part, com_part)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct WidthRecord {
    pub account: usize,
    pub integer: usize,
    pub fraction: usize,
    pub commodity: usize,
}

impl WidthRecord {
    pub fn merge(&self, other: &WidthRecord) -> WidthRecord {
        WidthRecord {
            account: self.account.max(other.account),
            integer: self.integer.max(other.integer),
            fraction: self.fraction.max(other.fraction),
            commodity: self.commodity.max(other.commodity),
        }
    }
}

pub fn parse_rational(s: &str) -> Option<BigRational> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    // Validate: only digits, '.', '-', '+'
    if !s.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == '+') {
        return None;
    }
    if let Some(dot_pos) = s.find('.') {
        let digits_str = s.replace('.', "");
        let num_str_len = digits_str.len();
        let frac_digits = num_str_len - dot_pos;
        let numerator = digits_str.parse::<BigInt>().ok()?;
        let denominator = BigInt::from(10u32).pow(frac_digits as u32);
        Some(BigRational::new(numerator, denominator))
    } else {
        let n = s.parse::<BigInt>().ok()?;
        Some(BigRational::new(n, BigInt::from(1u32)))
    }
}

fn rational_to_display_string(r: &BigRational) -> String {
    use num_traits::ToPrimitive;
    if let Some(f) = r.to_f64() {
        let s = format!("{}", f);
        s
    } else {
        format!("{}/{}", r.numer(), r.denom())
    }
}

fn length_of_num_parts(r: &BigRational) -> (usize, usize) {
    let s = rational_to_display_string(r);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_len = parts[0].len();
    let frac_len = if parts.len() > 1 && parts[1] != "0" {
        parts[1].len() + 1 // +1 for the decimal point
    } else {
        0
    };
    (int_len, frac_len)
}

impl Serialize for Amount {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let s = format!("{} {}", rational_to_display_string(&self.quantity), self.commodity);
        serializer.serialize_str(&s)
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Amount::parse(&s).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_integer_amount() {
        let a = Amount::parse("20 €").unwrap();
        assert_eq!(a.commodity, Commodity::new("€"));
        assert_eq!(a.quantity, BigRational::new(BigInt::from(20), BigInt::from(1)));
    }

    #[test]
    fn parse_decimal_amount() {
        let a = Amount::parse("5.5 USD").unwrap();
        assert_eq!(a.commodity, Commodity::new("USD"));
        // 5.5 = 55/10 = 11/2
        assert_eq!(a.quantity, BigRational::new(BigInt::from(11), BigInt::from(2)));
    }

    #[test]
    fn parse_negative_amount() {
        let a = Amount::parse("-3 BTC").unwrap();
        assert_eq!(a.quantity, BigRational::new(BigInt::from(-3), BigInt::from(1)));
    }
}
