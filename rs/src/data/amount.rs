use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Zero;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::str::FromStr;

use crate::data::commodity::Commodity;

/// Amount = a rational quantity + a commodity, e.g. "15 €" or "0.5 BTC"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount {
    pub quantity: BigRational,
    pub commodity: Commodity,
}

impl Amount {
    pub fn new(quantity: BigRational, commodity: Commodity) -> Self {
        Amount { quantity, commodity }
    }

    pub fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }

    pub fn negate(&self) -> Self {
        Amount {
            quantity: -self.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }
}

impl FromStr for Amount {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.splitn(2, ' ').collect();
        if parts.len() != 2 {
            return Err(format!("Amount must be 'value commodity', got: {s:?}"));
        }
        let quantity = parse_rational(parts[0])
            .ok_or_else(|| format!("Invalid number in amount: {:?}", parts[0]))?;
        let commodity = Commodity::new(parts[1].trim());
        Ok(Amount { quantity, commodity })
    }
}

fn parse_rational(s: &str) -> Option<BigRational> {
    if let Some(dot_pos) = s.find('.') {
        let integer_part = &s[..dot_pos];
        let frac_part = &s[dot_pos + 1..];
        let decimals = frac_part.len() as u32;
        let denom = BigInt::from(10u64).pow(decimals);

        let sign = if integer_part.starts_with('-') { -1i64 } else { 1i64 };
        let abs_int = integer_part.trim_start_matches('-');

        let int_val = BigInt::parse_bytes(abs_int.as_bytes(), 10)?;
        let frac_val = BigInt::parse_bytes(frac_part.as_bytes(), 10)?;

        let numer = BigInt::from(sign) * (int_val * denom.clone() + frac_val);
        Some(BigRational::new(numer, denom))
    } else {
        let numer = BigInt::parse_bytes(s.as_bytes(), 10)?;
        Some(BigRational::from_integer(numer))
    }
}

impl Serialize for Amount {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let s = format!("{} {}", self.quantity, self.commodity);
        serializer.serialize_str(&s)
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        s.parse().map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_euro() {
        let a: Amount = "15 €".parse().unwrap();
        assert_eq!(a.commodity, Commodity::new("€"));
        assert_eq!(a.quantity, BigRational::from_integer(BigInt::from(15)));
    }

    #[test]
    fn parse_decimal() {
        let a: Amount = "0.434114 BTC".parse().unwrap();
        assert_eq!(a.commodity, Commodity::new("BTC"));
    }

    #[test]
    fn parse_negative() {
        let a: Amount = "-500 €".parse().unwrap();
        assert!(a.quantity < BigRational::zero());
    }
}
