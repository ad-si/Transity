use crate::data::commodity::Commodity;
use anyhow::{anyhow, Result};
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Zero;
use serde::{de, Deserialize, Deserializer, Serialize};
use std::fmt;
use std::str::FromStr;

/// An amount with a quantity and a commodity, e.g. "20 €"
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Amount {
    pub quantity: BigRational,
    pub commodity: Commodity,
}

impl Amount {
    pub fn zero() -> Self {
        Amount {
            quantity: BigRational::zero(),
            commodity: Commodity(String::new()),
        }
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

    /// Parse from a string like "20 €" or "7.8 €"
    pub fn parse(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.splitn(2, ' ').collect();
        if parts.len() != 2 {
            return Err(anyhow!("Amount must be '<value> <commodity>', got: {}", s));
        }
        let quantity = parse_rational(parts[0])?;
        Ok(Amount {
            quantity,
            commodity: Commodity(parts[1].to_string()),
        })
    }

    pub fn show_pretty_aligned(
        &self,
        _color: crate::data::config::ColorFlag,
        int_w: usize,
        frac_w: usize,
        com_w: usize,
    ) -> String {
        use num_traits::ToPrimitive;
        let val = self.quantity.to_f64().unwrap_or(0.0);
        format!("{} {:<com_w$}", format_number(val, int_w, frac_w), self.commodity.0)
    }

    pub fn show_pretty(&self) -> String {
        self.show_pretty_aligned(crate::data::config::ColorFlag::No, 0, 0, 0)
    }
}

impl fmt::Display for Amount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.show_pretty())
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(d: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        Amount::parse(&s).map_err(de::Error::custom)
    }
}

/// Parse a decimal string like "7.8" or "-20" into a BigRational.
pub fn parse_rational(s: &str) -> Result<BigRational> {
    if let Some(dot_pos) = s.find('.') {
        let frac_digits = s.len() - dot_pos - 1;
        let digits = s.replace('.', "");
        let numerator =
            BigInt::from_str(&digits).map_err(|_| anyhow!("Invalid number: {}", s))?;
        let denominator = BigInt::from(10u64).pow(frac_digits as u32);
        Ok(BigRational::new(numerator, denominator))
    } else {
        let n = BigInt::from_str(s).map_err(|_| anyhow!("Invalid number: {}", s))?;
        Ok(BigRational::new(n, BigInt::from(1)))
    }
}

fn format_number(val: f64, int_w: usize, frac_w: usize) -> String {
    let s = format!("{}", val);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_part = parts[0];
    let frac_part = if parts.len() > 1 && parts[1] != "0" {
        format!(".{}", parts[1])
    } else {
        String::new()
    };
    let int_padded = format!("{:>width$}", int_part, width = int_w);
    let frac_padded = if frac_w > 0 {
        format!("{:<width$}", frac_part, width = frac_w)
    } else {
        frac_part
    };
    format!("{}{}", int_padded, frac_padded)
}
