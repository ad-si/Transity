use num_rational::Ratio;
use num_bigint::BigInt;
use num_traits::Zero;
use serde::{Deserialize, Deserializer};
use std::fmt;
use std::str::FromStr;

pub type Rational = Ratio<BigInt>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Commodity(pub String);

impl fmt::Display for Commodity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount {
    pub quantity: Rational,
    pub commodity: Commodity,
}

impl Amount {
    pub fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }

    pub fn negate(&self) -> Self {
        Amount {
            quantity: -self.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }

    pub fn add(&self, other: &Amount) -> Amount {
        assert_eq!(self.commodity, other.commodity);
        Amount {
            quantity: self.quantity.clone() + other.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }

    pub fn subtract(&self, other: &Amount) -> Amount {
        assert_eq!(self.commodity, other.commodity);
        Amount {
            quantity: self.quantity.clone() - other.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }
}

/// Parse an amount string like "500 €" or "66.05 BTC"
impl FromStr for Amount {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.splitn(2, ' ').collect();
        if parts.len() != 2 {
            return Err(format!("Amount '{}' does not contain value and commodity", s));
        }
        let value_str = parts[0];
        let commodity_str = parts[1];
        let quantity = parse_rational(value_str)
            .ok_or_else(|| format!("Invalid amount value: '{}'", value_str))?;
        Ok(Amount {
            quantity,
            commodity: Commodity(commodity_str.to_string()),
        })
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        s.parse().map_err(serde::de::Error::custom)
    }
}

/// Parse a decimal string like "212.19" into a Rational.
pub fn parse_rational(s: &str) -> Option<Rational> {
    // Validate: only digits, '.', '-', '+'
    if !s.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == '+') {
        return None;
    }
    let dot_pos = s.find('.');
    match dot_pos {
        None => {
            // Integer
            let num = BigInt::from_str(s).ok()?;
            Some(Ratio::new(num, BigInt::from(1u32)))
        }
        Some(idx) => {
            // Remove decimal point
            let numerator_str: String = s.chars().filter(|&c| c != '.').collect();
            let num_digits_after_dot = s.len() - idx - 1;
            let denominator =
                BigInt::from(10u32).pow(num_digits_after_dot as u32);
            let numerator = BigInt::from_str(&numerator_str).ok()?;
            Some(Ratio::new(numerator, denominator))
        }
    }
}

/// Convert a Rational to f64, matching JS Number behavior for display.
pub fn rational_to_f64(r: &Rational) -> f64 {
    use num_traits::ToPrimitive;
    r.to_f64().unwrap_or(f64::NAN)
}

/// Format a number matching JS String(number): no trailing ".0", minimal significant digits.
pub fn format_number_js(n: f64) -> String {
    format!("{}", n)
}

pub fn split_number_parts(s: &str) -> (&str, &str) {
    match s.find('.') {
        Some(idx) => (&s[..idx], &s[idx..]),
        None => (s, ""),
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct WidthRecord {
    pub account: usize,
    pub integer: usize,
    pub fraction: usize,
    pub commodity: usize,
}

impl WidthRecord {
    pub fn zero() -> Self {
        WidthRecord::default()
    }

    pub fn merge(&self, other: &WidthRecord) -> WidthRecord {
        WidthRecord {
            account: self.account.max(other.account),
            integer: self.integer.max(other.integer),
            fraction: self.fraction.max(other.fraction),
            commodity: self.commodity.max(other.commodity),
        }
    }
}

pub fn amount_to_width_record(amount: &Amount) -> WidthRecord {
    let s = format_number_js(rational_to_f64(&amount.quantity));
    let (int_part, frac_part) = split_number_parts(&s);
    let frac_len = if frac_part.is_empty() || frac_part == ".0" { 0 } else { frac_part.chars().count() };
    WidthRecord {
        account: 0,
        integer: int_part.chars().count(),
        fraction: frac_len,
        commodity: amount.commodity.0.chars().count(),
    }
}

pub fn pad_start(n: usize, s: &str) -> String {
    let char_count = s.chars().count();
    if char_count >= n { s.to_string() } else { " ".repeat(n - char_count) + s }
}

pub fn pad_end(n: usize, s: &str) -> String {
    let char_count = s.chars().count();
    if char_count >= n { s.to_string() } else { s.to_string() + &" ".repeat(n - char_count) }
}

pub fn align_number(int_w: usize, frac_w: usize, n: f64) -> String {
    let s = format_number_js(n);
    let (int_part, frac_part) = split_number_parts(&s);

    let int_aligned = pad_start(int_w, int_part);

    let frac_aligned = if frac_part.is_empty() || frac_part == ".0" {
        " ".repeat(frac_w)
    } else {
        pad_end(frac_w, frac_part)
    };

    int_aligned + &frac_aligned
}

pub fn show_amount_aligned(int_w: usize, frac_w: usize, com_w: usize, amount: &Amount) -> String {
    let n = rational_to_f64(&amount.quantity);
    let num_part = align_number(int_w, frac_w, n);
    let com_part = pad_end(com_w, &amount.commodity.0);
    num_part + " " + &com_part
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_number_js() {
        assert_eq!(format_number_js(50.0), "50");
        assert_eq!(format_number_js(212.19), "212.19");
        assert_eq!(format_number_js(60.752114), "60.752114");
    }

    #[test]
    fn test_parse_rational() {
        let r = parse_rational("212.19").unwrap();
        let f = rational_to_f64(&r);
        assert_eq!(format_number_js(f), "212.19");
    }
}
