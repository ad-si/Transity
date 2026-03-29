use std::ops::{Add, AddAssign};

use anyhow::{bail, Result};
use num_rational::BigRational;
use num_traits::Zero;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use crate::data::commodity::Commodity;
use crate::data::config::ColorFlag;
use crate::utils::{align_number, digits_to_rational, length_of_num_parts, pad_end, rational_to_f64, WidthRecord};

/// E.g. "20 €", "10 cows", or "20 minutes"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Amount {
    pub quantity: BigRational,
    pub commodity: Commodity,
}

impl Amount {
    pub fn new(quantity: BigRational, commodity: Commodity) -> Self {
        Amount { quantity, commodity }
    }

    /// Parse a string like "20 €" or "10 cows".
    pub fn parse(s: &str) -> Result<Amount> {
        let mut parts = s.splitn(2, ' ');
        let value_str = parts
            .next()
            .ok_or_else(|| anyhow::anyhow!("Amount is empty"))?;
        let commodity_str = parts
            .next()
            .ok_or_else(|| anyhow::anyhow!("Amount does not contain a value and a commodity"))?;
        let quantity = digits_to_rational(value_str)
            .ok_or_else(|| anyhow::anyhow!("Amount does not contain a valid value"))?;
        Ok(Amount {
            quantity,
            commodity: Commodity::new(commodity_str.trim()),
        })
    }

    pub fn subtract(&self, other: &Amount) -> Result<Amount> {
        if self.commodity != other.commodity {
            bail!("INVALID COMPUTATION: cannot subtract different commodities");
        }
        Ok(Amount {
            quantity: &self.quantity - &other.quantity,
            commodity: self.commodity.clone(),
        })
    }

    pub fn negate(&self) -> Amount {
        Amount {
            quantity: -self.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }

    pub fn to_width_record(&self) -> WidthRecord {
        let n = rational_to_f64(&self.quantity);
        let (int_part, frac_part) = length_of_num_parts(n);
        WidthRecord {
            account: 0,
            integer: int_part,
            fraction: frac_part,
            commodity: self.commodity.as_str().chars().count(),
        }
    }

    pub fn show_pretty(&self) -> String {
        self.show_pretty_aligned(ColorFlag::No, 0, 0, 0)
    }

    /// Specify the width (in characters) of the integer part,
    /// the width of the fractional part (including decimal point),
    /// the width of the commodity part, and receive a pretty-printed amount.
    pub fn show_pretty_aligned(
        &self,
        color: ColorFlag,
        int_width: usize,
        frac_width: usize,
        com_width: usize,
    ) -> String {
        let n = rational_to_f64(&self.quantity);
        format!(
            "{} {}",
            align_number(color, int_width, frac_width, n),
            pad_end(com_width, self.commodity.as_str())
        )
    }
}

impl Add for Amount {
    type Output = Amount;

    fn add(self, other: Amount) -> Amount {
        if self.commodity != other.commodity {
            panic!("INVALID COMPUTATION: cannot add different commodities");
        }
        Amount {
            quantity: self.quantity + other.quantity,
            commodity: self.commodity,
        }
    }
}

impl AddAssign for Amount {
    fn add_assign(&mut self, other: Amount) {
        if self.commodity != other.commodity {
            panic!("INVALID COMPUTATION: cannot add different commodities");
        }
        self.quantity += other.quantity;
    }
}

impl Serialize for Amount {
    fn serialize<S: Serializer>(&self, s: S) -> std::result::Result<S::Ok, S::Error> {
        s.serialize_str(&self.show_pretty())
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D: Deserializer<'de>>(d: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        Amount::parse(&s).map_err(de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::BigInt;

    #[test]
    fn test_amount_parse() {
        let a = Amount::parse("20 €").unwrap();
        assert_eq!(a.commodity, Commodity::new("€"));
        assert_eq!(*a.quantity.numer(), BigInt::from(20));
        assert_eq!(*a.quantity.denom(), BigInt::from(1));
    }

    #[test]
    fn test_amount_parse_decimal() {
        let a = Amount::parse("66.05 USD").unwrap();
        assert_eq!(a.commodity, Commodity::new("USD"));
        // BigRational reduces 6605/100 to 1321/20, compare as rationals
        use num_rational::BigRational;
        assert_eq!(
            a.quantity,
            BigRational::new(BigInt::from(6605), BigInt::from(100))
        );
    }

    #[test]
    fn test_amount_add() {
        let a = Amount::parse("10 €").unwrap();
        let b = Amount::parse("5 €").unwrap();
        let c = a + b;
        assert_eq!(*c.quantity.numer(), BigInt::from(15));
        assert_eq!(c.commodity, Commodity::new("€"));
    }

    #[test]
    #[should_panic(expected = "INVALID COMPUTATION")]
    fn test_amount_add_different_commodities_panics() {
        let a = Amount::parse("10 €").unwrap();
        let b = Amount::parse("5 USD").unwrap();
        let _ = a + b;
    }

    #[test]
    fn test_amount_negate() {
        let a = Amount::parse("20 €").unwrap();
        let neg = a.negate();
        assert_eq!(*neg.quantity.numer(), BigInt::from(-20));
    }

    #[test]
    fn test_amount_is_zero() {
        let a = Amount::parse("0 €").unwrap();
        assert!(a.is_zero());
        let b = Amount::parse("1 €").unwrap();
        assert!(!b.is_zero());
    }

    #[test]
    fn test_amount_show_pretty() {
        let a = Amount::parse("20 €").unwrap();
        insta::assert_snapshot!(a.show_pretty());
    }

    #[test]
    fn test_amount_show_pretty_decimal() {
        let a = Amount::parse("66.05 USD").unwrap();
        insta::assert_snapshot!(a.show_pretty());
    }
}
