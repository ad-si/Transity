use crate::data::amount::Amount;
use crate::data::config::ColorFlag;
use crate::utils::{date_show_pretty, deserialize_optional_utc};
use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use num_traits::Zero;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transfer {
    #[serde(
        default,
        deserialize_with = "deserialize_optional_utc",
        skip_serializing_if = "Option::is_none"
    )]
    pub utc: Option<DateTime<Utc>>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
}

impl Transfer {
    pub fn verify(&self) -> Result<()> {
        if self.from.is_empty() {
            return Err(anyhow!("Field 'from' must not be empty"));
        }
        if self.to.is_empty() {
            return Err(anyhow!("Field 'to' must not be empty"));
        }
        if self.amount.quantity.is_zero() {
            return Err(anyhow!("Field 'amount' must not be 0"));
        }
        Ok(())
    }

    pub fn negate(&self) -> Transfer {
        Transfer {
            amount: self.amount.negate(),
            ..self.clone()
        }
    }

    pub fn zero() -> Transfer {
        Transfer {
            utc: None,
            from: String::new(),
            to: String::new(),
            amount: Amount::zero(),
            note: None,
        }
    }

    pub fn show_pretty(&self) -> String {
        self.show_pretty_aligned(ColorFlag::No, 15, 15, 5, 3, 10)
    }

    /// Format: `"YYYY-MM-DD HH:MM | {from:<from_w} -> {to:<to_w} : {amount} | {note}\n"`
    /// 19 spaces are used when no UTC is set (matches the date+separator width).
    pub fn show_pretty_aligned(
        &self,
        color: ColorFlag,
        from_w: usize,
        to_w: usize,
        int_w: usize,
        frac_w: usize,
        com_w: usize,
    ) -> String {
        let date_part = match &self.utc {
            Some(dt) => format!("{} | ", date_show_pretty(dt)),
            None => " ".repeat(19),
        };
        format!(
            "{}{:<from_w$} -> {:<to_w$} : {} | {}\n",
            date_part,
            self.from,
            self.to,
            self.amount.show_pretty_aligned(color, int_w, frac_w, com_w),
            self.note.as_deref().unwrap_or(""),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::amount::{parse_rational, Amount};
    use crate::data::commodity::Commodity;

    fn make_transfer(from: &str, to: &str, qty: &str) -> Transfer {
        Transfer {
            utc: None,
            from: from.to_string(),
            to: to.to_string(),
            amount: Amount {
                quantity: parse_rational(qty).unwrap(),
                commodity: Commodity("€".to_string()),
            },
            note: None,
        }
    }

    #[test]
    fn verify_rejects_empty_from() {
        let err = make_transfer("", "to", "10").verify().unwrap_err();
        assert!(err.to_string().contains("'from'"));
    }

    #[test]
    fn verify_rejects_empty_to() {
        let err = make_transfer("from", "", "10").verify().unwrap_err();
        assert!(err.to_string().contains("'to'"));
    }

    #[test]
    fn verify_rejects_zero_amount() {
        let err = make_transfer("from", "to", "0").verify().unwrap_err();
        assert!(err.to_string().contains("'amount'"));
    }

    #[test]
    fn verify_accepts_valid_transfer() {
        assert!(make_transfer("john:giro", "evil-corp", "15").verify().is_ok());
    }

    #[test]
    fn show_pretty_snapshot() {
        let output = make_transfer("john:giro", "evil-corp", "15").show_pretty();
        insta::assert_snapshot!(output);
    }
}
