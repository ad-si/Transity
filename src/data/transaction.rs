use crate::data::config::ColorFlag;
use crate::data::transfer::Transfer;
use crate::utils::{date_show_pretty, deserialize_optional_utc, indent_subsequent};
use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(
        default,
        deserialize_with = "deserialize_optional_utc",
        skip_serializing_if = "Option::is_none"
    )]
    pub utc: Option<DateTime<Utc>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
    #[serde(default)]
    pub files: Vec<String>,
    pub transfers: Vec<Transfer>,
}

impl Transaction {
    pub fn verify(&self) -> Result<()> {
        for transfer in &self.transfers {
            transfer.verify()?;
        }
        Ok(())
    }

    pub fn to_transfers(&self) -> Vec<Transfer> {
        self.transfers
            .iter()
            .map(|t| Transfer {
                utc: t.utc.or(self.utc),
                ..t.clone()
            })
            .collect()
    }

    pub fn show_pretty(&self) -> String {
        self.show_pretty_aligned(ColorFlag::No)
    }

    pub fn show_pretty_aligned(&self, color: ColorFlag) -> String {
        // date_show_pretty produces "YYYY-MM-DD HH:MM" = 16 chars
        let date_part = match &self.utc {
            Some(dt) => date_show_pretty(dt),
            None => " ".repeat(16),
        };
        let note_part = format!("{:<30}", self.note.as_deref().unwrap_or("NO NOTE"));
        let id_part = match &self.id {
            Some(id) => format!(" | (id {})", id),
            None => String::new(),
        };
        let transfers_pretty: String = self
            .transfers
            .iter()
            .map(|t| t.show_pretty_aligned(color, 15, 15, 5, 3, 10))
            .collect();
        let indented = indent_subsequent(4, &format!("\n{}", transfers_pretty));
        format!("{} | {}{}{}\n", date_part, note_part, id_part, indented)
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

    fn make_transaction(utc: Option<DateTime<Utc>>, transfers: Vec<Transfer>) -> Transaction {
        Transaction {
            id: None,
            utc,
            note: Some("Test".to_string()),
            files: vec![],
            transfers,
        }
    }

    #[test]
    fn to_transfers_promotes_utc_from_parent() {
        use chrono::TimeZone;
        let dt = Utc.with_ymd_and_hms(2017, 12, 24, 0, 0, 0).unwrap();
        let tx = make_transaction(Some(dt), vec![make_transfer("john:giro", "evil-corp", "15")]);
        assert_eq!(tx.to_transfers()[0].utc, Some(dt));
    }

    #[test]
    fn to_transfers_keeps_transfer_utc_if_set() {
        use chrono::TimeZone;
        let parent_dt = Utc.with_ymd_and_hms(2017, 12, 24, 0, 0, 0).unwrap();
        let transfer_dt = Utc.with_ymd_and_hms(2017, 12, 25, 12, 0, 0).unwrap();
        let mut t = make_transfer("john:giro", "evil-corp", "15");
        t.utc = Some(transfer_dt);
        let tx = make_transaction(Some(parent_dt), vec![t]);
        assert_eq!(tx.to_transfers()[0].utc, Some(transfer_dt));
    }

    #[test]
    fn to_transfers_no_utc_on_either() {
        let tx = make_transaction(None, vec![make_transfer("john:giro", "evil-corp", "15")]);
        assert!(tx.to_transfers()[0].utc.is_none());
    }

    #[test]
    fn yaml_deserialization_and_verify() {
        let yaml = r#"
utc: '2017-12-24'
note: Money for evil deal
transfers:
  - from: john:giro
    to: evil-corp
    amount: 15 €
"#;
        let tx: Transaction = serde_yaml::from_str(yaml).unwrap();
        assert!(tx.verify().is_ok());
        assert_eq!(tx.note.as_deref(), Some("Money for evil deal"));
        assert_eq!(tx.transfers.len(), 1);
        assert_eq!(tx.transfers[0].from, "john:giro");
    }
}
