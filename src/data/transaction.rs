use chrono::NaiveDateTime;
use serde::{Deserialize, Deserializer};
use crate::data::amount::pad_start;
use crate::data::transfer::{Transfer, TransferRaw, format_date_pretty, parse_datetime};

#[derive(Debug, Clone)]
pub struct Transaction {
    pub utc: Option<NaiveDateTime>,
    pub note: Option<String>,
    pub transfers: Vec<Transfer>,
}

impl Transaction {
    pub fn to_transfers(&self) -> Vec<Transfer> {
        self.transfers.iter().map(|t| Transfer {
            utc: t.utc.or(self.utc),
            from: t.from.clone(),
            to: t.to.clone(),
            amount: t.amount.clone(),
            note: t.note.clone(),
        }).collect()
    }

    pub fn show_pretty_aligned(&self) -> String {
        let date_part = match self.utc {
            Some(dt) => format_date_pretty(dt),
            None => " ".repeat(16),
        };
        let note_part = pad_start(30, self.note.as_deref().unwrap_or("NO NOTE"));
        let transfers_pretty: String = self.transfers.iter()
            .map(|t| t.show_pretty_aligned(15, 15, 5, 3, 10))
            .collect();
        let indented = indent_subsequent(4, &format!("\n{}", transfers_pretty));
        format!("{} | {}{}\n", date_part, note_part, indented)
    }
}

fn indent_subsequent(n: usize, s: &str) -> String {
    s.replace('\n', &format!("\n{}", " ".repeat(n)))
}

fn deserialize_datetime_opt<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<Option<NaiveDateTime>, D::Error> {
    let s: Option<String> = Option::deserialize(deserializer)?;
    Ok(s.and_then(|s| parse_datetime(&s)))
}

#[derive(Debug, Deserialize)]
pub struct TransactionRaw {
    #[serde(default, deserialize_with = "deserialize_datetime_opt")]
    pub utc: Option<NaiveDateTime>,
    #[serde(default)]
    pub note: Option<String>,
    pub transfers: Vec<TransferRaw>,
}

impl From<TransactionRaw> for Transaction {
    fn from(r: TransactionRaw) -> Self {
        Transaction {
            utc: r.utc,
            note: r.note,
            transfers: r.transfers.into_iter().map(Transfer::from).collect(),
        }
    }
}
