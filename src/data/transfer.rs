use chrono::NaiveDateTime;
use serde::{Deserialize, Deserializer};
use crate::data::amount::{Amount, pad_start, pad_end, align_number, rational_to_f64};

#[derive(Debug, Clone)]
pub struct Transfer {
    pub utc: Option<NaiveDateTime>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    pub note: Option<String>,
}

impl Transfer {
    pub fn show_pretty_colorized(&self) -> String {
        self.show_pretty_aligned(15, 15, 5, 3, 10)
    }

    pub fn show_pretty_aligned(
        &self,
        from_w: usize,
        to_w: usize,
        int_w: usize,
        frac_w: usize,
        com_w: usize,
    ) -> String {
        let date_str = self.utc.map(|dt| format_date_pretty(dt));
        let from_part = pad_start(from_w, &self.from);
        let to_part = pad_start(to_w, &self.to);
        let n = rational_to_f64(&self.amount.quantity);
        let amount_str = align_number(int_w, frac_w, n);
        let com_str = pad_end(com_w, &self.amount.commodity.0);
        let note_str = self.note.as_deref().unwrap_or("");

        let rest = format!(
            "{} -> {} : {} {} | {}\n",
            from_part, to_part, amount_str, com_str, note_str
        );

        match date_str {
            Some(d) => format!("{} | {}", d, rest),
            None => format!("{}{}", " ".repeat(19), rest),
        }
    }
}

pub fn format_date_pretty(dt: NaiveDateTime) -> String {
    dt.format("%Y-%m-%d %H:%M").to_string()
}

pub fn parse_datetime(s: &str) -> Option<NaiveDateTime> {
    for fmt in &["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M"] {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(dt);
        }
    }
    chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d")
        .ok()
        .and_then(|d| d.and_hms_opt(0, 0, 0))
}

#[derive(Debug, Deserialize)]
pub struct TransferRaw {
    #[serde(default, deserialize_with = "deserialize_datetime_opt")]
    pub utc: Option<NaiveDateTime>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    #[serde(default)]
    pub note: Option<String>,
}

fn deserialize_datetime_opt<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<Option<NaiveDateTime>, D::Error> {
    let s: Option<String> = Option::deserialize(deserializer)?;
    Ok(s.and_then(|s| parse_datetime(&s)))
}

impl From<TransferRaw> for Transfer {
    fn from(r: TransferRaw) -> Self {
        Transfer {
            utc: r.utc,
            from: r.from,
            to: r.to,
            amount: r.amount,
            note: r.note,
        }
    }
}
