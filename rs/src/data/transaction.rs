use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

use crate::data::transfer::Transfer;
use crate::datetime_serde;

/// A group of transfers (possibly with a note, title, and attached files)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction {
    #[serde(default)]
    pub id: Option<String>,
    #[serde(default, with = "datetime_serde::option")]
    pub utc: Option<NaiveDateTime>,
    /// Either "note" or "title" in YAML
    #[serde(default, alias = "title")]
    pub note: Option<String>,
    #[serde(default)]
    pub files: Vec<String>,
    pub transfers: Vec<Transfer>,
}
