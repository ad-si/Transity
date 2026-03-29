use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

use crate::data::amount::Amount;
use crate::datetime_serde;

/// A single movement of value from one account to another
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transfer {
    #[serde(default, with = "datetime_serde::option")]
    pub utc: Option<NaiveDateTime>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    #[serde(default)]
    pub note: Option<String>,
}
