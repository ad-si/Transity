use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::data::amount::Amount;

/// A transfer of an amount from one account to another at a given time.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transfer {
    pub utc: Option<DateTime<Utc>>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    pub note: Option<String>,
}
