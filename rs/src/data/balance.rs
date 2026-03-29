use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

use crate::data::amount::Amount;
use crate::datetime_serde;

/// A balance checkpoint: at `utc`, the account held these amounts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Balance {
    #[serde(with = "datetime_serde")]
    pub utc: NaiveDateTime,
    pub amounts: Vec<Amount>,
}
