use serde::{Deserialize, Serialize};

use crate::data::balance::Balance;

/// A physical account within an entity (e.g. wallet, giro, visa)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Account {
    pub id: String,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub note: Option<String>,
    #[serde(default)]
    pub tags: Option<Vec<String>>,
    /// "balances" key in YAML
    #[serde(default)]
    pub balances: Option<Vec<Balance>>,
}
