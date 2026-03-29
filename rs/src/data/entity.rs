use serde::{Deserialize, Serialize};

use crate::data::account::Account;

/// A person, company, or other participant that holds accounts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Entity {
    pub id: String,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub note: Option<String>,
    #[serde(default)]
    pub tags: Option<Vec<String>>,
    #[serde(default)]
    pub accounts: Option<Vec<Account>>,
}
