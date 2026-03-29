use serde::Deserialize;
use crate::data::transaction::{Transaction, TransactionRaw};

#[derive(Debug)]
pub struct Ledger {
    pub owner: Option<String>,
    pub transactions: Vec<Transaction>,
}

impl Ledger {
    pub fn from_yaml(yaml: &str) -> Result<Self, String> {
        let raw: LedgerRaw = serde_yaml::from_str(yaml)
            .map_err(|e| format!("YAML parse error: {}", e))?;
        Ok(Ledger {
            owner: raw.owner,
            transactions: raw.transactions.into_iter().map(Transaction::from).collect(),
        })
    }
}

#[derive(Debug, Deserialize)]
struct LedgerRaw {
    #[serde(default)]
    pub owner: Option<String>,
    #[serde(default)]
    pub transactions: Vec<TransactionRaw>,
}
