use serde::Deserialize;

#[derive(Debug, Deserialize, Default)]
pub struct Ledger {
    pub owner: Option<String>,
    pub entities: Option<Vec<Entity>>,
    #[serde(default)]
    pub transactions: Vec<Transaction>,
}

#[derive(Debug, Deserialize)]
pub struct Entity {
    pub id: String,
    pub accounts: Option<Vec<Account>>,
}

#[derive(Debug, Deserialize)]
pub struct Account {
    pub id: String,
}

#[derive(Debug, Deserialize)]
pub struct Transaction {
    pub utc: Option<String>,
    pub note: Option<String>,
    #[serde(default)]
    pub files: Vec<String>,
    #[serde(default)]
    pub transfers: Vec<Transfer>,
}

#[derive(Debug, Deserialize)]
pub struct Transfer {
    pub from: String,
    pub to: String,
    pub amount: String,
    pub utc: Option<String>,
}

impl Ledger {
    pub fn from_yaml(content: &str) -> anyhow::Result<Self> {
        let ledger: Ledger = serde_yaml::from_str(content)?;
        Ok(ledger)
    }
}
