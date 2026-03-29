use std::path::Path;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use crate::data::entity::Entity;
use crate::data::transaction::Transaction;

/// The top-level journal — owner, entities, and transactions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ledger {
    #[serde(default)]
    pub owner: Option<String>,
    #[serde(default)]
    pub entities: Option<Vec<Entity>>,
    pub transactions: Vec<Transaction>,
}

impl Default for Ledger {
    fn default() -> Self {
        Ledger {
            owner: None,
            entities: None,
            transactions: vec![],
        }
    }
}

impl Ledger {
    /// Parse a YAML string into a Ledger.
    pub fn from_yaml(s: &str) -> Result<Ledger> {
        serde_yaml::from_str(s).context("Failed to parse ledger YAML")
    }

    /// Parse a JSON string into a Ledger.
    pub fn from_json(s: &str) -> Result<Ledger> {
        serde_json::from_str(s).context("Failed to parse ledger JSON")
    }

    /// Read and parse a file as a Ledger (YAML or JSON based on extension).
    pub fn from_file(path: &Path) -> Result<Ledger> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Cannot read file: {}", path.display()))?;
        match path.extension().and_then(|e| e.to_str()) {
            Some("json") => Self::from_json(&content),
            _ => Self::from_yaml(&content),
        }
    }

    /// Merge multiple ledger files left-associatively.
    pub fn combine(paths: &[&Path]) -> Result<Ledger> {
        paths
            .iter()
            .map(|p| Self::from_file(p))
            .try_fold(Ledger::default(), |acc, ledger| Ok(acc + ledger?))
    }
}

/// Merge two ledgers: owner/entities from `self`, transactions combined.
/// When both have entities, the vecs are concatenated.
impl std::ops::Add<Ledger> for Ledger {
    type Output = Ledger;

    fn add(self, rhs: Ledger) -> Ledger {
        let entities = match (self.entities, rhs.entities) {
            (None, r) => r,
            (l, None) => l,
            (Some(mut l), Some(r)) => {
                l.extend(r);
                Some(l)
            }
        };
        let mut transactions = self.transactions;
        transactions.extend(rhs.transactions);
        Ledger {
            owner: self.owner,
            entities,
            transactions,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minimal_yaml() {
        let yaml = r#"
transactions:
  - utc: '2017-12-24'
    note: Test
    transfers:
      - from: alice
        to: bob
        amount: 10 €
"#;
        let ledger = Ledger::from_yaml(yaml).unwrap();
        assert_eq!(ledger.transactions.len(), 1);
        assert_eq!(ledger.transactions[0].note.as_deref(), Some("Test"));
    }

    #[test]
    fn full_journal_yaml() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("examples/journal.yaml");
        let ledger = Ledger::from_file(&path).unwrap();
        assert_eq!(ledger.owner.as_deref(), Some("john"));
        assert!(!ledger.transactions.is_empty());
        assert!(!ledger.entities.as_ref().unwrap().is_empty());
    }

    #[test]
    fn add_merges_transactions() {
        let l1 = Ledger {
            owner: Some("alice".into()),
            entities: None,
            transactions: vec![],
        };
        let l2 = Ledger {
            owner: Some("bob".into()),
            entities: None,
            transactions: vec![],
        };
        let merged = l1 + l2;
        assert_eq!(merged.owner.as_deref(), Some("alice"));
    }

    #[test]
    fn add_concatenates_entities() {
        use crate::data::entity::Entity;

        let make_entity = |id: &str| Entity {
            id: id.into(),
            name: None,
            note: None,
            tags: None,
            accounts: None,
        };

        let l1 = Ledger {
            owner: None,
            entities: Some(vec![make_entity("anna")]),
            transactions: vec![],
        };
        let l2 = Ledger {
            owner: None,
            entities: Some(vec![make_entity("bob")]),
            transactions: vec![],
        };
        let merged = l1 + l2;
        let entities = merged.entities.unwrap();
        assert_eq!(entities.len(), 2);
        assert_eq!(entities[0].id, "anna");
        assert_eq!(entities[1].id, "bob");
    }
}
