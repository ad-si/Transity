use chrono::{DateTime, Utc};
use serde::{Deserialize, Deserializer, Serialize};

use crate::data::account::Account;
use crate::data::transfer::Transfer;

/// An entity (person, organization, etc.) that owns accounts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Entity {
    pub id: String,
    pub name: Option<String>,
    pub note: Option<String>,
    #[serde(default, deserialize_with = "deserialize_optional_utc")]
    pub utc: Option<DateTime<Utc>>,
    pub tags: Option<Vec<String>>,
    pub accounts: Option<Vec<Account>>,
}

/// Deserialize an optional UTC datetime from either a string (flexible format) or null.
fn deserialize_optional_utc<'de, D>(deserializer: D) -> Result<Option<DateTime<Utc>>, D::Error>
where
    D: Deserializer<'de>,
{
    let s: Option<String> = Option::deserialize(deserializer)?;
    match s {
        None => Ok(None),
        Some(ref text) => parse_datetime_str(text)
            .map(Some)
            .map_err(serde::de::Error::custom),
    }
}

/// Parse a datetime string in several common formats.
pub fn parse_datetime_str(s: &str) -> anyhow::Result<DateTime<Utc>> {
    if let Ok(dt) = s.parse::<DateTime<Utc>>() {
        return Ok(dt);
    }
    if let Ok(naive) = chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S") {
        return Ok(naive.and_utc());
    }
    if let Ok(naive) = chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M") {
        return Ok(naive.and_utc());
    }
    if let Ok(date) = chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        return Ok(date.and_hms_opt(0, 0, 0).unwrap().and_utc());
    }
    anyhow::bail!("Cannot parse datetime string: {:?}", s)
}

impl Entity {
    /// Return accounts with IDs qualified as `entity_id:account_id`.
    pub fn to_accounts_with_id(&self) -> Vec<Account> {
        self.accounts
            .as_deref()
            .unwrap_or(&[])
            .iter()
            .map(|acc| Account {
                id: format!("{}:{}", self.id, acc.id),
                ..acc.clone()
            })
            .collect()
    }

    /// Convert balance snapshots to synthetic transfers (balance reconciliation transfers).
    /// Each balance amount becomes a Transfer from account to "_void_".
    pub fn to_transfers(&self) -> Vec<Transfer> {
        self.to_accounts_with_id()
            .into_iter()
            .flat_map(|acc| {
                acc.balances
                    .clone()
                    .unwrap_or_default()
                    .into_iter()
                    .flat_map(move |balance| {
                        let com_map = balance.to_commodity_map();
                        let account_id = acc.id.clone();
                        com_map.into_values().map(move |amount| Transfer {
                            utc: Some(balance.utc),
                            from: account_id.clone(),
                            to: "_void_".to_string(),
                            amount,
                            note: None,
                        })
                    })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ENTITY_YAML: &str = r#"
id: anna
name: Anna Smith
accounts:
  - id: wallet
    balances:
      - utc: '2015-04-02T20:11:45Z'
        amounts:
          - "10 €"
          - "5 USD"
"#;

    #[test]
    fn deserialize_entity() {
        let entity: Entity = serde_yaml::from_str(ENTITY_YAML).unwrap();
        assert_eq!(entity.id, "anna");
        assert_eq!(entity.name.as_deref(), Some("Anna Smith"));
        assert!(entity.accounts.is_some());
        let accounts = entity.accounts.unwrap();
        assert_eq!(accounts.len(), 1);
        assert_eq!(accounts[0].id, "wallet");
    }

    #[test]
    fn to_accounts_with_id() {
        let entity: Entity = serde_yaml::from_str(ENTITY_YAML).unwrap();
        let accounts = entity.to_accounts_with_id();
        assert_eq!(accounts.len(), 1);
        assert_eq!(accounts[0].id, "anna:wallet");
    }

    #[test]
    fn to_transfers() {
        let entity: Entity = serde_yaml::from_str(ENTITY_YAML).unwrap();
        let transfers = entity.to_transfers();
        assert_eq!(transfers.len(), 2); // 2 amounts in the balance
        for t in &transfers {
            assert_eq!(t.from, "anna:wallet");
            assert_eq!(t.to, "_void_");
            assert!(t.utc.is_some());
        }
    }

    #[test]
    fn entity_utc_date_only() {
        let yaml = r#"
id: bob
utc: '2017-02-17'
"#;
        let entity: Entity = serde_yaml::from_str(yaml).unwrap();
        assert!(entity.utc.is_some());
    }

    #[test]
    fn entity_utc_datetime() {
        let yaml = r#"
id: bob
utc: '2017-02-17 12:00'
"#;
        let entity: Entity = serde_yaml::from_str(yaml).unwrap();
        assert!(entity.utc.is_some());
    }

    #[test]
    fn entity_no_accounts_to_transfers_is_empty() {
        let entity = Entity {
            id: "empty".to_string(),
            name: None,
            note: None,
            utc: None,
            tags: None,
            accounts: None,
        };
        assert!(entity.to_transfers().is_empty());
        assert!(entity.to_accounts_with_id().is_empty());
    }
}
