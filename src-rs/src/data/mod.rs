use chrono::NaiveDateTime;
use std::collections::BTreeSet;

use crate::utils::{date_show_pretty_long, parse_datetime};

// ---------------------------------------------------------------------------
// Amount / Commodity
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct Amount {
    pub quantity: num_rational::Rational64,
    pub commodity: String,
}

impl Amount {
    /// Pretty-print the amount: e.g. "500 €" or "33.95 €"
    pub fn show_pretty(&self) -> String {
        if self.quantity.denom() == &1 {
            format!("{} {}", self.quantity.numer(), self.commodity)
        } else {
            let n = *self.quantity.numer() as f64 / *self.quantity.denom() as f64;
            format!("{} {}", n, self.commodity)
        }
    }
}

fn parse_amount_str(s: &str) -> Option<Amount> {
    let parts: Vec<&str> = s.splitn(2, ' ').collect();
    if parts.len() != 2 {
        return None;
    }
    let value_str = parts[0];
    let commodity = parts[1].to_string();

    // Parse as rational from decimal string
    let quantity = parse_rational(value_str)?;
    Some(Amount { quantity, commodity })
}

fn parse_rational(s: &str) -> Option<num_rational::Rational64> {
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        let without_dot = s.replace('.', "");
        let numer: i64 = without_dot.parse().ok()?;
        let denom: i64 = 10i64.pow(decimals as u32);
        Some(num_rational::Rational64::new(numer, denom))
    } else {
        let n: i64 = s.parse().ok()?;
        Some(num_rational::Rational64::new(n, 1))
    }
}

// ---------------------------------------------------------------------------
// CommodityMap entry — list of commodity name strings
// ---------------------------------------------------------------------------

/// Ordered list of commodity names that appeared in amounts
pub type CommodityMapJson = Vec<String>;

fn commodity_key_json(name: &str) -> String {
    format!(
        "{{\"values\":{},\"tag\":\"Commodity\"}}",
        serde_json::to_string(&[name]).unwrap()
    )
}

fn commodity_map_to_json(commodities: &[String]) -> String {
    let entries: Vec<String> = commodities
        .iter()
        .map(|c| format!("[{},\"TODO\"]", commodity_key_json(c)))
        .collect();
    format!("[{}]", entries.join(","))
}

// ---------------------------------------------------------------------------
// Balance
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Balance {
    pub utc: NaiveDateTime,
    pub commodity_map: CommodityMapJson,
}

impl Balance {
    pub fn to_json_string(&self) -> String {
        format!(
            "{{\"utc\":{},\"amounts\":[],\"commodityMap\":{}}}",
            serde_json::to_string(&date_show_pretty_long(&self.utc)).unwrap(),
            commodity_map_to_json(&self.commodity_map)
        )
    }
}

// ---------------------------------------------------------------------------
// Account
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Account {
    pub id: String,
    pub commodity_map: CommodityMapJson,
    pub balances: Option<Vec<Balance>>,
}

impl Account {
    pub fn to_json_string(&self) -> String {
        let balances_str = match &self.balances {
            None => "null".to_string(),
            Some(bals) => {
                let entries: Vec<String> = bals.iter().map(|b| b.to_json_string()).collect();
                format!("[{}]", entries.join(","))
            }
        };
        format!(
            "{{\"id\":{},\"commodityMap\":{},\"balances\":{}}}",
            serde_json::to_string(&self.id).unwrap(),
            commodity_map_to_json(&self.commodity_map),
            balances_str
        )
    }
}

// ---------------------------------------------------------------------------
// Entity
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Entity {
    pub id: String,
    pub name: Option<String>,
    pub note: Option<String>,
    pub utc: Option<NaiveDateTime>,
    pub tags: Option<Vec<String>>,
    pub accounts: Option<Vec<Account>>,
}

// ---------------------------------------------------------------------------
// Transfer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Transfer {
    pub utc: Option<NaiveDateTime>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    pub note: Option<String>,
}

// ---------------------------------------------------------------------------
// Transaction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Transaction {
    pub id: Option<String>,
    pub utc: Option<NaiveDateTime>,
    pub note: Option<String>,
    pub files: Vec<String>,
    pub transfers: Vec<Transfer>,
}

// ---------------------------------------------------------------------------
// Ledger
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Ledger {
    pub owner: Option<String>,
    pub entities: Option<Vec<Entity>>,
    pub transactions: Vec<Transaction>,
}

// ---------------------------------------------------------------------------
// YAML parsing
// ---------------------------------------------------------------------------

pub fn parse_ledger(yaml: &str) -> anyhow::Result<Ledger> {
    let value: serde_yaml::Value = serde_yaml::from_str(yaml)?;
    let map = value
        .as_mapping()
        .ok_or_else(|| anyhow::anyhow!("Expected YAML mapping at root"))?;

    let owner = map
        .get("owner")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let entities = parse_entities(map.get("entities"));
    let transactions = parse_transactions(map.get("transactions"))?;

    Ok(Ledger {
        owner,
        entities,
        transactions,
    })
}

fn parse_entities(val: Option<&serde_yaml::Value>) -> Option<Vec<Entity>> {
    let arr = val?.as_sequence()?;
    let entities: Vec<Entity> = arr.iter().filter_map(parse_entity).collect();
    Some(entities)
}

fn parse_entity(val: &serde_yaml::Value) -> Option<Entity> {
    let map = val.as_mapping()?;
    let id = map.get("id")?.as_str()?.to_string();
    let name = map
        .get("name")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let note = map
        .get("note")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let utc = map
        .get("utc")
        .and_then(|v| v.as_str())
        .and_then(parse_datetime);
    let tags = map.get("tags").and_then(|v| v.as_sequence()).map(|seq| {
        seq.iter()
            .filter_map(|t| t.as_str().map(|s| s.to_string()))
            .collect()
    });
    let accounts = map
        .get("accounts")
        .and_then(|v| v.as_sequence())
        .map(|seq| seq.iter().filter_map(parse_account).collect());

    Some(Entity {
        id,
        name,
        note,
        utc,
        tags,
        accounts,
    })
}

fn parse_account(val: &serde_yaml::Value) -> Option<Account> {
    let map = val.as_mapping()?;
    let id = map.get("id")?.as_str()?.to_string();

    let balances = map
        .get("balances")
        .and_then(|v| v.as_sequence())
        .map(|seq| seq.iter().filter_map(parse_balance).collect());

    Some(Account {
        id,
        commodity_map: vec![],
        balances,
    })
}

fn parse_balance(val: &serde_yaml::Value) -> Option<Balance> {
    let map = val.as_mapping()?;
    let utc_str = map.get("utc")?.as_str()?;
    let utc = parse_datetime(utc_str)?;

    let amounts: Vec<Amount> = map
        .get("amounts")
        .and_then(|v| v.as_sequence())
        .map(|seq| {
            seq.iter()
                .filter_map(|a| a.as_str().and_then(parse_amount_str))
                .collect()
        })
        .unwrap_or_default();

    // Collect unique commodity names, sorted (PureScript Map is ordered by key)
    let commodity_map_json: CommodityMapJson = amounts
        .iter()
        .map(|a| a.commodity.clone())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();

    Some(Balance {
        utc,
        commodity_map: commodity_map_json,
    })
}

fn parse_transactions(val: Option<&serde_yaml::Value>) -> anyhow::Result<Vec<Transaction>> {
    let arr = match val.and_then(|v| v.as_sequence()) {
        Some(a) => a,
        None => return Ok(vec![]),
    };
    let txns: Vec<Transaction> = arr.iter().filter_map(parse_transaction).collect();
    Ok(txns)
}

fn parse_transaction(val: &serde_yaml::Value) -> Option<Transaction> {
    let map = val.as_mapping()?;
    let id = map
        .get("id")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let utc = map
        .get("utc")
        .and_then(|v| v.as_str())
        .and_then(parse_datetime);
    let note = map
        .get("note")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    let files = map
        .get("files")
        .and_then(|v| v.as_sequence())
        .map(|seq| {
            seq.iter()
                .filter_map(|f| f.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();
    let transfers = map
        .get("transfers")
        .and_then(|v| v.as_sequence())
        .map(|seq| seq.iter().filter_map(|t| parse_transfer(t, utc)).collect())
        .unwrap_or_default();

    Some(Transaction {
        id,
        utc,
        note,
        files,
        transfers,
    })
}

fn parse_transfer(val: &serde_yaml::Value, txn_utc: Option<NaiveDateTime>) -> Option<Transfer> {
    let map = val.as_mapping()?;
    let from = map.get("from")?.as_str()?.to_string();
    let to = map.get("to")?.as_str()?.to_string();
    let amount_str = map.get("amount")?.as_str()?;
    let amount = parse_amount_str(amount_str)?;
    let utc = map
        .get("utc")
        .and_then(|v| v.as_str())
        .and_then(parse_datetime)
        .or(txn_utc);
    let note = map
        .get("note")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    Some(Transfer {
        utc,
        from,
        to,
        amount,
        note,
    })
}
