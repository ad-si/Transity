use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};

use crate::data::{
    is_commodity_zero, add_amount_to_map, subtract_amount_from_map,
    CommodityMap, Ledger, Transfer,
};

/// BalanceMap: account_id -> CommodityMap
pub type BalanceMap = HashMap<String, CommodityMap>;

/// Normalize an account ID: if it has no colon, append ":_default_"
fn normalize_account_id(id: &str) -> String {
    if id.contains(':') {
        id.to_string()
    } else {
        format!("{}:_default_", id)
    }
}

/// Apply a transfer to the balance map:
/// - `to` account gets the amount added
/// - `from` account gets the amount subtracted
pub fn add_transfer(map: &mut BalanceMap, transfer: &Transfer) {
    let to = normalize_account_id(&transfer.to);
    let from = normalize_account_id(&transfer.from);

    let to_entry = map.entry(to).or_insert_with(CommodityMap::new);
    add_amount_to_map(to_entry, &transfer.amount);

    let from_entry = map.entry(from).or_insert_with(CommodityMap::new);
    subtract_amount_from_map(from_entry, &transfer.amount);
}

/// Verify that all accounts used in transfers are declared in entities.
pub fn verify_accounts(ledger: &Ledger) -> Result<()> {
    let defined: HashSet<String> = ledger
        .entities
        .as_deref()
        .unwrap_or(&[])
        .iter()
        .flat_map(|entity| {
            let mut ids = vec![entity.id.clone()];
            if let Some(accounts) = &entity.accounts {
                for account in accounts {
                    ids.push(format!("{}:{}", entity.id, account.id));
                }
            }
            ids
        })
        .collect();

    let used: HashSet<String> = ledger
        .transactions
        .iter()
        .flat_map(|tx| {
            tx.transfers
                .iter()
                .flat_map(|t| [t.from.clone(), t.to.clone()])
        })
        .collect();

    let mut undefined: Vec<String> = used.difference(&defined).cloned().collect();
    undefined.sort();

    if !undefined.is_empty() {
        let ids = undefined
            .iter()
            .map(|id| format!("\n  - id: {}", id))
            .collect::<Vec<_>>()
            .join("");
        bail!(
            "Following accounts were not declared, but still used for transfers:\n\nentities:{}\n\nPlease add or rename the missing accounts to the entities section to fix this error",
            ids
        );
    }

    Ok(())
}

/// Format a NaiveDateTime in the PureScript dateShowPretty style: "YYYY-MM-DD HH:MM"
fn format_datetime(dt: &chrono::NaiveDateTime) -> String {
    dt.format("%Y-%m-%d %H:%M").to_string()
}

/// Verify that balance snapshots in entities match the computed running balance.
pub fn verify_ledger_balances(ledger: &Ledger) -> Result<()> {
    // Skip if no entities defined
    let entities = match &ledger.entities {
        None => return Ok(()),
        Some(e) if e.is_empty() => return Ok(()),
        Some(e) => e,
    };

    let balance_marker = "___BALANCE___";

    let mut combined: Vec<Transfer> = entities
        .iter()
        .flat_map(|entity| entity.to_transfers())
        .map(|mut t| { t.note = Some(balance_marker.to_string()); t })
        .chain(ledger.transactions.iter().flat_map(|tx| tx.to_transfers()))
        .collect();

    // None UTCs sort last
    combined.sort_by(|a, b| a.utc.cmp(&b.utc));

    let mut balance_map: BalanceMap = BalanceMap::new();

    for transfer in &combined {
        if transfer.note.as_deref() == Some(balance_marker) {
            add_transfer(&mut balance_map, transfer);

            let from = normalize_account_id(&transfer.from);
            let empty = CommodityMap::new();
            if !is_commodity_zero(
                balance_map.get(&from).unwrap_or(&empty),
                &transfer.amount.commodity,
            ) {
                let date_str = transfer.utc.as_ref().map(format_datetime).unwrap_or_default();
                let off_by = balance_map
                    .get(&from)
                    .and_then(|cm| cm.get(&transfer.amount.commodity))
                    .map(|a| a.negate().show_pretty())
                    .unwrap_or_else(|| "ERROR: Amount is missing".to_string());
                bail!(
                    "ERROR:\nThe verification balance of account '{}' on '{}'\nis off by {} from the calculated balance.",
                    transfer.from,
                    date_str,
                    off_by
                );
            }

            // Undo the balance-check transfer so it doesn't affect subsequent calculations
            let undo = Transfer {
                utc: transfer.utc,
                from: transfer.to.clone(),
                to: transfer.from.clone(),
                amount: transfer.amount.clone(),
                note: None,
            };
            add_transfer(&mut balance_map, &undo);
        } else {
            add_transfer(&mut balance_map, transfer);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{Amount, Commodity, Ledger};
    use num_rational::Rational64;

    fn make_transfer(from: &str, to: &str, qty: i64, denom: i64, com: &str) -> Transfer {
        Transfer {
            utc: None,
            from: from.to_string(),
            to: to.to_string(),
            amount: Amount {
                quantity: Rational64::new(qty, denom),
                commodity: Commodity(com.to_string()),
            },
            note: None,
        }
    }

    #[test]
    fn test_add_transfer_normalizes_single_segment() {
        let mut map = BalanceMap::new();
        let t = make_transfer("alice", "bob", 100, 1, "€");
        add_transfer(&mut map, &t);

        // Single-segment accounts should be normalized to "alice:_default_" and "bob:_default_"
        assert!(map.contains_key("alice:_default_"), "alice should be normalized");
        assert!(map.contains_key("bob:_default_"), "bob should be normalized");
        assert!(!map.contains_key("alice"), "alice should not appear un-normalized");
        assert!(!map.contains_key("bob"), "bob should not appear un-normalized");
    }

    #[test]
    fn test_add_transfer_two_segment_unchanged() {
        let mut map = BalanceMap::new();
        let t = make_transfer("john:wallet", "anna:savings", 50, 1, "€");
        add_transfer(&mut map, &t);

        assert!(map.contains_key("john:wallet"));
        assert!(map.contains_key("anna:savings"));
    }

    #[test]
    fn test_add_transfer_balances() {
        let mut map = BalanceMap::new();
        let t = make_transfer("john:wallet", "anna:default_", 100, 1, "€");
        add_transfer(&mut map, &t);

        let john_map = map.get("john:wallet").unwrap();
        let eur = Commodity("€".to_string());
        let john_eur = john_map.get(&eur).unwrap();
        assert_eq!(john_eur.quantity, Rational64::new(-100, 1));

        let anna_map = map.get("anna:default_").unwrap();
        let anna_eur = anna_map.get(&eur).unwrap();
        assert_eq!(anna_eur.quantity, Rational64::new(100, 1));
    }

    fn load_ledger(path: &str) -> Ledger {
        let content = std::fs::read_to_string(path)
            .unwrap_or_else(|_| panic!("Cannot read {}", path));
        Ledger::from_yaml(&content).unwrap_or_else(|e| panic!("Parse error in {}: {}", path, e))
    }

    #[test]
    fn test_verify_accounts_journal_passes() {
        let ledger = load_ledger("examples/journal.yaml");
        verify_accounts(&ledger).expect("verify_accounts should pass for journal.yaml");
    }

    #[test]
    fn test_verify_accounts_undeclared_fails() {
        let ledger = load_ledger("examples/journal.yaml");

        use crate::data::Transaction;
        let mut bad_ledger = ledger.clone();
        bad_ledger.transactions.push(Transaction {
            utc: None,
            note: None,
            transfers: vec![make_transfer("john:wallet", "undeclared_xyz", 10, 1, "€")],
        });

        let result = verify_accounts(&bad_ledger);
        assert!(result.is_err(), "Should fail with undeclared account");
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("undeclared_xyz"), "Error should mention undeclared_xyz");
        assert!(msg.contains("Following accounts were not declared"), "Error message format");
    }

    #[test]
    fn test_verify_ledger_balances_journal_passes() {
        let ledger = load_ledger("examples/journal.yaml");
        verify_ledger_balances(&ledger).expect("verify_ledger_balances should pass for journal.yaml");
    }

    #[test]
    fn test_verify_ledger_balances_broken_fails() {
        use crate::data::{Account, Balance, Entity, Transaction};

        // A ledger where the declared balance (999 €) doesn't match the computed balance (0 €)
        let ledger = Ledger {
            owner: Some("john".to_string()),
            entities: Some(vec![
                Entity {
                    id: "john".to_string(),
                    accounts: Some(vec![Account {
                        id: "wallet".to_string(),
                        balances: Some(vec![
                            Balance {
                                utc: chrono::NaiveDate::from_ymd_opt(2024, 1, 1)
                                    .unwrap()
                                    .and_hms_opt(0, 0, 0)
                                    .unwrap(),
                                amounts: vec![Amount { quantity: Rational64::new(0, 1), commodity: Commodity("€".to_string()) }],
                            },
                            Balance {
                                utc: chrono::NaiveDate::from_ymd_opt(2024, 12, 1)
                                    .unwrap()
                                    .and_hms_opt(0, 0, 0)
                                    .unwrap(),
                                amounts: vec![Amount { quantity: Rational64::new(999, 1), commodity: Commodity("€".to_string()) }],
                            },
                        ]),
                    }]),
                },
                Entity { id: "bakery".to_string(), accounts: None },
            ]),
            transactions: vec![Transaction {
                utc: Some(chrono::NaiveDate::from_ymd_opt(2024, 5, 28).unwrap().and_hms_opt(14, 35, 0).unwrap()),
                note: Some("Bread".to_string()),
                transfers: vec![make_transfer("john:wallet", "bakery", 419, 100, "€")],
            }],
        };

        let result = verify_ledger_balances(&ledger);
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("ERROR:"));
        assert!(msg.contains("verification balance"));
    }

    #[test]
    fn test_verify_ledger_balances_verification_balances_passes() {
        let ledger = load_ledger("examples/verification-balances.yaml");
        verify_ledger_balances(&ledger).expect("Should pass for verification-balances.yaml");
    }
}
