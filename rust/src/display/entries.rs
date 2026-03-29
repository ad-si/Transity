use crate::data::{format_quantity, Ledger};
use chrono::NaiveDateTime;

fn iso_string(dt: &NaiveDateTime) -> String {
    dt.format("%Y-%m-%dT%H:%M:%S").to_string()
}

/// Build the flat entry rows from the ledger.
///
/// Each row is `[iso_date, account, quantity_str, commodity]`.
/// Returns `None` if any transfer lacks a UTC timestamp.
pub fn get_entries(ledger: &Ledger) -> Option<Vec<Vec<String>>> {
    let mut rows: Vec<Vec<String>> = Vec::new();

    for txn in &ledger.transactions {
        for transfer in &txn.transfers {
            let utc = transfer.utc.or(txn.utc)?;
            let date = iso_string(&utc);

            let negated = transfer.amount.negate();
            rows.push(vec![
                date.clone(),
                transfer.from.clone(),
                format_quantity(&negated.quantity),
                negated.commodity.clone(),
            ]);
            rows.push(vec![
                date,
                transfer.to.clone(),
                format_quantity(&transfer.amount.quantity),
                transfer.amount.commodity.clone(),
            ]);
        }
    }

    // Zero-amount balances represent initial entity state (not transactions).
    for entity in &ledger.entities {
        for account in &entity.accounts {
            // Strip `:_default_` suffix to match PureScript output.
            let display_id = format!("{}:{}", entity.id, account.id)
                .trim_end_matches(":_default_")
                .to_string();
            for balance in &account.balances {
                let Some(utc) = balance.utc else { continue };
                let date = iso_string(&utc);
                for amount in &balance.amounts {
                    if amount.is_zero() {
                        rows.push(vec![
                            date.clone(),
                            display_id.clone(),
                            format_quantity(&amount.quantity),
                            amount.commodity.clone(),
                        ]);
                    }
                }
            }
        }
    }

    Some(rows)
}

/// Render entries as separated fields, one row per line.
///
/// Fields within each row are joined with `separator`.
/// Rows are sorted lexicographically (date first).
/// Output ends with a trailing newline.
pub fn show_entries(separator: &str, ledger: &Ledger) -> Option<String> {
    let mut entries = get_entries(ledger)?;
    entries.sort();
    let lines: Vec<String> = entries.iter().map(|row| row.join(separator)).collect();
    let mut result = lines.join("\n");
    result.push_str("\n\n");
    Some(result)
}

/// `account commodity` key for a row — used for grouping and sorting.
fn acc_com_key(row: &[String]) -> String {
    format!(
        "{} {}",
        row.get(1).map(String::as_str).unwrap_or(""),
        row.get(3).map(String::as_str).unwrap_or("")
    )
}

/// Render entries grouped by account + commodity.
///
/// Each group is preceded by a `"account commodity"` header. Groups are
/// separated by two blank lines; within each group entries are sorted by date.
pub fn show_entries_by_account(ledger: &Ledger) -> Option<String> {
    let mut sorted = get_entries(ledger)?;
    sorted.sort_by(|a, b| acc_com_key(a).cmp(&acc_com_key(b)));

    let mut groups: Vec<Vec<Vec<String>>> = Vec::new();
    let mut current_key = String::new();
    for row in sorted {
        let key = acc_com_key(&row);
        if key != current_key {
            groups.push(Vec::new());
            current_key = key;
        }
        groups.last_mut().unwrap().push(row);
    }

    let group_strings: Vec<String> = groups
        .into_iter()
        .map(|mut group| {
            group.sort_by(|a, b| a[0].cmp(&b[0]));
            let header = format!("\"{}\"", acc_com_key(&group[0]));
            let mut lines = vec![header];
            for row in group {
                lines.push(row.join(" "));
            }
            lines.join("\n")
        })
        .collect();

    let mut result = group_strings.join("\n\n\n");
    result.push_str("\n\n");
    Some(result)
}
