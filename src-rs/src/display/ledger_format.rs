use crate::data::Ledger;
use crate::utils::utc_to_iso_date_string;

pub fn entries_to_ledger(ledger: &Ledger) -> String {
    let mut results = Vec::new();

    for transaction in &ledger.transactions {
        if let Some(utc) = &transaction.utc {
            let date = utc_to_iso_date_string(utc);
            let note = transaction.note.as_deref().unwrap_or("");

            for transfer in &transaction.transfers {
                let line = format!(
                    "{} {}\n  {}  {}\n  {}\n",
                    date,
                    note,
                    transfer.to,
                    transfer.amount.show_pretty(),
                    transfer.from
                );
                results.push(line);
            }
        }
    }

    results.join("\n")
}
