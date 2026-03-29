use crate::data::ledger::Ledger;

pub fn show_transfers(ledger: &Ledger) -> String {
    let header = format!(
        "Journal for \"{}\"\n{}\n",
        ledger.owner.as_deref().unwrap_or("UNKNOWN"),
        "=".repeat(80)
    );

    let transfers_str: String = ledger
        .transactions
        .iter()
        .flat_map(|t| t.to_transfers())
        .map(|t| t.show_pretty_colorized())
        .collect();

    header + &transfers_str + "\n\n"
}

pub fn show_pretty_aligned(ledger: &Ledger) -> String {
    let header = format!(
        "Journal for \"{}\"\n{}\n",
        ledger.owner.as_deref().unwrap_or("UNKNOWN"),
        "=".repeat(80)
    );

    let transactions_str: String = ledger
        .transactions
        .iter()
        .map(|t| t.show_pretty_aligned())
        .collect();

    header + &transactions_str + "\n\n"
}
