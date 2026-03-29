use transity::data::ledger::Ledger;
use transity::display::balance::{show_balance, BalanceFilter};
use transity::display::transfers::{show_transfers, show_pretty_aligned};

fn load_journal() -> Ledger {
    let yaml = include_str!("../examples/journal.yaml");
    Ledger::from_yaml(yaml).expect("Failed to parse journal.yaml")
}

fn load_snapshot(name: &str) -> String {
    std::fs::read_to_string(format!("test/snapshots/{}.txt", name))
        .unwrap_or_else(|e| panic!("Cannot read snapshot {}.txt: {}", name, e))
}

#[test]
fn test_balance_only_owner() {
    let ledger = load_journal();
    let result = show_balance(&BalanceFilter::OnlyOwner, &ledger);
    let expected = load_snapshot("balance");
    assert_eq!(
        result, expected,
        "balance output does not match snapshot"
    );
}

#[test]
fn test_balance_all() {
    let ledger = load_journal();
    let result = show_balance(&BalanceFilter::All, &ledger);
    let expected = load_snapshot("balance-all");
    assert_eq!(
        result, expected,
        "balance-all output does not match snapshot"
    );
}

#[test]
fn test_show_transfers() {
    let ledger = load_journal();
    let result = show_transfers(&ledger);
    let expected = load_snapshot("transfers");
    assert_eq!(
        result, expected,
        "transfers output does not match snapshot"
    );
}

#[test]
fn test_show_pretty_aligned() {
    let ledger = load_journal();
    let result = show_pretty_aligned(&ledger);
    let expected = load_snapshot("transactions");
    assert_eq!(
        result, expected,
        "transactions output does not match snapshot"
    );
}
