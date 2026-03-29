use std::fs;
use transity::data::parse_ledger;
use transity::display::entities::show_entities;
use transity::display::ledger_format::entries_to_ledger;
use transity::utils::SortOrder;

fn load_journal() -> transity::data::Ledger {
    let yaml = fs::read_to_string("../examples/journal.yaml")
        .expect("examples/journal.yaml not found");
    parse_ledger(&yaml).expect("Failed to parse journal.yaml")
}

fn load_snapshot(name: &str) -> String {
    fs::read_to_string(format!("../test/snapshots/{}", name))
        .unwrap_or_else(|_| panic!("Snapshot file not found: {}", name))
}

#[test]
fn test_entities_custom_sort() {
    let ledger = load_journal();
    let result = show_entities(&SortOrder::CustomSort, &ledger);
    let expected = load_snapshot("entities.txt");
    assert_eq!(
        result.trim_end_matches('\n'),
        expected.trim_end_matches('\n')
    );
}

#[test]
fn test_entities_sorted() {
    let ledger = load_journal();
    let result = show_entities(&SortOrder::Alphabetically, &ledger);
    let expected = load_snapshot("entities-sorted.txt");
    assert_eq!(
        result.trim_end_matches('\n'),
        expected.trim_end_matches('\n')
    );
}

#[test]
fn test_entries_to_ledger() {
    let ledger = load_journal();
    let result = entries_to_ledger(&ledger);
    let expected = load_snapshot("ledger-entries.txt");
    assert_eq!(
        result.trim_end_matches('\n'),
        expected.trim_end_matches('\n')
    );
}
