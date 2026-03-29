use std::process::Command;
use std::str;
use std::sync::OnceLock;

fn strip_ansi(s: &str) -> String {
    static RE: OnceLock<regex::Regex> = OnceLock::new();
    let re = RE.get_or_init(|| regex::Regex::new(r"\x1b\[[0-9;]*m").unwrap());
    re.replace_all(s, "").to_string()
}

fn run_transity(args: &[&str]) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_transity"))
        .args(args)
        .output()
        .expect("Failed to run transity");
    let stdout = str::from_utf8(&output.stdout).unwrap();
    strip_ansi(stdout)
}

#[test]
fn test_balance() {
    let output = run_transity(&["balance", "examples/journal.yaml"]);
    insta::assert_snapshot!("balance", output);
}

#[test]
fn test_balance_multi() {
    let output = run_transity(&[
        "balance",
        "examples/journal.yaml",
        "examples/journal-only-transactions.yaml",
    ]);
    insta::assert_snapshot!("balance_multi", output);
}

#[test]
fn test_balance_all() {
    let output = run_transity(&["balance-all", "examples/journal.yaml"]);
    insta::assert_snapshot!("balance_all", output);
}

#[test]
fn test_transactions() {
    let output = run_transity(&["transactions", "examples/journal.yaml"]);
    insta::assert_snapshot!("transactions", output);
}

#[test]
fn test_transactions_multi() {
    let output = run_transity(&[
        "transactions",
        "examples/journal.yaml",
        "examples/journal-only-transactions.yaml",
    ]);
    insta::assert_snapshot!("transactions_multi", output);
}

#[test]
fn test_transfers() {
    let output = run_transity(&["transfers", "examples/journal.yaml"]);
    insta::assert_snapshot!("transfers", output);
}

#[test]
fn test_entries() {
    let output = run_transity(&["entries", "examples/journal.yaml"]);
    insta::assert_snapshot!("entries", output);
}

#[test]
fn test_entries_by_account() {
    let output = run_transity(&["entries-by-account", "examples/journal.yaml"]);
    insta::assert_snapshot!("entries_by_account", output);
}

#[test]
fn test_entities() {
    let output = run_transity(&["entities", "examples/journal.yaml"]);
    insta::assert_snapshot!("entities", output);
}

#[test]
fn test_entities_sorted() {
    let output = run_transity(&["entities-sorted", "examples/journal.yaml"]);
    insta::assert_snapshot!("entities_sorted", output);
}

#[test]
fn test_ledger_entries() {
    let output = run_transity(&["ledger-entries", "examples/journal.yaml"]);
    insta::assert_snapshot!("ledger_entries", output);
}

#[test]
fn test_csv() {
    let output = run_transity(&["csv", "examples/journal.yaml"]);
    insta::assert_snapshot!("csv", output);
}

#[test]
fn test_tsv() {
    let output = run_transity(&["tsv", "examples/journal.yaml"]);
    insta::assert_snapshot!("tsv", output);
}

#[test]
fn test_unused_files() {
    let output = Command::new(env!("CARGO_BIN_EXE_transity"))
        .args(["unused-files", "examples/receipts", "examples/journal.yaml"])
        .output()
        .expect("Failed to run transity");
    let stderr = strip_ansi(str::from_utf8(&output.stderr).unwrap());
    assert!(
        stderr.contains("2020-01-07t1205_lunch.pdf"),
        "Expected unreferenced file in stderr, got: {}",
        stderr
    );
    assert!(
        !stderr.contains("2020-01-06t1217_lunch.pdf"),
        "Referenced file incorrectly flagged: {}",
        stderr
    );
}

#[test]
fn test_broken_journal_fails() {
    let status = Command::new(env!("CARGO_BIN_EXE_transity"))
        .args([
            "balance",
            "examples/journal.yaml",
            "examples/journal-broken-transaction.yaml",
        ])
        .status()
        .expect("Failed to run transity");
    assert!(
        !status.success(),
        "Expected non-zero exit for broken journal"
    );
}

#[test]
fn test_version() {
    let output = run_transity(&["--version"]);
    assert!(
        output.trim().contains('.'),
        "Version should contain dots: {}",
        output.trim()
    );
}

#[test]
fn test_help() {
    let output = run_transity(&["--help"]);
    let has_commands = output.contains("balance")
        || output.contains("COMMANDS")
        || output.contains("Commands:");
    assert!(has_commands, "Help should mention commands: {}", output);
}
