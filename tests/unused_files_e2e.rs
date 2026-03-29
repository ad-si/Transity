use std::process::Command;

#[test]
fn test_unused_files_e2e() {
    let output = Command::new(env!("CARGO_BIN_EXE_transity"))
        .args(["unused-files", "examples/receipts", "examples/journal.yaml"])
        .output()
        .expect("failed to run transity");

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    assert!(
        combined.contains("2020-01-07t1205_lunch.pdf"),
        "unreferenced file should appear in output, got:\n{}",
        combined
    );
    assert!(
        !combined.contains("2020-01-06t1217_lunch.pdf"),
        "referenced file should NOT appear in output, got:\n{}",
        combined
    );
}
