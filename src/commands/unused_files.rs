use crate::data::Ledger;
use anyhow::Result;
use colored::Colorize;
use std::collections::HashSet;
use std::path::Path;

/// Recursively collect all files under `dir_path`, skipping ".DS_Store"
pub fn get_all_files(dir_path: &Path) -> Result<Vec<std::path::PathBuf>> {
    let mut files = Vec::new();
    for entry in walkdir::WalkDir::new(dir_path) {
        let entry = entry?;
        if entry.file_type().is_file() {
            let name = entry.file_name().to_string_lossy();
            if name != ".DS_Store" {
                files.push(entry.path().to_path_buf());
            }
        }
    }
    Ok(files)
}

fn load_and_verify_ledger(journal_paths: &[&Path]) -> Result<Ledger> {
    let mut combined = Ledger::default();
    for path in journal_paths {
        let content = std::fs::read_to_string(path)?;
        let ledger = Ledger::from_yaml(&content)?;
        combined.transactions.extend(ledger.transactions);
        if combined.owner.is_none() {
            combined.owner = ledger.owner;
        }
        if let Some(new_entities) = ledger.entities {
            combined.entities.get_or_insert_with(Vec::new).extend(new_entities);
        }
    }
    Ok(combined)
}

fn check_file_paths(journal_dir: &Path, ledger: &Ledger) {
    for file_rel in ledger.transactions.iter().flat_map(|t| t.files.iter()) {
        let abs = journal_dir.join(file_rel);
        if !abs.exists() {
            eprintln!(
                "{}",
                format!("Warning: \"{}\" does not exist", abs.display()).yellow()
            );
        }
    }
}

/// Check for unreferenced files in a directory
pub fn check_unused_files(files_dir: &Path, journal_paths: &[&Path]) -> Result<()> {
    let ledger = load_and_verify_ledger(journal_paths)?;

    let journal_dir = journal_paths[0]
        .parent()
        .unwrap_or(Path::new("."));

    check_file_paths(journal_dir, &ledger);

    let found_files = get_all_files(files_dir)?;

    let ledger_files_abs: HashSet<std::path::PathBuf> = ledger
        .transactions
        .iter()
        .flat_map(|t| t.files.iter())
        .map(|f| {
            let p = journal_dir.join(f);
            p.canonicalize().unwrap_or(p)
        })
        .collect();

    let unused: Vec<_> = found_files
        .iter()
        .filter(|f| {
            let canonical = f.canonicalize().unwrap_or_else(|_| f.to_path_buf());
            !ledger_files_abs.contains(&canonical)
        })
        .collect();

    if unused.is_empty() {
        println!(
            "{}",
            format!("No unused files found in {}", files_dir.display()).green()
        );
    } else {
        eprintln!(
            "{}",
            "Warning: Following files are not referenced in the journal".yellow()
        );
        for file in &unused {
            eprintln!("{}", format!("- {}", file.display()).yellow());
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_get_all_files_finds_receipts() {
        let receipts_dir = Path::new("examples/receipts");
        let files = get_all_files(receipts_dir).expect("should list files");
        assert!(!files.is_empty(), "should find receipt files");
        // Check .DS_Store is excluded
        for f in &files {
            assert_ne!(
                f.file_name().unwrap().to_string_lossy().as_ref(),
                ".DS_Store"
            );
        }
        // Check the two known receipts are present
        let names: Vec<String> = files
            .iter()
            .map(|f| f.file_name().unwrap().to_string_lossy().into_owned())
            .collect();
        assert!(
            names.contains(&"2020-01-06t1217_lunch.pdf".to_string()),
            "should find 2020-01-06t1217_lunch.pdf"
        );
        assert!(
            names.contains(&"2020-01-07t1205_lunch.pdf".to_string()),
            "should find 2020-01-07t1205_lunch.pdf"
        );
    }

    #[test]
    fn test_check_unused_files_reports_unreferenced() {
        let receipts_dir = Path::new("examples/receipts")
            .canonicalize()
            .expect("receipts dir exists");
        let journal_path = Path::new("examples/journal.yaml")
            .canonicalize()
            .expect("journal exists");

        let journal_dir = journal_path.parent().unwrap();
        let found_files = get_all_files(&receipts_dir).expect("list files");
        let content = std::fs::read_to_string(&journal_path).expect("read journal");
        let ledger = Ledger::from_yaml(&content).expect("parse ledger");

        let ledger_files_abs: HashSet<std::path::PathBuf> = ledger
            .transactions
            .iter()
            .flat_map(|t| t.files.iter())
            .map(|f| {
                let p = journal_dir.join(f);
                p.canonicalize().unwrap_or(p)
            })
            .collect();

        let unused_names: Vec<String> = found_files
            .iter()
            .filter(|f| {
                let canonical = f.canonicalize().unwrap_or_else(|_| f.to_path_buf());
                !ledger_files_abs.contains(&canonical)
            })
            .map(|f| f.file_name().unwrap().to_string_lossy().into_owned())
            .collect();

        assert!(
            unused_names.contains(&"2020-01-07t1205_lunch.pdf".to_string()),
            "unreferenced file should appear in unused list: {:?}",
            unused_names
        );
        assert!(
            !unused_names.contains(&"2020-01-06t1217_lunch.pdf".to_string()),
            "referenced file should NOT appear in unused list: {:?}",
            unused_names
        );
    }
}
