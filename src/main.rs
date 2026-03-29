use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::Colorize;
use std::path::{Path, PathBuf};
use transity::*;

// ─── CLI ─────────────────────────────────────────────────────────────────────

#[derive(Parser)]
#[command(
    name = "transity",
    version = "0.8.0",
    about = "Transity is a full fledged, CLI based, plain text accounting tool."
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Simple balance of the owner's accounts
    Balance {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// Simple balance of all accounts
    BalanceAll {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// All transactions and their transfers
    Transactions {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// All transfers with one transfer per line
    Transfers {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// All individual deposits & withdrawals, space separated
    Entries {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// [WIP] List all referenced entities
    Entities {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// [WIP] List all referenced entities sorted alphabetically
    EntitiesSorted {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// All entries in Ledger format
    LedgerEntries {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// Transfers, comma separated (printed to stdout)
    Csv {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// Transfers, tab separated (printed to stdout)
    Tsv {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// XLSX file with all transfers (printed to stdout)
    Xlsx {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// All individual deposits & withdrawals, grouped by account
    EntriesByAccount {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// Code and data for gnuplot impulse diagram
    Gplot {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// Code and data for cumulative gnuplot step chart
    GplotCumul {
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
    /// Recursively list all files in a directory which are not referenced in the journal
    UnusedFiles {
        directory: String,
        journal: String,
        #[arg(trailing_var_arg = true)]
        extra: Vec<String>,
    },
}

// ─── MAIN ─────────────────────────────────────────────────────────────────────

const UTC_ERROR: &str = "All transfers or their parent transaction must have a valid UTC field";

fn collect_paths(journal: &str, extra: &[String]) -> Vec<PathBuf> {
    let mut paths = vec![PathBuf::from(journal)];
    paths.extend(extra.iter().map(PathBuf::from));
    paths
}

fn get_all_files(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut result = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            result.extend(get_all_files(&path)?);
        } else {
            result.push(path);
        }
    }
    Ok(result)
}

fn check_unused_files(directory: &Path, journal_paths: &[PathBuf]) -> Result<()> {
    let ledger = load_and_verify(journal_paths)?;

    // Determine journal directory from first path
    let journal_path = &journal_paths[0];
    let journal_dir = if journal_path.to_string_lossy().starts_with("/dev/fd/") {
        std::env::current_dir()?
    } else {
        journal_path
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf()
    };

    // Collect referenced files (resolved relative to journal directory)
    let referenced: std::collections::HashSet<PathBuf> = ledger
        .transactions
        .iter()
        .flat_map(|tx| tx.files.iter())
        .map(|f| {
            journal_dir
                .join(f)
                .canonicalize()
                .unwrap_or_else(|_| journal_dir.join(f))
        })
        .collect();

    let dir_canonical = directory
        .canonicalize()
        .unwrap_or_else(|_| directory.to_path_buf());
    let all_files = get_all_files(&dir_canonical)?;

    let unused: Vec<&PathBuf> = all_files
        .iter()
        .filter(|f| !referenced.contains(*f))
        .collect();

    if unused.is_empty() {
        println!(
            "{}",
            format!("No unused files found in {}", dir_canonical.display()).green()
        );
    } else {
        eprintln!(
            "{}",
            "Warning: Following files are not referenced in the journal".yellow()
        );
        for f in &unused {
            eprintln!("{}", format!("- {}", f.display()).yellow());
        }
    }

    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Balance { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            print!("{}", show_balance(BalanceFilter::OnlyOwner, true, &ledger));
        }

        Commands::BalanceAll { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            print!("{}", show_balance(BalanceFilter::All, true, &ledger));
        }

        Commands::Transactions { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            println!("{}", show_pretty_aligned(true, &ledger));
        }

        Commands::Transfers { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            println!("{}", show_transfers(true, &ledger));
        }

        Commands::Entries { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match show_entries(" ", &ledger) {
                Some(s) => println!("{}", s),
                None => {
                    eprintln!("{}", UTC_ERROR.red());
                    std::process::exit(1);
                }
            }
        }

        Commands::Entities { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            println!("{}", show_entities(false, &ledger));
        }

        Commands::EntitiesSorted { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            println!("{}", show_entities(true, &ledger));
        }

        Commands::LedgerEntries { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            println!("{}", entries_to_ledger(&ledger));
        }

        Commands::Csv { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match show_entries(",", &ledger) {
                Some(s) => println!("{}", s),
                None => {
                    eprintln!("{}", UTC_ERROR.red());
                    std::process::exit(1);
                }
            }
        }

        Commands::Tsv { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match show_entries("\t", &ledger) {
                Some(s) => println!("{}", s),
                None => {
                    eprintln!("{}", UTC_ERROR.red());
                    std::process::exit(1);
                }
            }
        }

        Commands::Xlsx { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match entries_as_xlsx(&ledger) {
                Ok(bytes) => {
                    use std::io::Write;
                    std::io::stdout().write_all(&bytes)?;
                }
                Err(e) => {
                    eprintln!("{}", e.to_string().red());
                    std::process::exit(1);
                }
            }
        }

        Commands::EntriesByAccount { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match show_entries_by_account(&ledger) {
                Some(s) => println!("{}", s),
                None => {
                    eprintln!("{}", UTC_ERROR.red());
                    std::process::exit(1);
                }
            }
        }

        Commands::Gplot { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match show_entries_by_account(&ledger) {
                Some(data) => println!("{}", gplot_code(&data, &journal)),
                None => {
                    eprintln!("{}", UTC_ERROR.red());
                    std::process::exit(1);
                }
            }
        }

        Commands::GplotCumul { journal, extra } => {
            let paths = collect_paths(&journal, &extra);
            let ledger = load_and_verify(&paths).unwrap_or_else(|e| {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            });
            match show_entries_by_account(&ledger) {
                Some(data) => {
                    let title = format!("{} - Cumulative", journal);
                    println!("{}", gplot_code_cumul(&data, &title));
                }
                None => {
                    eprintln!("{}", UTC_ERROR.red());
                    std::process::exit(1);
                }
            }
        }

        Commands::UnusedFiles {
            directory,
            journal,
            extra,
        } => {
            let journal_paths = collect_paths(&journal, &extra);
            if let Err(e) = check_unused_files(Path::new(&directory), &journal_paths) {
                eprintln!("{}", e.to_string().red());
                std::process::exit(1);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use num_rational::BigRational;
    use transity::*;

    fn br(num: i64, den: i64) -> BigRational {
        BigRational::new(BigInt::from(num), BigInt::from(den))
    }

    fn make_amount(num: i64, den: i64, commodity: &str) -> Amount {
        Amount {
            quantity: br(num, den),
            commodity: commodity.to_string(),
        }
    }

    fn parse_ledger(yaml: &str) -> Ledger {
        let raw: LedgerRaw = serde_yaml::from_str(yaml).expect("YAML parse failed");
        Ledger::from_raw(raw).expect("Ledger::from_raw failed")
    }

    // ─── digits_to_rational ──────────────────────────────────────────────────

    #[test]
    fn digits_to_rational_137() {
        assert_eq!(digits_to_rational("137"), Some(br(137, 1)));
    }

    #[test]
    fn digits_to_rational_13() {
        assert_eq!(digits_to_rational("13"), Some(br(13, 1)));
    }

    #[test]
    fn digits_to_rational_3() {
        assert_eq!(digits_to_rational("3"), Some(br(3, 1)));
    }

    #[test]
    fn digits_to_rational_0() {
        assert_eq!(digits_to_rational("0"), Some(br(0, 1)));
    }

    #[test]
    fn digits_to_rational_0_3() {
        assert_eq!(digits_to_rational("0.3"), Some(br(3, 10)));
    }

    #[test]
    fn digits_to_rational_dot_3() {
        assert_eq!(digits_to_rational(".3"), Some(br(3, 10)));
    }

    #[test]
    fn digits_to_rational_0_300_reduces() {
        assert_eq!(digits_to_rational("0.300"), Some(br(3, 10)));
    }

    #[test]
    fn digits_to_rational_2_1() {
        assert_eq!(digits_to_rational("2.1"), Some(br(21, 10)));
    }

    #[test]
    fn digits_to_rational_3_21() {
        assert_eq!(digits_to_rational("3.21"), Some(br(321, 100)));
    }

    #[test]
    fn digits_to_rational_12_3456() {
        assert_eq!(digits_to_rational("12.3456"), Some(br(123456, 10000)));
    }

    #[test]
    fn digits_to_rational_neg_0_3() {
        assert_eq!(digits_to_rational("-0.3"), Some(br(-3, 10)));
    }

    #[test]
    fn digits_to_rational_abc_returns_none() {
        assert_eq!(digits_to_rational("abc"), None);
    }

    #[test]
    fn digits_to_rational_big_int_precision() {
        // "1.5555555555" (10 fives) — exercises BigInt rather than native int
        let result = digits_to_rational("1.5555555555");
        assert!(result.is_some(), "Expected Some for big-int rational");
        let r = result.unwrap();
        // 15555555555 / 10000000000 reduces to 3111111111 / 2000000000
        let expected = BigRational::new(BigInt::from(3111111111i64), BigInt::from(2000000000i64));
        assert_eq!(r, expected);
    }

    // ─── parse_amount ────────────────────────────────────────────────────────

    #[test]
    fn parse_amount_valid() {
        let result = parse_amount("15 €").unwrap();
        assert_eq!(result, make_amount(15, 1, "€"));
    }

    #[test]
    fn parse_amount_missing_commodity_fails() {
        assert!(parse_amount("15").is_err());
    }

    #[test]
    fn amount_negate() {
        let a = make_amount(15, 1, "€");
        assert_eq!(a.negate(), make_amount(-15, 1, "€"));
    }

    #[test]
    fn amount_subtract_same_commodity() {
        let a = make_amount(10, 1, "€");
        let b = make_amount(3, 1, "€");
        // subtract: a - b = a.add(b.negate())
        let result = a.add(&b.negate());
        assert_eq!(result, make_amount(7, 1, "€"));
    }

    #[test]
    fn amount_is_zero_true() {
        let a = make_amount(0, 1, "€");
        assert!(a.is_zero());
    }

    #[test]
    fn amount_is_zero_false() {
        let a = make_amount(1, 1, "€");
        assert!(!a.is_zero());
    }

    // ─── CommodityMap ────────────────────────────────────────────────────────

    #[test]
    fn commodity_map_add_accumulates_same_commodity() {
        let mut map = CommodityMap::new();
        commodity_map_add(&mut map, make_amount(10, 1, "€"));
        commodity_map_add(&mut map, make_amount(5, 1, "€"));
        commodity_map_add(&mut map, make_amount(3, 1, "$"));
        assert_eq!(map.get("€"), Some(&make_amount(15, 1, "€")));
        assert_eq!(map.get("$"), Some(&make_amount(3, 1, "$")));
    }

    #[test]
    fn commodity_map_subtract_reduces() {
        let mut map = CommodityMap::new();
        commodity_map_add(&mut map, make_amount(42, 1, "€"));
        commodity_map_subtract(&mut map, &make_amount(5, 1, "€"));
        assert_eq!(map.get("€"), Some(&make_amount(37, 1, "€")));
    }

    // ─── pad_start / pad_end / indent_subsequent / align_number ─────────────

    #[test]
    fn pad_start_pads_left() {
        assert_eq!(pad_start(5, "ab"), "   ab");
    }

    #[test]
    fn pad_end_pads_right() {
        assert_eq!(pad_end(5, "ab"), "ab   ");
    }

    #[test]
    fn indent_subsequent_indents_lines_after_first() {
        assert_eq!(
            indent_subsequent(2, "line1\nline2\nline3"),
            "line1\n  line2\n  line3"
        );
    }

    #[test]
    fn align_number_positive_contains_int_and_frac() {
        let result = align_number(false, 5, 3, 42.5);
        assert!(result.contains("42"), "Expected '42' in '{}'", result);
        assert!(result.contains(".5"), "Expected '.5' in '{}'", result);
    }

    #[test]
    fn align_number_negative_contains_value() {
        let result = align_number(false, 5, 3, -7.0);
        assert!(result.contains("-7"), "Expected '-7' in '{}'", result);
    }

    // ─── length_of_num_parts ─────────────────────────────────────────────────

    #[test]
    fn length_of_num_parts_integer() {
        let (i, f) = length_of_num_parts(42.0);
        assert_eq!(i, 2);
        assert_eq!(f, 0);
    }

    #[test]
    fn length_of_num_parts_decimal() {
        // 3.14 → int part "3" (len 1), frac part ".14" (len 3 incl dot)
        let (i, f) = length_of_num_parts(3.14);
        assert_eq!(i, 1);
        assert_eq!(f, 3);
    }

    // ─── WidthRecord merge ───────────────────────────────────────────────────

    #[test]
    fn width_record_merge_takes_max() {
        let a = WidthRecord {
            account: 5,
            integer: 3,
            fraction: 0,
            commodity: 0,
        };
        let b = WidthRecord {
            account: 2,
            integer: 7,
            fraction: 0,
            commodity: 0,
        };
        let m = a.merge(&b);
        assert_eq!(m.account, 5);
        assert_eq!(m.integer, 7);
        assert_eq!(m.fraction, 0);
        assert_eq!(m.commodity, 0);
    }

    // ─── Date formatting ─────────────────────────────────────────────────────

    #[test]
    fn utc_to_iso_string_formats_correctly() {
        // parse_datetime does not support the Z suffix; use space separator
        let dt = parse_datetime("2014-12-24 10:30:45").unwrap();
        assert_eq!(utc_to_iso_string(&dt), "2014-12-24T10:30:45");
    }

    #[test]
    fn utc_to_iso_date_string_formats_correctly() {
        let dt = parse_datetime("2014-12-24 10:30:45").unwrap();
        assert_eq!(utc_to_iso_date_string(&dt), "2014-12-24");
    }

    #[test]
    fn date_show_pretty_formats_without_seconds() {
        let dt = parse_datetime("2014-12-24 10:30:00").unwrap();
        assert_eq!(date_show_pretty(&dt), "2014-12-24 10:30");
    }

    // ─── Ledger YAML parsing ─────────────────────────────────────────────────

    #[test]
    fn ledger_from_yaml_simple() {
        let yaml = r#"
owner: John Doe
entities:
  - id: anna
    accounts:
      - id: wallet
  - id: ben
    accounts:
      - id: wallet
transactions:
  - utc: '2005-01-01 12:00'
    transfers:
      - from: ben:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let ledger = parse_ledger(yaml);
        assert_eq!(ledger.owner, Some("John Doe".to_string()));
        assert_eq!(ledger.entities.len(), 2);
        assert_eq!(ledger.transactions.len(), 1);
        assert_eq!(
            ledger.transactions[0].transfers[0].amount,
            make_amount(3, 1, "€")
        );
    }

    #[test]
    fn ledger_merge_keeps_first_owner_merges_transactions() {
        let yaml1 = r#"
owner: John Doe
transactions:
  - utc: '2005-01-01'
    transfers:
      - from: john:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let yaml2 = r#"
owner: Anna Smith
transactions:
  - utc: '2006-01-01'
    transfers:
      - from: anna:wallet
        to: john:wallet
        amount: 5 €
"#;
        let l1 = parse_ledger(yaml1);
        let l2 = parse_ledger(yaml2);
        let combined = l1.merge(l2);
        assert_eq!(combined.owner, Some("John Doe".to_string()));
        assert_eq!(combined.transactions.len(), 2);
    }

    // ─── verify_accounts ─────────────────────────────────────────────────────

    #[test]
    fn verify_accounts_passes_when_all_declared() {
        let yaml = r#"
owner: John Doe
entities:
  - id: anna
    accounts:
      - id: wallet
  - id: ben
    accounts:
      - id: wallet
transactions:
  - utc: '2005-01-01 12:00'
    transfers:
      - from: ben:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_accounts(&ledger).is_ok());
    }

    #[test]
    fn verify_accounts_fails_with_undeclared_account() {
        let yaml = r#"
owner: John Doe
entities:
  - id: anna
    accounts:
      - id: wallet
transactions:
  - utc: '2005-01-01 12:00'
    transfers:
      - from: undeclared:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_accounts(&ledger).is_err());
    }

    // ─── verify_ledger_balances ───────────────────────────────────────────────

    #[test]
    fn verify_ledger_balances_passes_with_no_balances() {
        let yaml = r#"
owner: John Doe
transactions:
  - utc: '2005-01-01'
    transfers:
      - from: john:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_ledger_balances(&ledger).is_ok());
    }

    #[test]
    fn verify_ledger_balances_fails_when_incorrect() {
        let yaml = r#"
owner: John Doe
entities:
  - id: anna
    accounts:
      - id: wallet
        balances:
          - utc: '2000-01-01 12:00'
            amounts: []
          - utc: '2010-01-01 12:00'
            amounts: ['5 €']
  - id: ben
    accounts:
      - id: wallet
transactions:
  - utc: '2005-01-01 12:00'
    transfers:
      - from: ben:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_ledger_balances(&ledger).is_err());
    }

    #[test]
    fn verify_ledger_balances_passes_when_correct() {
        let yaml = r#"
owner: John Doe
entities:
  - id: anna
    accounts:
      - id: wallet
        balances:
          - utc: '2000-01-01 12:00'
            amounts: []
          - utc: '2010-01-01 12:00'
            amounts: ['3 €']
  - id: ben
    accounts:
      - id: wallet
transactions:
  - utc: '2005-01-01 12:00'
    transfers:
      - from: ben:wallet
        to: anna:wallet
        amount: 3 €
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_ledger_balances(&ledger).is_ok());
    }

    #[test]
    fn verify_ledger_balances_passes_at_different_utcs() {
        // Balance checkpoints are non-destructive: passing a check does not reset
        // the running balance. Each checkpoint verifies the cumulative state.
        let yaml = r#"
owner: John Doe
entities:
  - id: anna
    accounts:
      - id: wallet
        balances:
          - utc: '2000-01-01 12:00'
            amounts: []
          - utc: '2006-01-01 12:00'
            amounts: ['3 €']
          - utc: '2010-01-01 12:00'
            amounts: ['4 $']
  - id: ben
    accounts:
      - id: wallet
transactions:
  - utc: '2005-01-01 12:00'
    transfers:
      - from: ben:wallet
        to: anna:wallet
        amount: 3 €
  - utc: '2007-01-01 12:00'
    transfers:
      - from: ben:wallet
        to: anna:wallet
        amount: 4 $
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_ledger_balances(&ledger).is_ok());
    }

    #[test]
    fn verify_ledger_balances_sequential_checkpoints_do_not_drain_account() {
        // Regression: a passing balance checkpoint must not drain the running
        // balance. Prior to the fix, the balancing transfer was applied to the
        // balance map even on success, so each subsequent checkpoint saw a zero
        // baseline instead of the real cumulative balance.
        let yaml = r#"
owner: alice
entities:
  - id: alice
    accounts:
      - id: depot
        balances:
          - utc: '2010-01-01'
            amounts: [0 FUND]
          - utc: '2010-07-01'
            amounts: [10 FUND]
          - utc: '2011-01-01'
            amounts: [25 FUND]
  - id: broker
    accounts:
      - id: account
transactions:
  - utc: '2010-04-01'
    transfers:
      - from: broker:account
        to: alice:depot
        amount: 10 FUND
  - utc: '2010-09-01'
    transfers:
      - from: broker:account
        to: alice:depot
        amount: 15 FUND
"#;
        let ledger = parse_ledger(yaml);
        assert!(verify_ledger_balances(&ledger).is_ok());
    }

    // ─── show_balance ─────────────────────────────────────────────────────────

    fn simple_ledger() -> Ledger {
        // Mirrors Fixtures.ledger: owner=John Doe, one transaction transferSimple
        let yaml = r#"
owner: John Doe
transactions:
  - utc: '2014-12-24'
    transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
        note: A note with special chars like < and &
"#;
        parse_ledger(yaml)
    }

    fn owner_ledger() -> Ledger {
        // Ledger where the owner name matches the account prefix, so OnlyOwner filter works
        let yaml = r#"
owner: john
transactions:
  - utc: '2014-12-24'
    transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
"#;
        parse_ledger(yaml)
    }

    #[test]
    fn show_balance_owner_only_filter_returns_owner_accounts() {
        // OnlyOwner uses ledger.owner as the prefix filter; owner must match account prefix
        let ledger = owner_ledger();
        let result = show_balance(BalanceFilter::OnlyOwner, false, &ledger);
        assert!(
            result.contains("john:giro"),
            "Expected 'john:giro' in: {}",
            result
        );
        assert!(
            !result.contains("evil-corp"),
            "Did not expect 'evil-corp' in: {}",
            result
        );
    }

    #[test]
    fn show_balance_all_filter_returns_all_accounts() {
        let ledger = simple_ledger();
        let result = show_balance(BalanceFilter::All, false, &ledger);
        assert!(
            result.contains("john:giro"),
            "Expected 'john:giro' in: {}",
            result
        );
        assert!(
            result.contains("evil-corp"),
            "Expected 'evil-corp' in: {}",
            result
        );
    }

    #[test]
    fn show_balance_non_matching_owner_returns_empty() {
        // When owner is "nonexistent" and no accounts match, result should be just whitespace/newlines
        let yaml = r#"
owner: nonexistent
transactions:
  - utc: '2014-12-24'
    transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
"#;
        let ledger = parse_ledger(yaml);
        let result = show_balance(BalanceFilter::OnlyOwner, false, &ledger);
        assert!(
            result.trim().is_empty(),
            "Expected empty for non-matching owner, got: {:?}",
            result
        );
    }

    #[test]
    fn show_balance_multiple_transactions_accumulate() {
        let yaml = r#"
owner: John Doe
transactions:
  - utc: '2014-12-24'
    transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
  - utc: '2015-01-01'
    transfers:
      - from: flower-power
        to: evil-corp
        amount: 7 €
"#;
        let ledger = parse_ledger(yaml);
        let result = show_balance(BalanceFilter::All, false, &ledger);
        // evil-corp should show 22 € total (15 + 7)
        assert!(result.contains("22"), "Expected '22' in: {}", result);
    }

    // ─── show_transfers ───────────────────────────────────────────────────────

    #[test]
    fn show_transfers_contains_john_giro() {
        let ledger = simple_ledger();
        let result = show_transfers(false, &ledger);
        assert!(
            result.contains("john:giro"),
            "Expected 'john:giro' in: {}",
            result
        );
    }

    // ─── show_entities ────────────────────────────────────────────────────────

    #[test]
    fn show_entities_no_entities_returns_message() {
        let ledger = simple_ledger();
        let result = show_entities(false, &ledger);
        assert_eq!(result, "Journal does not contain any entities");
    }

    #[test]
    fn show_entities_alphabetical_sort() {
        let yaml = r#"
owner: John Doe
entities:
  - id: Zara
  - id: Anna
  - id: mike
transactions: []
"#;
        let ledger = parse_ledger(yaml);
        let result = show_entities(true, &ledger);
        let anna_pos = result.find("Anna").expect("Anna not found");
        let mike_pos = result.find("mike").expect("mike not found");
        let zara_pos = result.find("Zara").expect("Zara not found");
        assert!(anna_pos < mike_pos, "Anna should come before mike");
        assert!(mike_pos < zara_pos, "mike should come before Zara");
    }

    #[test]
    fn show_entities_custom_sort_preserves_order() {
        let yaml = r#"
owner: John Doe
entities:
  - id: Anna
  - id: Bob
transactions: []
"#;
        let ledger = parse_ledger(yaml);
        let result = show_entities(false, &ledger);
        let anna_pos = result.find("Anna").expect("Anna not found");
        let bob_pos = result.find("Bob").expect("Bob not found");
        assert!(
            anna_pos < bob_pos,
            "Anna should appear before Bob in custom order"
        );
    }

    // ─── get_entries / show_entries / show_entries_by_account ────────────────

    #[test]
    fn get_entries_returns_none_when_no_utc() {
        let yaml = r#"
owner: Test
transactions:
  - transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
"#;
        let ledger = parse_ledger(yaml);
        assert_eq!(get_entries(&ledger), None);
    }

    #[test]
    fn get_entries_returns_some_when_valid() {
        let ledger = simple_ledger();
        assert!(get_entries(&ledger).is_some());
    }

    #[test]
    fn show_entries_content_contains_account_and_commodity() {
        let ledger = simple_ledger();
        let result = show_entries(",", &ledger);
        assert!(result.is_some());
        let s = result.unwrap();
        assert!(s.contains("john:giro"), "Expected 'john:giro' in: {}", s);
        assert!(s.contains("€"), "Expected '€' in: {}", s);
    }

    #[test]
    fn show_entries_by_account_groups_by_account() {
        let ledger = simple_ledger();
        let result = show_entries_by_account(&ledger);
        assert!(result.is_some());
        let s = result.unwrap();
        assert!(s.contains("john:giro"), "Expected 'john:giro' in: {}", s);
        assert!(s.contains("evil-corp"), "Expected 'evil-corp' in: {}", s);
    }

    // ─── entries_to_ledger ────────────────────────────────────────────────────

    #[test]
    fn entries_to_ledger_format() {
        let ledger = simple_ledger();
        let result = entries_to_ledger(&ledger);
        // Expected from Fixtures.ledgerLedger:
        // "2014-12-24 A short note about this transaction\n  evil-corp  15 €\n  john:giro\n"
        // Our simple_ledger has no note, so note field is empty
        assert!(
            result.contains("2014-12-24"),
            "Expected date in: {}",
            result
        );
        assert!(
            result.contains("evil-corp"),
            "Expected 'evil-corp' in: {}",
            result
        );
        assert!(
            result.contains("john:giro"),
            "Expected 'john:giro' in: {}",
            result
        );
        assert!(result.contains("15"), "Expected amount in: {}", result);
        assert!(result.contains("€"), "Expected commodity in: {}", result);
    }

    #[test]
    fn entries_to_ledger_with_note() {
        let yaml = r#"
owner: John Doe
transactions:
  - utc: '2014-12-24'
    note: A short note about this transaction
    transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
"#;
        let ledger = parse_ledger(yaml);
        let result = entries_to_ledger(&ledger);
        assert!(
            result.contains("A short note about this transaction"),
            "Expected note in: {}",
            result
        );
    }

    // ─── Transaction UTC promotion ────────────────────────────────────────────

    #[test]
    fn transfers_with_date_promotes_transaction_utc() {
        let yaml = r#"
owner: John Doe
transactions:
  - utc: '2014-12-24'
    transfers:
      - from: john:giro
        to: evil-corp
        amount: 15 €
"#;
        let ledger = parse_ledger(yaml);
        let tx = &ledger.transactions[0];
        let transfers = tx.transfers_with_date();
        // Transfer has no utc of its own; should inherit transaction utc
        let utc = transfers[0].utc.expect("UTC should be set");
        assert_eq!(utc_to_iso_date_string(&utc), "2014-12-24");
    }

    #[test]
    fn transfers_with_date_preserves_transfer_utc() {
        let yaml = r#"
owner: John Doe
transactions:
  - utc: '2014-12-24'
    transfers:
      - utc: '2014-12-25'
        from: john:giro
        to: evil-corp
        amount: 15 €
"#;
        let ledger = parse_ledger(yaml);
        let tx = &ledger.transactions[0];
        let transfers = tx.transfers_with_date();
        let utc = transfers[0].utc.expect("UTC should be set");
        // Transfer's own utc takes precedence
        assert_eq!(utc_to_iso_date_string(&utc), "2014-12-25");
    }

    // ─── Transfer verify ──────────────────────────────────────────────────────

    #[test]
    fn transfer_from_raw_fails_if_from_empty() {
        let raw = TransferRaw {
            utc: None,
            from: "".to_string(),
            to: "anna".to_string(),
            amount: "5 €".to_string(),
            note: None,
        };
        assert!(Transfer::from_raw(&raw).is_err());
    }

    #[test]
    fn transfer_from_raw_fails_if_to_empty() {
        let raw = TransferRaw {
            utc: None,
            from: "john".to_string(),
            to: "".to_string(),
            amount: "5 €".to_string(),
            note: None,
        };
        assert!(Transfer::from_raw(&raw).is_err());
    }

    #[test]
    fn transfer_from_raw_fails_if_amount_zero() {
        let raw = TransferRaw {
            utc: None,
            from: "john".to_string(),
            to: "anna".to_string(),
            amount: "0 €".to_string(),
            note: None,
        };
        assert!(Transfer::from_raw(&raw).is_err());
    }

    // ─── add_account_default ─────────────────────────────────────────────────

    #[test]
    fn add_account_default_single_segment() {
        assert_eq!(add_account_default("john"), "john:_default_");
    }

    #[test]
    fn add_account_default_multi_segment_unchanged() {
        assert_eq!(add_account_default("john:giro"), "john:giro");
    }
}
