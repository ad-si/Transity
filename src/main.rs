use std::env;
use std::io::Write;

use transity::data::Ledger;
use transity::display::xlsx::entries_as_xlsx;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: transity <command> <journal.yaml>");
        eprintln!("Commands: xlsx");
        std::process::exit(1);
    }

    let command = &args[1];
    let path = &args[2];

    let yaml = std::fs::read_to_string(path)?;
    let ledger = Ledger::from_yaml(&yaml)?;

    match command.as_str() {
        "xlsx" => {
            let buf = entries_as_xlsx(&ledger)?;
            std::io::stdout().write_all(&buf)?;
        }
        other => {
            eprintln!("Unknown command: {}", other);
            std::process::exit(1);
        }
    }

    Ok(())
}
