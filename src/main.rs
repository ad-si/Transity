use std::path::Path;
use transity::commands::unused_files::check_unused_files;

fn usage() {
    eprintln!("Usage: transity <command> [args...]");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  unused-files <files-dir> <journal.yaml> [extra-journals...]");
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        usage();
        std::process::exit(1);
    }

    match args[1].as_str() {
        "unused-files" => {
            if args.len() < 4 {
                eprintln!("Usage: transity unused-files <files-dir> <journal.yaml> [extra-journals...]");
                std::process::exit(1);
            }
            let files_dir = Path::new(&args[2]);
            let journal_paths: Vec<&Path> = args[3..].iter().map(|s| Path::new(s)).collect();
            if let Err(e) = check_unused_files(files_dir, &journal_paths) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        cmd => {
            eprintln!("Unknown command: {}", cmd);
            usage();
            std::process::exit(1);
        }
    }
}
