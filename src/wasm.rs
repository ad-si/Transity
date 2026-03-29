use wasm_bindgen::prelude::*;

use crate::{parse_ledger_str, show_balance, BalanceFilter};

#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

/// Parse a YAML journal string and return the balance as a formatted string.
#[wasm_bindgen]
pub fn get_balance(yaml: &str) -> String {
    match parse_ledger_str(yaml) {
        Ok(ledger) => show_balance(BalanceFilter::All, false, &ledger),
        Err(e) => format!("Error: {e}"),
    }
}
