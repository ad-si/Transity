use anyhow::Result;
use rust_xlsxwriter::{Formula, Workbook};

use crate::data::{entities_to_initial_transfers, utc_to_iso_string, Ledger};

pub struct SheetRow {
    pub utc: String,
    pub account: String,
    pub amount: f64,
    pub commodity: String,
    pub note: String,
    pub files: Vec<String>,
}

/// Collect all sheet rows from the ledger.
///
/// For each transaction → for each transfer that has a UTC (falling back to
/// the transaction UTC), emit two rows: one for `from` (negated amount) and
/// one for `to`. Transfers with no resolvable UTC are skipped.
/// Zero-amount entity account-initialisation rows are appended at the end.
pub fn get_sheet_rows(ledger: &Ledger) -> Vec<SheetRow> {
    let mut rows: Vec<SheetRow> = Vec::new();

    for transaction in &ledger.transactions {
        for transfer in &transaction.transfers {
            let Some(utc_raw) = transfer.utc.as_deref().or(transaction.utc.as_deref()) else {
                continue;
            };
            let utc = utc_to_iso_string(utc_raw);

            let note = [transaction.note.as_deref(), transfer.note.as_deref()]
                .iter()
                .filter_map(|s| *s)
                .collect::<Vec<_>>()
                .join(", ");

            let from_amount = transfer.amount.negate();

            rows.push(SheetRow {
                utc: utc.clone(),
                account: transfer.from.clone(),
                amount: from_amount.to_f64(),
                commodity: from_amount.commodity.clone(),
                note: note.clone(),
                files: transaction.files.clone(),
            });

            rows.push(SheetRow {
                utc,
                account: transfer.to.clone(),
                amount: transfer.amount.to_f64(),
                commodity: transfer.amount.commodity.clone(),
                note,
                files: transaction.files.clone(),
            });
        }
    }

    for transfer in entities_to_initial_transfers(&ledger.entities) {
        let utc_raw = transfer.utc.as_deref().unwrap_or("INVALID DATE");
        rows.push(SheetRow {
            utc: utc_to_iso_string(utc_raw),
            account: transfer.from.replace(":_default_", ""),
            amount: transfer.amount.to_f64(),
            commodity: transfer.amount.commodity.clone(),
            note: transfer.note.unwrap_or_default(),
            files: vec![],
        });
    }

    rows
}

/// Generate an Excel HYPERLINK formula that builds an absolute file path
/// relative to the workbook's location.  Works on macOS, Windows, Excel, and
/// LibreOffice (Apple Numbers does not support local file links at all).
pub fn hyperlink_formula(filename: &str) -> String {
    format!(
        r#"=HYPERLINK(SUBSTITUTE(LEFT(SUBSTITUTE(CELL("filename"),"\","/"),FIND("?",SUBSTITUTE(SUBSTITUTE(CELL("filename"),"\","/"),"/","?",LEN(SUBSTITUTE(CELL("filename"),"\","/")-LEN(SUBSTITUTE(SUBSTITUTE(CELL("filename"),"\","/"),"/","")))))&"{filename}","'file://",""),"{filename}")"#,
        filename = filename,
    )
}

/// Write the ledger's transfers to an XLSX buffer and return the raw bytes.
pub fn entries_as_xlsx(ledger: &Ledger) -> Result<Vec<u8>> {
    let rows = get_sheet_rows(ledger);

    let mut workbook = Workbook::new();
    let worksheet = workbook.add_worksheet();

    worksheet.write_string(0, 0, "Timestamp (UTC)")?;
    worksheet.write_string(0, 1, "Account")?;
    worksheet.write_string(0, 2, "Amount")?;
    worksheet.write_string(0, 3, "Commodity")?;
    worksheet.write_string(0, 4, "Note")?;
    worksheet.write_string(0, 5, "File 1")?;
    worksheet.write_string(0, 6, "File 2")?;
    worksheet.write_string(0, 7, "File 3")?;
    worksheet.write_string(0, 8, "File 4")?;

    let mut sorted_rows = rows;
    sorted_rows.sort_by(|a, b| a.utc.cmp(&b.utc));

    for (i, row) in sorted_rows.iter().enumerate() {
        let r = (i + 1) as u32;
        worksheet.write_string(r, 0, &format!("{}Z", row.utc))?;
        worksheet.write_string(r, 1, &row.account)?;
        worksheet.write_number(r, 2, row.amount)?;
        worksheet.write_string(r, 3, &row.commodity)?;
        worksheet.write_string(r, 4, &row.note)?;

        let mut files_padded: Vec<&str> =
            row.files.iter().map(|s| s.as_str()).collect();
        while files_padded.len() < 4 {
            files_padded.push("");
        }
        files_padded.truncate(4);

        for (j, file) in files_padded.iter().enumerate() {
            if file.is_empty() {
                worksheet.write_string(r, (5 + j) as u16, "")?;
            } else {
                worksheet.write_formula(r, (5 + j) as u16, Formula::new(&hyperlink_formula(file)))?;
            }
        }
    }

    workbook.save_to_buffer().map_err(|e| anyhow::anyhow!(e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hyperlink_formula_contains_filename() {
        let formula = hyperlink_formula("receipts/2020-01-06t1217_lunch.pdf");
        assert!(formula.contains("receipts/2020-01-06t1217_lunch.pdf"));
        assert!(formula.starts_with("=HYPERLINK("));
    }

    #[test]
    fn entries_as_xlsx_produces_valid_zip() {
        let yaml = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("examples/journal.yaml"),
        )
        .expect("examples/journal.yaml not found");

        let ledger = Ledger::from_yaml(&yaml).expect("failed to parse YAML");
        let buf = entries_as_xlsx(&ledger).expect("xlsx generation failed");
        assert!(buf.len() >= 2);
        assert_eq!(&buf[..2], b"PK", "XLSX must start with ZIP magic bytes");
    }
}
