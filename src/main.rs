use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, NaiveDateTime, Utc};
use clap::{Parser, Subcommand};
use colored::Colorize;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Zero;
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

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

// ─── DATA TYPES ──────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct Amount {
    pub quantity: BigRational,
    pub commodity: String,
}

impl Amount {
    pub fn new(quantity: BigRational, commodity: String) -> Self {
        Amount {
            quantity,
            commodity,
        }
    }

    pub fn negate(&self) -> Self {
        Amount {
            quantity: -self.quantity.clone(),
            commodity: self.commodity.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }

    pub fn add(&self, other: &Amount) -> Amount {
        if self.commodity != other.commodity {
            Amount {
                quantity: BigRational::zero(),
                commodity: "INVALID COMPUTATION".to_string(),
            }
        } else {
            Amount {
                quantity: &self.quantity + &other.quantity,
                commodity: self.commodity.clone(),
            }
        }
    }
}

fn parse_amount(s: &str) -> Result<Amount> {
    let parts: Vec<&str> = s.splitn(2, ' ').collect();
    if parts.len() != 2 {
        return Err(anyhow!(
            "Amount does not contain a value and a commodity: {}",
            s
        ));
    }
    let quantity = digits_to_rational(parts[0])
        .ok_or_else(|| anyhow!("Amount does not contain a valid value: {}", parts[0]))?;
    Ok(Amount {
        quantity,
        commodity: parts[1].to_string(),
    })
}

fn digits_to_rational(s: &str) -> Option<BigRational> {
    // Check all chars are valid number chars
    for c in s.chars() {
        if !matches!(c, '0'..='9' | '.' | '-' | '+') {
            return None;
        }
    }
    // Parse as rational by finding decimal point
    let dot_pos = s.find('.');
    match dot_pos {
        None => {
            let n: BigInt = s.parse().ok()?;
            Some(BigRational::new(n, BigInt::from(1u32)))
        }
        Some(pos) => {
            let frac_digits = s.len() - pos - 1;
            let numerator_str = s.replace('.', "");
            let numerator: BigInt = numerator_str.parse().ok()?;
            let denominator = BigInt::from(10u32).pow(frac_digits as u32);
            Some(BigRational::new(numerator, denominator))
        }
    }
}

fn rational_to_f64(r: &BigRational) -> f64 {
    // Convert BigRational to f64
    let num = r.numer();
    let den = r.denom();
    // Use string conversion via num-traits
    let num_f: f64 = num.to_string().parse().unwrap_or(0.0);
    let den_f: f64 = den.to_string().parse().unwrap_or(1.0);
    num_f / den_f
}

/// Type alias for commodity => amount map
type CommodityMap = BTreeMap<String, Amount>;

fn commodity_map_add(map: &mut CommodityMap, amount: Amount) {
    let entry = map.entry(amount.commodity.clone()).or_insert(Amount {
        quantity: BigRational::zero(),
        commodity: amount.commodity.clone(),
    });
    *entry = entry.add(&amount);
}

fn commodity_map_subtract(map: &mut CommodityMap, amount: &Amount) {
    let neg = amount.negate();
    let entry = map.entry(amount.commodity.clone()).or_insert(Amount {
        quantity: BigRational::zero(),
        commodity: amount.commodity.clone(),
    });
    *entry = entry.add(&neg);
}

#[derive(Debug, Clone)]
pub struct Balance {
    pub utc: DateTime<Utc>,
    pub commodity_map: CommodityMap,
}

#[derive(Debug, Clone, Deserialize)]
pub struct AccountRaw {
    pub id: String,
    pub balances: Option<Vec<BalanceRaw>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct BalanceRaw {
    pub utc: String,
    pub amounts: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Account {
    pub id: String,
    pub balances: Vec<Balance>,
}

impl Account {
    fn from_raw(raw: &AccountRaw) -> Result<Account> {
        let mut balances = Vec::new();
        if let Some(raw_balances) = &raw.balances {
            for rb in raw_balances {
                let utc = parse_datetime(&rb.utc)?;
                let mut commodity_map = CommodityMap::new();
                for amt_str in &rb.amounts {
                    let amount = parse_amount(amt_str)?;
                    commodity_map_add(&mut commodity_map, amount);
                }
                balances.push(Balance { utc, commodity_map });
            }
        }
        Ok(Account {
            id: raw.id.clone(),
            balances,
        })
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct EntityRaw {
    pub id: String,
    pub name: Option<String>,
    pub note: Option<String>,
    pub utc: Option<String>,
    pub tags: Option<Vec<String>>,
    pub accounts: Option<Vec<AccountRaw>>,
}

#[derive(Debug, Clone)]
pub struct Entity {
    pub id: String,
    pub name: Option<String>,
    pub note: Option<String>,
    pub utc: Option<DateTime<Utc>>,
    pub tags: Option<Vec<String>>,
    pub accounts: Vec<Account>,
}

impl Entity {
    fn from_raw(raw: &EntityRaw) -> Result<Entity> {
        let utc = raw.utc.as_deref().map(parse_datetime).transpose()?;
        let accounts = raw
            .accounts
            .as_deref()
            .unwrap_or(&[])
            .iter()
            .map(Account::from_raw)
            .collect::<Result<Vec<_>>>()?;
        Ok(Entity {
            id: raw.id.clone(),
            name: raw.name.clone(),
            note: raw.note.clone(),
            utc,
            tags: raw.tags.clone(),
            accounts,
        })
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct TransferRaw {
    pub utc: Option<String>,
    pub from: String,
    pub to: String,
    pub amount: String,
    pub note: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Transfer {
    pub utc: Option<DateTime<Utc>>,
    pub from: String,
    pub to: String,
    pub amount: Amount,
    pub note: Option<String>,
}

impl Transfer {
    fn from_raw(raw: &TransferRaw) -> Result<Transfer> {
        let utc = raw.utc.as_deref().map(parse_datetime).transpose()?;
        let amount = parse_amount(&raw.amount)?;
        if raw.from.is_empty() {
            return Err(anyhow!("Field 'from' must not be empty"));
        }
        if raw.to.is_empty() {
            return Err(anyhow!("Field 'to' must not be empty"));
        }
        if amount.is_zero() {
            return Err(anyhow!("Field 'amount' must not be 0"));
        }
        Ok(Transfer {
            utc,
            from: raw.from.clone(),
            to: raw.to.clone(),
            amount,
            note: raw.note.clone(),
        })
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct TransactionRaw {
    pub id: Option<String>,
    pub utc: Option<String>,
    pub note: Option<String>,
    #[serde(default)]
    pub files: Vec<String>,
    pub transfers: Vec<TransferRaw>,
}

#[derive(Debug, Clone)]
pub struct Transaction {
    pub id: Option<String>,
    pub utc: Option<DateTime<Utc>>,
    pub note: Option<String>,
    pub files: Vec<String>,
    pub transfers: Vec<Transfer>,
}

impl Transaction {
    fn from_raw(raw: &TransactionRaw) -> Result<Transaction> {
        let utc = raw.utc.as_deref().map(parse_datetime).transpose()?;
        let transfers = raw
            .transfers
            .iter()
            .map(Transfer::from_raw)
            .collect::<Result<Vec<_>>>()?;
        Ok(Transaction {
            id: raw.id.clone(),
            utc,
            note: raw.note.clone(),
            files: raw.files.clone(),
            transfers,
        })
    }

    /// Returns transfers with transaction UTC as fallback
    fn transfers_with_date(&self) -> Vec<Transfer> {
        self.transfers
            .iter()
            .map(|t| {
                if t.utc.is_some() {
                    t.clone()
                } else {
                    Transfer {
                        utc: self.utc,
                        ..t.clone()
                    }
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct LedgerRaw {
    pub owner: Option<String>,
    pub entities: Option<Vec<EntityRaw>>,
    pub transactions: Vec<TransactionRaw>,
}

#[derive(Debug, Clone)]
pub struct Ledger {
    pub owner: Option<String>,
    pub entities: Vec<Entity>,
    pub transactions: Vec<Transaction>,
}

impl Ledger {
    fn from_raw(raw: LedgerRaw) -> Result<Ledger> {
        let entities = raw
            .entities
            .unwrap_or_default()
            .iter()
            .map(Entity::from_raw)
            .collect::<Result<Vec<_>>>()?;
        let transactions = raw
            .transactions
            .iter()
            .map(Transaction::from_raw)
            .collect::<Result<Vec<_>>>()?;
        Ok(Ledger {
            owner: raw.owner,
            entities,
            transactions,
        })
    }

    fn merge(mut self, other: Ledger) -> Ledger {
        self.entities.extend(other.entities);
        self.transactions.extend(other.transactions);
        self
    }
}

// ─── DATE PARSING ────────────────────────────────────────────────────────────

fn parse_datetime(s: &str) -> Result<DateTime<Utc>> {
    let s = s.trim();
    // Try various formats
    let formats = [
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%dT%H:%M",
        "%Y-%m-%d",
    ];
    for fmt in &formats {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Ok(DateTime::from_naive_utc_and_offset(dt, Utc));
        }
    }
    // Try date-only
    if let Ok(d) = chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        let dt = d.and_hms_opt(0, 0, 0).unwrap();
        return Ok(DateTime::from_naive_utc_and_offset(dt, Utc));
    }
    Err(anyhow!("Cannot parse datetime: {}", s))
}

// ─── DISPLAY ─────────────────────────────────────────────────────────────────

struct WidthRecord {
    account: usize,
    integer: usize,
    fraction: usize,
    commodity: usize,
}

impl WidthRecord {
    fn zero() -> Self {
        WidthRecord {
            account: 0,
            integer: 0,
            fraction: 0,
            commodity: 0,
        }
    }

    fn merge(&self, other: &WidthRecord) -> WidthRecord {
        WidthRecord {
            account: self.account.max(other.account),
            integer: self.integer.max(other.integer),
            fraction: self.fraction.max(other.fraction),
            commodity: self.commodity.max(other.commodity),
        }
    }
}

fn length_of_num_parts(f: f64) -> (usize, usize) {
    let s = format!("{}", f);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_len = parts[0].len();
    let frac_len = if parts.len() > 1 && parts[1] != "0" {
        parts[1].len() + 1 // +1 for decimal point
    } else {
        0
    };
    (int_len, frac_len)
}

fn amount_to_width_record(amount: &Amount) -> WidthRecord {
    let f = rational_to_f64(&amount.quantity);
    let (int_len, frac_len) = length_of_num_parts(f);
    WidthRecord {
        account: 0,
        integer: int_len,
        fraction: frac_len,
        commodity: str_char_len(&amount.commodity),
    }
}

fn str_char_len(s: &str) -> usize {
    s.chars().count()
}

fn pad_start(width: usize, s: &str) -> String {
    let len = str_char_len(s);
    if len >= width {
        s.to_string()
    } else {
        format!("{}{}", " ".repeat(width - len), s)
    }
}

fn pad_end(width: usize, s: &str) -> String {
    let len = str_char_len(s);
    if len >= width {
        s.to_string()
    } else {
        format!("{}{}", s, " ".repeat(width - len))
    }
}

fn align_number(color: bool, int_width: usize, frac_width: usize, f: f64) -> String {
    let s = format!("{}", f);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_part = pad_start(int_width, parts[0]);
    let frac_part = if parts.len() > 1 && parts[1] != "0" {
        pad_end(frac_width, &format!(".{}", parts[1]))
    } else {
        " ".repeat(frac_width)
    };

    if color {
        if f >= 0.0 {
            format!("{}{}", int_part.green(), frac_part.bright_black())
        } else {
            format!("{}{}", int_part.red(), frac_part.bright_black())
        }
    } else {
        format!("{}{}", int_part, frac_part)
    }
}

fn show_amount_aligned(
    color: bool,
    int_w: usize,
    frac_w: usize,
    com_w: usize,
    amount: &Amount,
) -> String {
    let f = rational_to_f64(&amount.quantity);
    format!(
        "{} {}",
        align_number(color, int_w, frac_w, f),
        pad_end(com_w, &amount.commodity)
    )
}

fn show_commodity_map_aligned(
    color: bool,
    int_w: usize,
    frac_w: usize,
    com_w: usize,
    map: &CommodityMap,
) -> String {
    map.values()
        .map(|a| show_amount_aligned(color, int_w, frac_w, com_w, a))
        .collect::<Vec<_>>()
        .join("\n")
}

fn commodity_map_to_width_record(map: &CommodityMap) -> WidthRecord {
    map.values().fold(WidthRecord::zero(), |acc, a| {
        acc.merge(&amount_to_width_record(a))
    })
}

fn account_to_width_record(account_id: &str, map: &CommodityMap) -> WidthRecord {
    let mut wr = commodity_map_to_width_record(map);
    wr.account = wr.account.max(str_char_len(account_id));
    wr
}

fn indent_subsequent(indentation: usize, s: &str) -> String {
    let pad = " ".repeat(indentation);
    s.replace('\n', &format!("\n{}", pad))
}

fn trim_lines(s: &str) -> String {
    let trimmed: Vec<&str> = s.lines().map(|l| l.trim_end()).collect();
    let result = trimmed.join("\n");
    // Preserve trailing newline if original had one
    if s.ends_with('\n') {
        format!("{}\n", result.trim_end_matches('\n'))
    } else {
        result
    }
}

fn show_account_aligned(
    color: bool,
    width_rec: &WidthRecord,
    account_id: &str,
    map: &CommodityMap,
) -> String {
    let gap = 2;
    let account_width = width_rec.account.max(account_id.len());
    // Right-align (pad on left) to match PureScript Text.Format behavior
    let acc_name = pad_start(account_width, account_id);

    let colored_acc = if color {
        acc_name.blue().to_string()
    } else {
        acc_name
    };

    let com_str = show_commodity_map_aligned(
        color,
        width_rec.integer,
        width_rec.fraction,
        width_rec.commodity,
        map,
    );

    format!(
        "{}{}{}\n",
        colored_acc,
        " ".repeat(gap),
        indent_subsequent(account_width + gap, &com_str)
    )
}

fn date_show_pretty(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%d %H:%M").to_string()
}

fn utc_to_iso_string(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%dT%H:%M:%S").to_string()
}

fn utc_to_iso_date_string(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%d").to_string()
}

// ─── BALANCE MAP ─────────────────────────────────────────────────────────────

type BalanceMap = HashMap<String, CommodityMap>;

fn add_account_default(account_id: &str) -> String {
    let parts: Vec<&str> = account_id.split(':').collect();
    if parts.len() == 1 {
        format!("{}:_default_", account_id)
    } else {
        account_id.to_string()
    }
}

fn balance_map_add_transfer(map: &mut BalanceMap, transfer: &Transfer) {
    let from = add_account_default(&transfer.from);
    let to = add_account_default(&transfer.to);

    // Subtract from sender
    let from_map = map.entry(from).or_insert_with(CommodityMap::new);
    commodity_map_subtract(from_map, &transfer.amount);

    // Add to receiver
    let to_map = map.entry(to).or_insert_with(CommodityMap::new);
    commodity_map_add(to_map, transfer.amount.clone());
}

// ─── ENTITIES → TRANSFERS ────────────────────────────────────────────────────

fn entity_to_transfers(entity: &Entity) -> Vec<Transfer> {
    let mut result = Vec::new();
    for account in &entity.accounts {
        let full_id = format!("{}:{}", entity.id, account.id);
        for balance in &account.balances {
            for amount in balance.commodity_map.values() {
                result.push(Transfer {
                    utc: Some(balance.utc),
                    from: full_id.clone(),
                    to: "_void_".to_string(),
                    amount: amount.clone(),
                    note: None,
                });
            }
        }
    }
    result
}

fn entities_to_transfers(entities: &[Entity]) -> Vec<Transfer> {
    entities.iter().flat_map(entity_to_transfers).collect()
}

fn entities_to_balancing_transfers(entities: &[Entity]) -> Vec<Transfer> {
    entities_to_transfers(entities)
        .into_iter()
        .map(|t| Transfer {
            note: Some("___BALANCE___".to_string()),
            ..t
        })
        .collect()
}

fn entities_to_initial_transfers(entities: &[Entity]) -> Vec<Transfer> {
    entities_to_transfers(entities)
        .into_iter()
        .filter(|t| t.amount.is_zero() && t.from != "_void_")
        .collect()
}

// ─── VERIFY ──────────────────────────────────────────────────────────────────

pub fn verify_accounts(ledger: &Ledger) -> Result<()> {
    use std::collections::HashSet;

    let defined: HashSet<String> = ledger
        .entities
        .iter()
        .flat_map(|e| {
            let mut ids = vec![e.id.clone()];
            for acc in &e.accounts {
                ids.push(format!("{}:{}", e.id, acc.id));
            }
            ids
        })
        .collect();

    if defined.is_empty() {
        return Ok(());
    }

    let used: HashSet<String> = ledger
        .transactions
        .iter()
        .flat_map(|t| t.transfers.iter())
        .flat_map(|t| [t.from.clone(), t.to.clone()])
        .collect();

    let undefined: Vec<String> = used.difference(&defined).cloned().collect();

    if undefined.is_empty() {
        Ok(())
    } else {
        let mut sorted = undefined.clone();
        sorted.sort();
        let list = sorted
            .iter()
            .map(|s| format!("\n  - id: {}", s))
            .collect::<Vec<_>>()
            .join("");
        Err(anyhow!(
            "Following accounts were not declared, but still used for transfers:\n\nentities:{}\n\nPlease add or rename the missing accounts to the entities section to fix this error",
            list
        ))
    }
}

pub fn verify_ledger_balances(ledger: &Ledger) -> Result<()> {
    if ledger.entities.is_empty() {
        return Ok(());
    }

    let mut balancing = entities_to_balancing_transfers(&ledger.entities);
    let mut tx_transfers: Vec<Transfer> = ledger
        .transactions
        .iter()
        .flat_map(|tx| tx.transfers_with_date())
        .collect();

    let mut combined = Vec::new();
    combined.append(&mut balancing);
    combined.append(&mut tx_transfers);

    // Sort by UTC
    combined.sort_by(|a, b| a.utc.cmp(&b.utc));

    let mut balance_map: BalanceMap = BalanceMap::new();

    for transfer in &combined {
        balance_map_add_transfer(&mut balance_map, transfer);

        if transfer.note.as_deref() == Some("___BALANCE___") {
            let acc_id = add_account_default(&transfer.from);
            if let Some(com_map) = balance_map.get(&acc_id) {
                let commodity = &transfer.amount.commodity;
                if let Some(bal_amount) = com_map.get(commodity) {
                    if !bal_amount.is_zero() {
                        let f = rational_to_f64(&bal_amount.quantity);
                        let date_str = transfer
                            .utc
                            .as_ref()
                            .map(date_show_pretty)
                            .unwrap_or_default();
                        return Err(anyhow!(
                            "ERROR:\nThe verification balance of account '{}' on '{}'\nis off by {} {} from the calculated balance.",
                            transfer.from,
                            date_str,
                            -f,
                            commodity
                        ));
                    }
                }
            }
        }
    }

    Ok(())
}

// ─── DISPLAY COMMANDS ────────────────────────────────────────────────────────

fn norm_acc_id(id: &str) -> String {
    id.replace(":_default_", "")
}

#[derive(Debug, Clone, Copy)]
pub enum BalanceFilter {
    OnlyOwner,
    All,
}

pub fn show_balance(filter: BalanceFilter, color: bool, ledger: &Ledger) -> String {
    let mut balance_map: BalanceMap = BalanceMap::new();
    for tx in &ledger.transactions {
        for t in &tx.transfers_with_date() {
            balance_map_add_transfer(&mut balance_map, t);
        }
    }

    // Apply filter and remove zero amounts
    let mut filtered: Vec<(String, CommodityMap)> = balance_map
        .into_iter()
        .filter_map(|(acc_id, com_map)| {
            let include = match filter {
                BalanceFilter::All => true,
                BalanceFilter::OnlyOwner => match &ledger.owner {
                    Some(owner) => acc_id == *owner || acc_id.starts_with(&format!("{}:", owner)),
                    None => true,
                },
            };
            if !include {
                return None;
            }
            let non_zero: CommodityMap =
                com_map.into_iter().filter(|(_, a)| !a.is_zero()).collect();
            if non_zero.is_empty() {
                None
            } else {
                Some((acc_id, non_zero))
            }
        })
        .collect();

    // Sort by account id
    filtered.sort_by(|a, b| a.0.cmp(&b.0));

    // Compute global width record
    let global_wr = filtered
        .iter()
        .fold(WidthRecord::zero(), |acc, (acc_id, map)| {
            acc.merge(&account_to_width_record(&norm_acc_id(acc_id), map))
        });
    let margin_left = 2;
    let global_wr = WidthRecord {
        account: global_wr.account + margin_left,
        ..global_wr
    };

    let mut result = String::new();
    for (acc_id, map) in &filtered {
        result.push_str(&show_account_aligned(
            color,
            &global_wr,
            &norm_acc_id(acc_id),
            map,
        ));
    }
    result.push('\n');
    result.push('\n');
    trim_lines(&result)
}

pub fn show_transfer_aligned(
    color: bool,
    from_w: usize,
    to_w: usize,
    int_w: usize,
    frac_w: usize,
    com_w: usize,
    transfer: &Transfer,
) -> String {
    let offset_date = 19usize;
    let date_part = match &transfer.utc {
        Some(dt) => format!("{} | ", date_show_pretty(dt)),
        None => " ".repeat(offset_date),
    };
    let amount_str = show_amount_aligned(color, int_w, frac_w, com_w, &transfer.amount);
    format!(
        "{}{} -> {} : {} | {}\n",
        date_part,
        pad_start(from_w, &transfer.from),
        pad_start(to_w, &transfer.to),
        amount_str,
        transfer.note.as_deref().unwrap_or("")
    )
}

pub fn show_transaction_pretty_aligned(color: bool, tx: &Transaction) -> String {
    let from_w = 15;
    let to_w = 15;
    let int_w = 5;
    let frac_w = 3;
    let com_w = 10;

    let transfers_pretty: String = tx
        .transfers
        .iter()
        .map(|t| show_transfer_aligned(color, from_w, to_w, int_w, frac_w, com_w, t))
        .collect();

    let offset_date = 16;
    let offset_indentation = 4;

    let date_str = tx
        .utc
        .as_ref()
        .map(date_show_pretty)
        .unwrap_or_else(|| " ".repeat(offset_date));

    let note_str = pad_start(30, tx.note.as_deref().unwrap_or("NO NOTE"));
    let id_part = tx
        .id
        .as_ref()
        .map(|id| format!(" | (id {})", id))
        .unwrap_or_default();

    format!(
        "{} | {}{}{}\n",
        date_str,
        note_str,
        id_part,
        indent_subsequent(offset_indentation, &format!("\n{}", transfers_pretty))
    )
}

pub fn show_pretty_aligned(color: bool, ledger: &Ledger) -> String {
    let transactions_pretty: String = ledger
        .transactions
        .iter()
        .map(|tx| show_transaction_pretty_aligned(color, tx))
        .collect();

    trim_lines(&format!(
        "Journal for \"{}\"\n{}\n{}\n",
        ledger.owner.as_deref().unwrap_or("UNKNOWN"),
        "=".repeat(80),
        transactions_pretty
    ))
}

pub fn show_transfers(color: bool, ledger: &Ledger) -> String {
    let transfers_pretty: String = ledger
        .transactions
        .iter()
        .map(|tx| {
            tx.transfers_with_date()
                .iter()
                .map(|t| {
                    // Use show_transfer_aligned with defaults
                    show_transfer_aligned(color, 15, 15, 5, 3, 10, t)
                })
                .collect::<String>()
        })
        .collect();

    trim_lines(&format!(
        "Journal for \"{}\"\n{}\n{}\n",
        ledger.owner.as_deref().unwrap_or("UNKNOWN"),
        "=".repeat(80),
        transfers_pretty
    ))
}

fn show_number(f: f64) -> String {
    // Match PureScript's `show Number` which always includes a decimal point
    // e.g., 500.0, -300.0, 0.0, 33.95, 60.752114
    let s = format!("{}", f);
    if s.contains('.') {
        s
    } else {
        format!("{}.0", s)
    }
}

fn get_entries(ledger: &Ledger) -> Option<Vec<Vec<String>>> {
    let mut all_rows: Vec<Vec<String>> = Vec::new();

    for tx in &ledger.transactions {
        let transfers_with_date = tx.transfers_with_date();
        for t in &transfers_with_date {
            let utc = t.utc.as_ref()?;
            let iso = utc_to_iso_string(utc);
            let f = rational_to_f64(&t.amount.quantity);
            let neg_f = -f;
            // from row
            all_rows.push(vec![
                iso.clone(),
                t.from.clone(),
                show_number(neg_f),
                t.amount.commodity.clone(),
            ]);
            // to row
            all_rows.push(vec![
                iso.clone(),
                t.to.clone(),
                show_number(f),
                t.amount.commodity.clone(),
            ]);
        }
    }

    // Add initial transfers (zero-amount entity balances)
    let initial = entities_to_initial_transfers(&ledger.entities);
    for t in &initial {
        if let Some(utc) = &t.utc {
            let iso = utc_to_iso_string(utc);
            let acc_id = norm_acc_id(&t.from);
            let f = rational_to_f64(&t.amount.quantity);
            all_rows.push(vec![
                iso,
                acc_id,
                show_number(f),
                t.amount.commodity.clone(),
            ]);
        }
    }

    Some(all_rows)
}

pub fn show_entries(separator: &str, ledger: &Ledger) -> Option<String> {
    let mut rows = get_entries(ledger)?;
    rows.sort();
    let result = rows
        .iter()
        .map(|row| row.join(separator))
        .collect::<Vec<_>>()
        .join("\n");
    Some(format!("{}\n", result))
}

pub fn show_entries_by_account(ledger: &Ledger) -> Option<String> {
    let rows = get_entries(ledger)?;

    // Sort by account+commodity
    let mut sorted = rows.clone();
    sorted.sort_by(|a, b| {
        let key_a = format!(
            "{} {}",
            a.get(1).unwrap_or(&String::new()),
            a.get(3).unwrap_or(&String::new())
        );
        let key_b = format!(
            "{} {}",
            b.get(1).unwrap_or(&String::new()),
            b.get(3).unwrap_or(&String::new())
        );
        key_a.cmp(&key_b)
    });

    // Group by account+commodity
    let mut groups: Vec<(String, Vec<Vec<String>>)> = Vec::new();
    for row in sorted {
        let key = format!(
            "{} {}",
            row.get(1).unwrap_or(&String::new()),
            row.get(3).unwrap_or(&String::new())
        );
        if let Some(last) = groups.last_mut() {
            if last.0 == key {
                last.1.push(row);
                continue;
            }
        }
        groups.push((key.clone(), vec![row]));
    }

    let mut output_parts: Vec<String> = Vec::new();
    for (i, (key, group_rows)) in groups.iter().enumerate() {
        if i > 0 {
            // PureScript inserts [\n] between groups, which results in 2 blank lines
            output_parts.push("\n".to_string());
        }
        output_parts.push(format!("\"{}\"", key));
        let mut sorted_group = group_rows.to_vec();
        sorted_group.sort();
        for row in &sorted_group {
            output_parts.push(row.join(" "));
        }
    }

    Some(format!("{}\n", output_parts.join("\n")))
}

fn encode_commodity_map_json(map: &CommodityMap) -> String {
    // Serialize as array of [{"values":["commodity"],"tag":"Commodity"},"TODO"] pairs
    let pairs: Vec<String> = map
        .keys()
        .map(|commodity| {
            format!(
                "[{{\"values\":[\"{}\"],\"tag\":\"Commodity\"}},\"TODO\"]",
                commodity
            )
        })
        .collect();
    format!("[{}]", pairs.join(","))
}

fn encode_balance_json(balance: &Balance) -> String {
    let utc_str = utc_to_iso_string(&balance.utc);
    let com_map_json = encode_commodity_map_json(&balance.commodity_map);
    format!(
        "{{\"utc\":\"{}\",\"amounts\":[],\"commodityMap\":{}}}",
        utc_str, com_map_json
    )
}

fn encode_account_json(account: &Account) -> String {
    if account.balances.is_empty() {
        format!(
            "{{\"id\":\"{}\",\"commodityMap\":[],\"balances\":null}}",
            account.id
        )
    } else {
        let balances_json: Vec<String> = account.balances.iter().map(encode_balance_json).collect();
        format!(
            "{{\"id\":\"{}\",\"commodityMap\":[],\"balances\":[{}]}}",
            account.id,
            balances_json.join(",")
        )
    }
}

pub fn show_entities(alphabetically: bool, ledger: &Ledger) -> String {
    if ledger.entities.is_empty() {
        return "Journal does not contain any entities".to_string();
    }

    let mut entities: Vec<&Entity> = ledger.entities.iter().collect();
    if alphabetically {
        entities.sort_by(|a, b| a.id.to_lowercase().cmp(&b.id.to_lowercase()));
    }

    let mut result = "entities:\n".to_string();
    for entity in entities {
        result.push_str(&format!("  - id: {}\n", entity.id));
        if let Some(name) = &entity.name {
            result.push_str(&format!("    name: {}\n", name));
        }
        if let Some(note) = &entity.note {
            result.push_str(&format!("    note: {}\n", note));
        }
        if let Some(utc) = &entity.utc {
            result.push_str(&format!("    utc: {}\n", date_show_pretty(utc)));
        }
        if let Some(tags) = &entity.tags {
            // Match PureScript's show then strip quotes: ["foo","bar"] -> [foo,bar]
            let tags_str = tags
                .iter()
                .map(|t| t.as_str())
                .collect::<Vec<_>>()
                .join(",");
            result.push_str(&format!("    tags: [{}]\n", tags_str));
        }
        if !entity.accounts.is_empty() {
            result.push_str("    accounts:\n");
            for acc in &entity.accounts {
                result.push_str(&format!("      - {}\n", encode_account_json(acc)));
            }
        }
        result.push('\n');
    }
    result.push('\n');
    trim_lines(&result)
}

pub fn entries_to_ledger(ledger: &Ledger) -> String {
    let mut lines: Vec<String> = Vec::new();
    for tx in &ledger.transactions {
        if let Some(utc) = &tx.utc {
            for t in &tx.transfers {
                let date = utc_to_iso_date_string(utc);
                let note = tx.note.as_deref().unwrap_or("");
                let f = rational_to_f64(&t.amount.quantity);
                let amount_str = show_amount_plain(f, &t.amount.commodity);
                lines.push(format!(
                    "{} {}\n  {}  {}\n  {}\n",
                    date, note, t.to, amount_str, t.from
                ));
            }
        }
    }
    trim_lines(&format!("{}\n", lines.join("\n")))
}

fn show_amount_plain(f: f64, commodity: &str) -> String {
    let s = format!("{}", f);
    let parts: Vec<&str> = s.splitn(2, '.').collect();
    let int_part = parts[0];
    let frac_part = if parts.len() > 1 && parts[1] != "0" {
        format!(".{}", parts[1])
    } else {
        String::new()
    };
    format!("{}{} {}", int_part, frac_part, commodity)
}

pub fn gplot_code(data: &str, title: &str) -> String {
    let table = format!("$data << EOD\n{}\nEOD\n", data);
    let commands = vec![
        "set terminal pngcairo size 840, 420".to_string(),
        format!("set title '{}'", title),
        "set key outside nobox".to_string(),
        "set style line 12 lc rgb'#808080' lt 0 lw 1".to_string(),
        "set grid back ls 12".to_string(),
        "set grid xtics ytics mxtics".to_string(),
        "set style fill solid".to_string(),
        "set xdata time".to_string(),
        "set timefmt '%Y-%m-%dT%H:%M:%S'".to_string(),
        "set format x '%Y-W%W'".to_string(),
        "set xlabel 'ISO Week'".to_string(),
        "set xtics rotate by 30 right".to_string(),
        "set zeroaxis".to_string(),
        "set ylabel 'Commodity'".to_string(),
        "plot for [i=0:*] $data index i using 1:3 with impulses title columnhead(1)".to_string(),
        String::new(),
    ];
    format!("{}{}", table, commands.join(";"))
}

pub fn gplot_code_cumul(data: &str, title: &str) -> String {
    let table = format!("$data << EOD\n{}\nEOD\n", data);
    let commands = vec![
        "set terminal pngcairo size 840, 420".to_string(),
        format!("set title '{}'", title),
        "set key outside nobox".to_string(),
        "set style line 12 lc rgb'#808080' lt 0 lw 1".to_string(),
        "set grid back ls 12".to_string(),
        "set grid xtics ytics mxtics".to_string(),
        "set xdata time".to_string(),
        "set timefmt '%Y-%m-%dT%H:%M:%S'".to_string(),
        "set format x '%Y-W%W'".to_string(),
        "set xlabel 'ISO Week'".to_string(),
        "set xtics rotate by 30 right".to_string(),
        "set zeroaxis".to_string(),
        "set yrange [*<0:0<*]".to_string(),
        "set ylabel 'Commodity'".to_string(),
        "plot for [i=0:*] $data index i using 1:3 smooth cumulative with fillsteps title columnhead(1)".to_string(),
        String::new(),
    ];
    format!("{}{}", table, commands.join(";"))
}

// ─── XLSX ─────────────────────────────────────────────────────────────────────

pub fn entries_as_xlsx(ledger: &Ledger) -> Result<Vec<u8>> {
    use rust_xlsxwriter::Workbook;

    let rows = get_entries(ledger).ok_or_else(|| {
        anyhow!("All transfers or their parent transaction must have a valid UTC field")
    })?;
    let mut sorted = rows;
    sorted.sort();

    let mut workbook = Workbook::new();
    let worksheet = workbook.add_worksheet();

    // Header
    worksheet.write_string(0, 0, "UTC")?;
    worksheet.write_string(0, 1, "Account")?;
    worksheet.write_string(0, 2, "Quantity")?;
    worksheet.write_string(0, 3, "Commodity")?;

    for (i, row) in sorted.iter().enumerate() {
        let row_idx = (i + 1) as u32;
        for (j, val) in row.iter().enumerate() {
            worksheet.write_string(row_idx, j as u16, val.as_str())?;
        }
    }

    let buf = workbook.save_to_buffer()?;
    Ok(buf)
}

// ─── FILE LOADING ────────────────────────────────────────────────────────────

fn load_ledger(path: &Path) -> Result<Ledger> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Cannot read file: {}", path.display()))?;
    let raw: LedgerRaw = serde_yaml::from_str(&content)
        .with_context(|| format!("Cannot parse YAML in: {}", path.display()))?;
    Ledger::from_raw(raw)
}

fn load_and_verify(paths: &[PathBuf]) -> Result<Ledger> {
    if paths.is_empty() {
        return Err(anyhow!("No journal files specified"));
    }

    let mut ledger = load_ledger(&paths[0])?;
    for p in &paths[1..] {
        let other = load_ledger(p)?;
        ledger = ledger.merge(other);
    }

    verify_accounts(&ledger)?;
    verify_ledger_balances(&ledger)?;

    Ok(ledger)
}

fn collect_paths(journal: &str, extra: &[String]) -> Vec<PathBuf> {
    let mut paths = vec![PathBuf::from(journal)];
    paths.extend(extra.iter().map(PathBuf::from));
    paths
}

// ─── UNUSED FILES ─────────────────────────────────────────────────────────────

fn get_all_files(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut result = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = path.file_name().unwrap_or_default().to_string_lossy();
        if name == ".DS_Store" {
            continue;
        }
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

    // Collect referenced files
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

    // Get all files in directory
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

// ─── MAIN ─────────────────────────────────────────────────────────────────────

const UTC_ERROR: &str = "All transfers or their parent transaction must have a valid UTC field";

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
