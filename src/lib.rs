use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, NaiveDateTime, Utc};
use colored::Colorize;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Zero;
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};
#[cfg(feature = "cli")]
use std::path::Path;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

#[cfg(any(feature = "ssr", feature = "hydrate"))]
pub mod app;

#[cfg(feature = "ssr")]
pub mod server;

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

pub fn parse_amount(s: &str) -> Result<Amount> {
  let parts: Vec<&str> = s.splitn(2, ' ').collect();
  if parts.len() != 2 {
    return Err(anyhow!(
      "Amount does not contain a value and a commodity: {}",
      s
    ));
  }
  let quantity = digits_to_rational(parts[0]).ok_or_else(|| {
    anyhow!("Amount does not contain a valid value: {}", parts[0])
  })?;
  Ok(Amount {
    quantity,
    commodity: parts[1].to_string(),
  })
}

pub fn digits_to_rational(s: &str) -> Option<BigRational> {
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

pub fn rational_to_f64(r: &BigRational) -> f64 {
  // Convert BigRational to f64
  let num = r.numer();
  let den = r.denom();
  // Use string conversion via num-traits
  let num_f: f64 = num.to_string().parse().unwrap_or(0.0);
  let den_f: f64 = den.to_string().parse().unwrap_or(1.0);
  num_f / den_f
}

/// Type alias for commodity => amount map
pub type CommodityMap = BTreeMap<String, Amount>;

pub fn commodity_map_add(map: &mut CommodityMap, amount: Amount) {
  let entry = map.entry(amount.commodity.clone()).or_insert(Amount {
    quantity: BigRational::zero(),
    commodity: amount.commodity.clone(),
  });
  *entry = entry.add(&amount);
}

pub fn commodity_map_subtract(map: &mut CommodityMap, amount: &Amount) {
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
  pub fn from_raw(raw: &AccountRaw) -> Result<Account> {
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
  pub fn from_raw(raw: &EntityRaw) -> Result<Entity> {
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
  pub fn from_raw(raw: &TransferRaw) -> Result<Transfer> {
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
  pub fn from_raw(raw: &TransactionRaw) -> Result<Transaction> {
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
  pub fn transfers_with_date(&self) -> Vec<Transfer> {
    self
      .transfers
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
pub struct ConfigRaw {
  pub separator: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LedgerRaw {
  pub owner: Option<String>,
  pub config: Option<ConfigRaw>,
  pub entities: Option<Vec<EntityRaw>>,
  pub transactions: Vec<TransactionRaw>,
}

#[derive(Debug, Clone)]
pub struct Ledger {
  pub owner: Option<String>,
  pub separator: String,
  /// When true, only the configured separator character is recognized.
  /// `:` and `/` are NOT interchangeable — they are literal characters.
  /// When false (no config), both `:` and `/` are accepted and normalized.
  pub separator_is_explicit: bool,
  pub entities: Vec<Entity>,
  pub transactions: Vec<Transaction>,
  /// Mapping from normalized account ID to the original form as
  /// written in the source file. Only populated when normalization
  /// changes an ID (e.g. `:` → `/`). Used for error messages.
  pub original_account_ids: HashMap<String, String>,
}

impl Ledger {
  pub fn from_raw(raw: LedgerRaw) -> Result<Ledger> {
    let explicit_separator =
      raw.config.as_ref().and_then(|c| c.separator.clone());
    let separator = explicit_separator
      .clone()
      .unwrap_or_else(|| "/".to_string());
    if separator.chars().count() != 1 {
      return Err(anyhow!(
        "separator must be exactly one character, got: {:?}",
        separator
      ));
    }
    let entities = raw
      .entities
      .unwrap_or_default()
      .iter()
      .map(Entity::from_raw)
      .collect::<Result<Vec<_>>>()?;
    let transactions: Vec<Transaction> = raw
      .transactions
      .iter()
      .map(Transaction::from_raw)
      .collect::<Result<Vec<_>>>()?;
    // When no explicit separator is configured, both : and / are
    // accepted and normalized to the default.  When a separator IS
    // configured, only that character is a separator — all other
    // characters are literal parts of the account name.
    let mut original_account_ids: HashMap<String, String> = HashMap::new();
    let is_explicit = explicit_separator.is_some();
    let (entities, transactions, owner) = if !is_explicit {
      let entities = entities
        .into_iter()
        .map(|mut e| {
          e.id = normalize_account_id(&e.id, &separator);
          e.accounts = e
            .accounts
            .into_iter()
            .map(|mut a| {
              a.id = normalize_account_id(&a.id, &separator);
              a
            })
            .collect();
          e
        })
        .collect();
      let transactions = transactions
        .into_iter()
        .map(|mut tx| {
          tx.transfers = tx
            .transfers
            .into_iter()
            .map(|mut t| {
              let norm_from = normalize_account_id(&t.from, &separator);
              if norm_from != t.from {
                original_account_ids
                  .entry(norm_from.clone())
                  .or_insert(t.from.clone());
              }
              let norm_to = normalize_account_id(&t.to, &separator);
              if norm_to != t.to {
                original_account_ids
                  .entry(norm_to.clone())
                  .or_insert(t.to.clone());
              }
              t.from = norm_from;
              t.to = norm_to;
              t
            })
            .collect();
          tx
        })
        .collect();
      let owner = raw.owner.map(|o| normalize_account_id(&o, &separator));
      (entities, transactions, owner)
    } else {
      (entities, transactions, raw.owner)
    };
    Ok(Ledger {
      owner,
      separator,
      separator_is_explicit: is_explicit,
      entities,
      transactions,
      original_account_ids,
    })
  }

  pub fn merge(mut self, other: Ledger) -> Ledger {
    // Merge original ID mappings (keep first-seen)
    for (k, v) in other.original_account_ids {
      self.original_account_ids.entry(k).or_insert(v);
    }
    if self.separator != other.separator {
      let sep = self.separator.clone();
      // Normalize entity and account IDs from the other ledger
      let normalized_entities: Vec<Entity> = other
        .entities
        .into_iter()
        .map(|mut e| {
          e.id = normalize_account_id(&e.id, &sep);
          e.accounts = e
            .accounts
            .into_iter()
            .map(|mut a| {
              a.id = normalize_account_id(&a.id, &sep);
              a
            })
            .collect();
          e
        })
        .collect();
      self.entities.extend(normalized_entities);
      let normalized: Vec<Transaction> = other
        .transactions
        .into_iter()
        .map(|mut tx| {
          tx.transfers = tx
            .transfers
            .into_iter()
            .map(|mut t| {
              let norm_from = normalize_account_id(&t.from, &sep);
              if norm_from != t.from {
                self
                  .original_account_ids
                  .entry(norm_from.clone())
                  .or_insert(t.from.clone());
              }
              let norm_to = normalize_account_id(&t.to, &sep);
              if norm_to != t.to {
                self
                  .original_account_ids
                  .entry(norm_to.clone())
                  .or_insert(t.to.clone());
              }
              t.from = norm_from;
              t.to = norm_to;
              t
            })
            .collect();
          tx
        })
        .collect();
      self.transactions.extend(normalized);
    } else {
      self.entities.extend(other.entities);
      self.transactions.extend(other.transactions);
    }
    self
  }

  /// Return a new ledger with only transfers in [begin, end).
  /// Transactions whose transfers are all filtered out are removed entirely.
  pub fn filter_by_date(
    &self,
    begin: Option<DateTime<Utc>>,
    end: Option<DateTime<Utc>>,
  ) -> Ledger {
    if begin.is_none() && end.is_none() {
      return self.clone();
    }
    let transactions = self
      .transactions
      .iter()
      .filter_map(|tx| {
        let filtered_transfers: Vec<Transfer> = tx
          .transfers_with_date()
          .into_iter()
          .filter(|t| {
            if let Some(begin_dt) = &begin {
              match &t.utc {
                Some(t_utc) if t_utc < begin_dt => return false,
                _ => {}
              }
            }
            if let Some(end_dt) = &end {
              match &t.utc {
                Some(t_utc) if t_utc >= end_dt => return false,
                _ => {}
              }
            }
            true
          })
          .collect();
        if filtered_transfers.is_empty() {
          None
        } else {
          Some(Transaction {
            transfers: filtered_transfers,
            ..tx.clone()
          })
        }
      })
      .collect();
    Ledger {
      owner: self.owner.clone(),
      separator: self.separator.clone(),
      separator_is_explicit: self.separator_is_explicit,
      entities: self.entities.clone(),
      transactions,
      original_account_ids: self.original_account_ids.clone(),
    }
  }

  /// Return a new ledger keeping only transactions where at least one
  /// transfer involves an entity matching the tag expression.
  /// A transfer matches if the `from` or `to` entity's tags satisfy
  /// the expression.
  pub fn filter_by_tags(&self, expr: &TagExpr) -> Ledger {
    // Build map: entity_id -> set of tags
    let entity_tags: HashMap<&str, std::collections::HashSet<&str>> = self
      .entities
      .iter()
      .map(|e| {
        let tags: std::collections::HashSet<&str> = e
          .tags
          .as_ref()
          .map(|ts| ts.iter().map(|t| t.as_str()).collect())
          .unwrap_or_default();
        (e.id.as_str(), tags)
      })
      .collect();

    let sep = &self.separator;
    let empty: std::collections::HashSet<&str> =
      std::collections::HashSet::new();

    let transactions = self
      .transactions
      .iter()
      .filter_map(|tx| {
        let filtered_transfers: Vec<Transfer> = tx
          .transfers
          .iter()
          .filter(|t| {
            let from_entity = t
              .from
              .split_once(sep.as_str())
              .map_or(t.from.as_str(), |(e, _)| e);
            let to_entity = t
              .to
              .split_once(sep.as_str())
              .map_or(t.to.as_str(), |(e, _)| e);
            let from_tags = entity_tags.get(from_entity).unwrap_or(&empty);
            let to_tags = entity_tags.get(to_entity).unwrap_or(&empty);
            expr.eval(from_tags) || expr.eval(to_tags)
          })
          .cloned()
          .collect();
        if filtered_transfers.is_empty() {
          None
        } else {
          Some(Transaction {
            transfers: filtered_transfers,
            ..tx.clone()
          })
        }
      })
      .collect();
    Ledger {
      owner: self.owner.clone(),
      separator: self.separator.clone(),
      separator_is_explicit: self.separator_is_explicit,
      entities: self.entities.clone(),
      transactions,
      original_account_ids: self.original_account_ids.clone(),
    }
  }
}

// ─── TAG EXPRESSION ─────────────────────────────────────────────────────────

/// Boolean expression over entity tags.
#[derive(Debug, Clone, PartialEq)]
pub enum TagExpr {
  Tag(String),
  And(Box<TagExpr>, Box<TagExpr>),
  Or(Box<TagExpr>, Box<TagExpr>),
  Not(Box<TagExpr>),
}

impl TagExpr {
  /// Evaluate the expression against a set of tags.
  pub fn eval(&self, tags: &std::collections::HashSet<&str>) -> bool {
    match self {
      TagExpr::Tag(t) => tags.contains(t.as_str()),
      TagExpr::And(a, b) => a.eval(tags) && b.eval(tags),
      TagExpr::Or(a, b) => a.eval(tags) || b.eval(tags),
      TagExpr::Not(a) => !a.eval(tags),
    }
  }
}

/// Parse a tag filter expression.
///
/// Grammar (precedence low→high):
///   expr   = and_expr ('or' and_expr)*
///   and    = unary ('and' unary)*
///   unary  = 'not' unary | atom
///   atom   = TAG | '(' expr ')'
///
/// TAG is any run of non-whitespace chars that isn't a keyword or paren.
pub fn parse_tag_expr(input: &str) -> Result<TagExpr> {
  let tokens = tokenize_tag_expr(input)?;
  let mut pos = 0;
  let result = parse_or(&tokens, &mut pos)?;
  if pos != tokens.len() {
    return Err(anyhow!(
      "Unexpected token '{}' at position {}",
      tokens[pos],
      pos
    ));
  }
  Ok(result)
}

fn tokenize_tag_expr(input: &str) -> Result<Vec<String>> {
  let mut tokens = Vec::new();
  let mut chars = input.chars().peekable();
  while let Some(&c) = chars.peek() {
    if c.is_whitespace() {
      chars.next();
      continue;
    }
    if c == '(' || c == ')' {
      tokens.push(c.to_string());
      chars.next();
      continue;
    }
    // Collect a word
    let mut word = String::new();
    while let Some(&c) = chars.peek() {
      if c.is_whitespace() || c == '(' || c == ')' {
        break;
      }
      word.push(c);
      chars.next();
    }
    tokens.push(word);
  }
  if tokens.is_empty() {
    return Err(anyhow!("Empty tag expression"));
  }
  Ok(tokens)
}

fn parse_or(tokens: &[String], pos: &mut usize) -> Result<TagExpr> {
  let mut left = parse_and(tokens, pos)?;
  while *pos < tokens.len() && tokens[*pos] == "or" {
    *pos += 1;
    let right = parse_and(tokens, pos)?;
    left = TagExpr::Or(Box::new(left), Box::new(right));
  }
  Ok(left)
}

fn parse_and(tokens: &[String], pos: &mut usize) -> Result<TagExpr> {
  let mut left = parse_unary(tokens, pos)?;
  while *pos < tokens.len() && tokens[*pos] == "and" {
    *pos += 1;
    let right = parse_unary(tokens, pos)?;
    left = TagExpr::And(Box::new(left), Box::new(right));
  }
  Ok(left)
}

fn parse_unary(tokens: &[String], pos: &mut usize) -> Result<TagExpr> {
  if *pos < tokens.len() && tokens[*pos] == "not" {
    *pos += 1;
    let inner = parse_unary(tokens, pos)?;
    return Ok(TagExpr::Not(Box::new(inner)));
  }
  parse_atom(tokens, pos)
}

fn parse_atom(tokens: &[String], pos: &mut usize) -> Result<TagExpr> {
  if *pos >= tokens.len() {
    return Err(anyhow!("Unexpected end of tag expression"));
  }
  if tokens[*pos] == "(" {
    *pos += 1;
    let inner = parse_or(tokens, pos)?;
    if *pos >= tokens.len() || tokens[*pos] != ")" {
      return Err(anyhow!("Missing closing parenthesis in tag expression"));
    }
    *pos += 1;
    return Ok(inner);
  }
  let tok = &tokens[*pos];
  if tok == ")" || tok == "and" || tok == "or" {
    return Err(anyhow!("Unexpected '{}' in tag expression", tok));
  }
  *pos += 1;
  Ok(TagExpr::Tag(tok.clone()))
}

pub fn parse_ledger_str(yaml: &str) -> Result<Ledger> {
  let raw: LedgerRaw = serde_yaml::from_str(yaml)
    .with_context(|| "Cannot parse YAML".to_string())?;
  Ledger::from_raw(raw)
}

/// Normalize account separator characters in an account ID.
/// Both `:` and `/` are recognized as hierarchy separators and
/// replaced with the configured separator character.
pub fn normalize_account_id(id: &str, separator: &str) -> String {
  let mut result = id.to_string();
  if separator != ":" {
    result = result.replace(':', separator);
  }
  if separator != "/" {
    result = result.replace('/', separator);
  }
  result
}

// ─── DATE PARSING ────────────────────────────────────────────────────────────

pub fn parse_datetime(s: &str) -> Result<DateTime<Utc>> {
  let s = s.trim();
  // Try RFC 3339 first (handles timezone offsets like `Z` or `+02:00`
  // and fractional seconds, e.g. `2019-11-23T13:42:40.000Z`)
  if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
    return Ok(dt.with_timezone(&Utc));
  }
  let formats = [
    "%Y-%m-%dT%H:%M:%S%.f",
    "%Y-%m-%d %H:%M:%S%.f",
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
  if let Ok(d) = chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d") {
    let dt = d.and_hms_opt(0, 0, 0).unwrap();
    return Ok(DateTime::from_naive_utc_and_offset(dt, Utc));
  }
  Err(anyhow!("Cannot parse datetime: {}", s))
}

// ─── DISPLAY ─────────────────────────────────────────────────────────────────

pub struct WidthRecord {
  pub account: usize,
  pub integer: usize,
  pub fraction: usize,
  pub commodity: usize,
}

impl WidthRecord {
  pub fn zero() -> Self {
    WidthRecord {
      account: 0,
      integer: 0,
      fraction: 0,
      commodity: 0,
    }
  }

  pub fn merge(&self, other: &WidthRecord) -> WidthRecord {
    WidthRecord {
      account: self.account.max(other.account),
      integer: self.integer.max(other.integer),
      fraction: self.fraction.max(other.fraction),
      commodity: self.commodity.max(other.commodity),
    }
  }
}

pub fn length_of_num_parts(f: f64) -> (usize, usize) {
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

pub fn amount_to_width_record(amount: &Amount) -> WidthRecord {
  let f = rational_to_f64(&amount.quantity);
  let (int_len, frac_len) = length_of_num_parts(f);
  WidthRecord {
    account: 0,
    integer: int_len,
    fraction: frac_len,
    commodity: str_char_len(&amount.commodity),
  }
}

pub fn str_char_len(s: &str) -> usize {
  s.chars().count()
}

pub fn pad_start(width: usize, s: &str) -> String {
  let len = str_char_len(s);
  if len >= width {
    s.to_string()
  } else {
    format!("{}{}", " ".repeat(width - len), s)
  }
}

pub fn pad_end(width: usize, s: &str) -> String {
  let len = str_char_len(s);
  if len >= width {
    s.to_string()
  } else {
    format!("{}{}", s, " ".repeat(width - len))
  }
}

pub fn align_number(
  color: bool,
  int_width: usize,
  frac_width: usize,
  f: f64,
) -> String {
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

pub fn show_amount_aligned(
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

pub fn show_commodity_map_aligned(
  color: bool,
  int_w: usize,
  frac_w: usize,
  com_w: usize,
  map: &CommodityMap,
) -> String {
  map
    .values()
    .map(|a| show_amount_aligned(color, int_w, frac_w, com_w, a))
    .collect::<Vec<_>>()
    .join("\n")
}

pub fn commodity_map_to_width_record(map: &CommodityMap) -> WidthRecord {
  map.values().fold(WidthRecord::zero(), |acc, a| {
    acc.merge(&amount_to_width_record(a))
  })
}

pub fn account_to_width_record(
  account_id: &str,
  map: &CommodityMap,
) -> WidthRecord {
  let mut wr = commodity_map_to_width_record(map);
  wr.account = wr.account.max(str_char_len(account_id));
  wr
}

pub fn indent_subsequent(indentation: usize, s: &str) -> String {
  let pad = " ".repeat(indentation);
  s.replace('\n', &format!("\n{}", pad))
}

pub fn trim_lines(s: &str) -> String {
  let trimmed: Vec<&str> = s.lines().map(|l| l.trim_end()).collect();
  let result = trimmed.join("\n");
  // Preserve trailing newline if original had one
  if s.ends_with('\n') {
    format!("{}\n", result.trim_end_matches('\n'))
  } else {
    result
  }
}

pub fn show_account_aligned(
  color: bool,
  width_rec: &WidthRecord,
  account_id: &str,
  map: &CommodityMap,
  is_group: bool,
) -> String {
  let gap = 2;
  let account_width = width_rec.account.max(account_id.len());
  // Right-align (pad on left) to match PureScript Text.Format behavior
  let acc_name = pad_start(account_width, account_id);

  let colored_acc = if color {
    if is_group {
      acc_name.black().on_cyan().to_string()
    } else {
      acc_name.blue().to_string()
    }
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

pub fn date_show_pretty(dt: &DateTime<Utc>) -> String {
  dt.format("%Y-%m-%d %H:%M").to_string()
}

pub fn utc_to_iso_string(dt: &DateTime<Utc>) -> String {
  dt.format("%Y-%m-%dT%H:%M:%S").to_string()
}

pub fn utc_to_iso_date_string(dt: &DateTime<Utc>) -> String {
  dt.format("%Y-%m-%d").to_string()
}

// ─── BALANCE MAP ─────────────────────────────────────────────────────────────

pub type BalanceMap = HashMap<String, CommodityMap>;

pub fn add_account_default(account_id: &str, separator: &str) -> String {
  if !account_id.contains(separator) {
    format!("{}{}_default_", account_id, separator)
  } else {
    account_id.to_string()
  }
}

pub fn balance_map_add_transfer(
  map: &mut BalanceMap,
  transfer: &Transfer,
  separator: &str,
) {
  let from = add_account_default(&transfer.from, separator);
  let to = add_account_default(&transfer.to, separator);

  // Subtract from sender
  let from_map = map.entry(from).or_default();
  commodity_map_subtract(from_map, &transfer.amount);

  // Add to receiver
  let to_map = map.entry(to).or_default();
  commodity_map_add(to_map, transfer.amount.clone());
}

/// Seed the balance map with entity balance declarations effective at `at`.
/// For each entity account, finds the latest balance checkpoint at or before
/// `at` and adds those amounts to the balance map.
pub fn seed_balance_map_from_entities(
  map: &mut BalanceMap,
  ledger: &Ledger,
  at: &DateTime<Utc>,
) {
  let separator = &ledger.separator;
  for entity in &ledger.entities {
    for account in &entity.accounts {
      // Find the latest balance checkpoint at or before `at`
      let latest = account
        .balances
        .iter()
        .filter(|b| b.utc <= *at)
        .max_by_key(|b| b.utc);

      if let Some(balance) = latest {
        let full_id = add_account_default(
          &format!("{}{}{}", entity.id, separator, account.id),
          separator,
        );
        let acc_map = map.entry(full_id).or_default();
        for amount in balance.commodity_map.values() {
          commodity_map_add(acc_map, amount.clone());
        }
      }
    }
  }
}

// ─── ENTITIES → TRANSFERS ────────────────────────────────────────────────────

pub fn entity_to_transfers(entity: &Entity, separator: &str) -> Vec<Transfer> {
  let mut result = Vec::new();
  for account in &entity.accounts {
    let full_id = format!("{}{}{}", entity.id, separator, account.id);
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

pub fn entities_to_transfers(
  entities: &[Entity],
  separator: &str,
) -> Vec<Transfer> {
  entities
    .iter()
    .flat_map(|e| entity_to_transfers(e, separator))
    .collect()
}

pub fn entities_to_balancing_transfers(
  entities: &[Entity],
  separator: &str,
) -> Vec<Transfer> {
  entities_to_transfers(entities, separator)
    .into_iter()
    .map(|t| Transfer {
      note: Some("___BALANCE___".to_string()),
      ..t
    })
    .collect()
}

pub fn entities_to_initial_transfers(
  entities: &[Entity],
  separator: &str,
) -> Vec<Transfer> {
  entities_to_transfers(entities, separator)
    .into_iter()
    .filter(|t| t.amount.is_zero() && t.from != "_void_")
    .collect()
}

// ─── VERIFY ──────────────────────────────────────────────────────────────────

pub fn verify_accounts(ledger: &Ledger) -> Result<()> {
  use std::collections::HashSet;

  let separator = &ledger.separator;
  let defined: HashSet<String> = ledger
    .entities
    .iter()
    .flat_map(|e| {
      let mut ids = vec![e.id.clone()];
      for acc in &e.accounts {
        ids.push(format!("{}{}{}", e.id, separator, acc.id));
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
      .map(|s| {
        let display = ledger.original_account_ids.get(s.as_str()).unwrap_or(s);
        format!("\n  - id: {}", display)
      })
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

  let separator = &ledger.separator;
  let mut balancing =
    entities_to_balancing_transfers(&ledger.entities, separator);
  let mut tx_transfers: Vec<Transfer> = ledger
    .transactions
    .iter()
    .flat_map(|tx| tx.transfers_with_date())
    .collect();

  let mut combined = Vec::new();
  combined.append(&mut balancing);
  combined.append(&mut tx_transfers);

  // Sort by UTC
  combined.sort_by_key(|a| a.utc);

  let mut balance_map: BalanceMap = BalanceMap::new();

  for transfer in &combined {
    if transfer.note.as_deref() == Some("___BALANCE___") {
      // Temporarily apply the balancing transfer to check, but don't keep it
      let mut check_map = balance_map.clone();
      balance_map_add_transfer(&mut check_map, transfer, separator);

      let acc_id = add_account_default(&transfer.from, separator);
      if let Some(com_map) = check_map.get(&acc_id) {
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
      // Balance check passed — do NOT apply to balance_map,
      // matching PureScript behavior where the balancing transfer
      // is used only for verification, not to drain the account.
    } else {
      balance_map_add_transfer(&mut balance_map, transfer, separator);
    }
  }

  Ok(())
}

// ─── DISPLAY COMMANDS ────────────────────────────────────────────────────────

pub fn norm_acc_id(id: &str, separator: &str) -> String {
  id.replace(&format!("{}_default_", separator), "")
}

#[derive(Debug, Clone, Copy)]
pub enum BalanceFilter {
  OnlyOwner,
  All,
}

/// Normalize account IDs (strip `:_default_`) and create parent entries
/// that aggregate the balances of all their children.
///
/// For example, given leaf accounts `john:visa`, `john:bank:savings`, and
/// `john:bank:depot`, this produces additional entries for `john:bank`
/// (sum of savings + depot) and `john` (sum of all three).
pub fn expand_account_hierarchy(
  balance_map: BalanceMap,
  separator: &str,
) -> BalanceMap {
  // Step 1: Normalize all keys (remove <sep>_default_ suffix)
  let mut normalized: BalanceMap = BalanceMap::new();
  for (acc_id, com_map) in balance_map {
    let norm_id = norm_acc_id(&acc_id, separator);
    let entry = normalized.entry(norm_id).or_default();
    for amount in com_map.into_values() {
      commodity_map_add(entry, amount);
    }
  }

  // Step 2: For each account, add its amounts to all ancestor prefixes
  let leaves: Vec<(String, CommodityMap)> = normalized
    .iter()
    .map(|(k, v)| (k.clone(), v.clone()))
    .collect();

  for (acc_id, com_map) in &leaves {
    let parts: Vec<&str> = acc_id.split(separator).collect();
    for i in 1..parts.len() {
      let ancestor = parts[..i].join(separator);
      let entry = normalized.entry(ancestor).or_default();
      for amount in com_map.values() {
        commodity_map_add(entry, amount.clone());
      }
    }
  }

  normalized
}

/// A single row in the balance display, suitable for serialization.
#[derive(Debug, Clone)]
#[cfg_attr(
  any(feature = "ssr", feature = "hydrate"),
  derive(serde::Serialize, serde::Deserialize)
)]
pub struct BalanceEntry {
  pub account: String,
  /// Each entry is (commodity, formatted_value)
  pub amounts: Vec<(String, String)>,
  pub is_group: bool,
}

/// Returns structured balance data as an ordered list of
/// (normalized_account_id, CommodityMap, is_group) tuples.
pub fn get_balance_data(
  filter: BalanceFilter,
  ledger: &Ledger,
  begin: Option<DateTime<Utc>>,
  end: Option<DateTime<Utc>>,
) -> Vec<(String, CommodityMap, bool)> {
  let separator = &ledger.separator;
  let filtered_ledger = ledger.filter_by_date(begin, end);
  let mut balance_map: BalanceMap = BalanceMap::new();

  if let Some(begin_dt) = &begin {
    seed_balance_map_from_entities(
      &mut balance_map,
      &filtered_ledger,
      begin_dt,
    );
  }

  for tx in &filtered_ledger.transactions {
    for t in &tx.transfers_with_date() {
      balance_map_add_transfer(&mut balance_map, t, separator);
    }
  }

  let balance_map = expand_account_hierarchy(balance_map, separator);

  let mut filtered: Vec<(String, CommodityMap)> = balance_map
    .into_iter()
    .filter_map(|(acc_id, com_map)| {
      let include = match filter {
        BalanceFilter::All => true,
        BalanceFilter::OnlyOwner => match &ledger.owner {
          Some(owner) => {
            acc_id == *owner
              || acc_id.starts_with(&format!("{}{}", owner, separator))
          }
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

  let account_ids: Vec<String> =
    filtered.iter().map(|(id, _)| id.clone()).collect();
  let is_group = |acc_id: &str| -> bool {
    let prefix = format!("{}{}", acc_id, separator);
    account_ids.iter().any(|id| id.starts_with(&prefix))
  };
  let parent_group = |acc_id: &str| -> Option<String> {
    account_ids
      .iter()
      .filter(|g| {
        is_group(g) && acc_id.starts_with(&format!("{}{}", g, separator))
      })
      .max_by_key(|g| g.len())
      .cloned()
  };

  filtered.sort_by(|a, b| a.0.cmp(&b.0));

  let mut ungrouped: Vec<(String, CommodityMap)> = Vec::new();
  let mut groups: std::collections::BTreeMap<
    String,
    Vec<(String, CommodityMap)>,
  > = std::collections::BTreeMap::new();
  let mut group_entries: std::collections::BTreeMap<String, CommodityMap> =
    std::collections::BTreeMap::new();

  for (acc_id, com_map) in filtered {
    if is_group(&acc_id) {
      groups.entry(acc_id.clone()).or_default();
      group_entries.insert(acc_id, com_map);
    } else if let Some(g) = parent_group(&acc_id) {
      groups.entry(g).or_default().push((acc_id, com_map));
    } else {
      ungrouped.push((acc_id, com_map));
    }
  }

  let mut ordered: Vec<(String, CommodityMap, bool)> = Vec::new();
  for (acc_id, com_map) in ungrouped {
    ordered.push((acc_id, com_map, false));
  }
  for (group_id, children) in &groups {
    if let Some(com_map) = group_entries.remove(group_id) {
      ordered.push((group_id.clone(), com_map, true));
    }
    for (acc_id, com_map) in children {
      ordered.push((acc_id.clone(), com_map.clone(), false));
    }
  }

  // Normalize account IDs
  ordered
    .into_iter()
    .map(|(acc_id, com_map, is_group)| {
      (norm_acc_id(&acc_id, separator), com_map, is_group)
    })
    .collect()
}

/// Converts a CommodityMap to a list of (commodity, formatted_value) pairs.
pub fn commodity_map_to_display(map: &CommodityMap) -> Vec<(String, String)> {
  map
    .iter()
    .map(|(commodity, amount)| {
      let f = rational_to_f64(&amount.quantity);
      (commodity.clone(), show_number(f))
    })
    .collect()
}

/// Converts structured balance data to a list of BalanceEntry for serialization.
pub fn balance_data_to_entries(
  data: Vec<(String, CommodityMap, bool)>,
) -> Vec<BalanceEntry> {
  data
    .into_iter()
    .map(|(account, com_map, is_group)| BalanceEntry {
      account,
      amounts: commodity_map_to_display(&com_map),
      is_group,
    })
    .collect()
}

pub fn show_balance(
  filter: BalanceFilter,
  color: bool,
  ledger: &Ledger,
  begin: Option<DateTime<Utc>>,
  end: Option<DateTime<Utc>>,
) -> String {
  let ordered = get_balance_data(filter, ledger, begin, end);

  // Compute global width record
  let global_wr = ordered
    .iter()
    .fold(WidthRecord::zero(), |acc, (acc_id, map, _)| {
      acc.merge(&account_to_width_record(acc_id, map))
    });
  let margin_left = 2;
  let global_wr = WidthRecord {
    account: global_wr.account + margin_left,
    ..global_wr
  };

  let mut result = String::new();
  for (acc_id, map, group) in &ordered {
    if *group {
      result.push('\n');
    }
    result.push_str(&show_account_aligned(
      color, &global_wr, acc_id, map, *group,
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
  let amount_str =
    show_amount_aligned(color, int_w, frac_w, com_w, &transfer.amount);
  format!(
    "{}{} -> {} : {} | {}\n",
    date_part,
    pad_start(from_w, &transfer.from),
    pad_start(to_w, &transfer.to),
    amount_str,
    transfer.note.as_deref().unwrap_or("")
  )
}

pub fn show_transaction_pretty_aligned(
  color: bool,
  tx: &Transaction,
) -> String {
  let from_w = 15;
  let to_w = 15;
  let int_w = 5;
  let frac_w = 3;
  let com_w = 10;

  let transfers_pretty: String = tx
    .transfers
    .iter()
    .map(|t| {
      show_transfer_aligned(color, from_w, to_w, int_w, frac_w, com_w, t)
    })
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

pub fn show_number(f: f64) -> String {
  // Match PureScript's `show Number` which always includes a decimal point
  // e.g., 500.0, -300.0, 0.0, 33.95, 60.752114
  let s = format!("{}", f);
  if s.contains('.') {
    s
  } else {
    format!("{}.0", s)
  }
}

pub fn get_entries(ledger: &Ledger) -> Option<Vec<Vec<String>>> {
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
  let separator = &ledger.separator;
  let initial = entities_to_initial_transfers(&ledger.entities, separator);
  for t in &initial {
    if let Some(utc) = &t.utc {
      let iso = utc_to_iso_string(utc);
      let acc_id = norm_acc_id(&t.from, separator);
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

pub fn encode_commodity_map_json(map: &CommodityMap) -> String {
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

pub fn encode_balance_json(balance: &Balance) -> String {
  let utc_str = utc_to_iso_string(&balance.utc);
  let com_map_json = encode_commodity_map_json(&balance.commodity_map);
  format!(
    "{{\"utc\":\"{}\",\"amounts\":[],\"commodityMap\":{}}}",
    utc_str, com_map_json
  )
}

pub fn encode_account_json(account: &Account) -> String {
  if account.balances.is_empty() {
    format!(
      "{{\"id\":\"{}\",\"commodityMap\":[],\"balances\":null}}",
      account.id
    )
  } else {
    let balances_json: Vec<String> =
      account.balances.iter().map(encode_balance_json).collect();
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
    entities.sort_by_key(|a| a.id.to_lowercase());
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

pub fn show_amount_plain(f: f64, commodity: &str) -> String {
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

#[cfg(feature = "cli")]
pub fn entries_as_xlsx(ledger: &Ledger) -> Result<Vec<u8>> {
  use rust_xlsxwriter::Workbook;

  let rows = get_entries(ledger).ok_or_else(|| {
    anyhow!(
      "All transfers or their parent transaction must have a valid UTC field"
    )
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

#[cfg(feature = "cli")]
pub fn load_ledger(path: &Path) -> Result<Ledger> {
  let content = std::fs::read_to_string(path)
    .with_context(|| format!("Cannot read file: {}", path.display()))?;
  let raw: LedgerRaw = serde_yaml::from_str(&content)
    .with_context(|| format!("Cannot parse YAML in: {}", path.display()))?;
  Ledger::from_raw(raw)
}

/// Load a YAML file, but override its separator config so all files
/// in a multi-file journal share the same separator.
#[cfg(feature = "cli")]
fn load_ledger_with_separator(
  path: &Path,
  separator: &ConfigRaw,
) -> Result<Ledger> {
  let content = std::fs::read_to_string(path)
    .with_context(|| format!("Cannot read file: {}", path.display()))?;
  let mut raw: LedgerRaw = serde_yaml::from_str(&content)
    .with_context(|| format!("Cannot parse YAML in: {}", path.display()))?;
  // Apply the shared separator config so all files are parsed consistently
  raw.config = Some(separator.clone());
  Ledger::from_raw(raw)
}

#[cfg(feature = "cli")]
pub fn load_and_verify(paths: &[std::path::PathBuf]) -> Result<Ledger> {
  if paths.is_empty() {
    return Err(anyhow!("No journal files specified"));
  }

  // Load the first file to determine the separator config
  let mut ledger = load_ledger(&paths[0])?;
  let shared_config = ConfigRaw {
    separator: if ledger.separator_is_explicit {
      Some(ledger.separator.clone())
    } else {
      None
    },
  };

  // Load remaining files with the same separator config
  for p in &paths[1..] {
    let other = load_ledger_with_separator(p, &shared_config)?;
    ledger = ledger.merge(other);
  }

  verify_accounts(&ledger)?;
  verify_ledger_balances(&ledger)?;

  Ok(ledger)
}

// ─── HYDRATE ──────────────────────────────────────────────────────────────────

#[cfg(feature = "hydrate")]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn hydrate() {
  console_error_panic_hook::set_once();
  leptos::mount::mount_to_body(app::App);
}
