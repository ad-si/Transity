//! Balance development of the owner's accounts over time.
//!
//! Produces one value per account (and commodity) for every day on which
//! a balance or an exchange rate changes. Values are converted into the
//! ledger's main currency with exchange rates implied by exchange
//! transactions (two transfers of different commodities between the same
//! two parties in opposite directions).

use chrono::{Datelike, NaiveDate};
use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::{
  add_account_default, norm_acc_id, rational_to_f64, Ledger, Transfer,
};

/// Maximum number of accounts that get their own series.
/// All further accounts are folded into a single "Other" series.
pub const MAX_TREND_ACCOUNTS: usize = 7;

pub const OTHER_LABEL: &str = "Other";

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  any(feature = "ssr", feature = "hydrate"),
  derive(serde::Serialize, serde::Deserialize)
)]
pub struct TrendSeries {
  pub label: String,
  /// Index into the categorical color palette.
  /// `None` for the folded "Other" series.
  pub color_slot: Option<usize>,
  /// One value per entry in `TrendData::days`.
  /// `None` before the account held the commodity for the first time.
  pub values: Vec<Option<f64>>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  any(feature = "ssr", feature = "hydrate"),
  derive(serde::Serialize, serde::Deserialize)
)]
pub struct CommodityTrend {
  pub commodity: String,
  pub series: Vec<TrendSeries>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  any(feature = "ssr", feature = "hydrate"),
  derive(serde::Serialize, serde::Deserialize)
)]
pub struct TrendData {
  /// Days since 0001-01-01 (`NaiveDate::num_days_from_ce`), ascending
  pub days: Vec<i32>,
  /// Most frequently used commodity, used as target for conversions
  pub currency: Option<String>,
  /// Balances per commodity in their native unit
  pub native: Vec<CommodityTrend>,
  /// Balances per account converted into `currency`
  pub converted: Vec<TrendSeries>,
  /// Commodities which could not be converted into `currency`
  pub unconvertible: Vec<String>,
}

/// Exchange rates observed in the ledger.
/// `rates[(a, b)]` lists (date, price of 1 `a` in `b`) sorted by date.
#[derive(Debug, Default)]
pub struct ExchangeRates {
  rates: HashMap<(String, String), Vec<(NaiveDate, f64)>>,
}

impl ExchangeRates {
  pub fn from_ledger(ledger: &Ledger) -> ExchangeRates {
    let mut rates: HashMap<(String, String), Vec<(NaiveDate, f64)>> =
      HashMap::new();
    for tx in &ledger.transactions {
      let transfers = tx.transfers_with_date();
      for (i, a) in transfers.iter().enumerate() {
        for b in &transfers[i + 1..] {
          if let Some((date, rate)) = implied_rate(a, b, &ledger.separator) {
            let (ca, cb) = (&a.amount.commodity, &b.amount.commodity);
            rates
              .entry((ca.clone(), cb.clone()))
              .or_default()
              .push((date, rate));
            rates
              .entry((cb.clone(), ca.clone()))
              .or_default()
              .push((date, 1.0 / rate));
          }
        }
      }
    }
    for obs in rates.values_mut() {
      obs.sort_by_key(|(d, _)| *d);
    }
    ExchangeRates { rates }
  }

  /// Dates on which at least one exchange rate was observed
  pub fn dates(&self) -> impl Iterator<Item = NaiveDate> + '_ {
    self.rates.values().flatten().map(|(d, _)| *d)
  }

  /// Latest rate at or before `date`, or the earliest one after it
  fn direct(&self, from: &str, to: &str, date: NaiveDate) -> Option<f64> {
    let obs = self.rates.get(&(from.to_string(), to.to_string()))?;
    let idx = obs.partition_point(|(d, _)| *d <= date);
    if idx > 0 {
      Some(obs[idx - 1].1)
    } else {
      obs.first().map(|(_, r)| *r)
    }
  }

  /// Price of 1 `from` in `to` at `date`.
  /// Falls back to a conversion via one intermediate commodity.
  pub fn rate(&self, from: &str, to: &str, date: NaiveDate) -> Option<f64> {
    if from == to {
      return Some(1.0);
    }
    if let Some(r) = self.direct(from, to, date) {
      return Some(r);
    }
    self
      .rates
      .keys()
      .filter(|(a, b)| a == from && b != to)
      .find_map(|(_, via)| {
        Some(self.direct(from, via, date)? * self.direct(via, to, date)?)
      })
  }
}

/// Two transfers form an exchange if they move different commodities
/// between the same two entities in opposite directions.
/// The accounts may differ (e.g. paying from `john:giro`
/// and receiving the shares in `john:depot`).
fn implied_rate(
  a: &Transfer,
  b: &Transfer,
  separator: &str,
) -> Option<(NaiveDate, f64)> {
  let entity = |id: &str| id.split(separator).next().unwrap_or("").to_string();
  if a.amount.commodity == b.amount.commodity
    || entity(&a.from) != entity(&b.to)
    || entity(&a.to) != entity(&b.from)
  {
    return None;
  }
  let qa = rational_to_f64(&a.amount.quantity).abs();
  let qb = rational_to_f64(&b.amount.quantity).abs();
  if qa == 0.0 || qb == 0.0 || !qa.is_finite() || !qb.is_finite() {
    return None;
  }
  let date = match (a.utc, b.utc) {
    (Some(x), Some(y)) => x.max(y),
    (Some(x), None) | (None, Some(x)) => x,
    (None, None) => return None,
  };
  Some((date.date_naive(), qb / qa))
}

/// The commodity used by the most transfers (ties resolved alphabetically)
pub fn main_currency(ledger: &Ledger) -> Option<String> {
  let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
  for tx in &ledger.transactions {
    for t in &tx.transfers {
      *counts.entry(&t.amount.commodity).or_default() += 1;
    }
  }
  counts
    .into_iter()
    .max_by(|(ca, a), (cb, b)| a.cmp(b).then(cb.cmp(ca)))
    .map(|(c, _)| c.to_string())
}

fn is_owned(ledger: &Ledger, acc_id: &str) -> bool {
  match &ledger.owner {
    Some(owner) => {
      acc_id == owner
        || acc_id.starts_with(&format!("{}{}", owner, ledger.separator))
    }
    None => true,
  }
}

fn sum_options(values: impl Iterator<Item = Option<f64>>) -> Option<f64> {
  values.fold(None, |acc, v| match (acc, v) {
    (None, None) => None,
    (a, b) => Some(a.unwrap_or(0.0) + b.unwrap_or(0.0)),
  })
}

fn is_empty_series(values: &[Option<f64>]) -> bool {
  values.iter().all(|v| v.is_none_or(|x| x.abs() < 1e-9))
}

/// Builds the list of series in rank order.
/// Accounts without a color slot are summed up into an "Other" series.
fn build_series(
  order: &[&str],
  slots: &HashMap<&str, usize>,
  len: usize,
  get: &dyn Fn(&str) -> Option<Vec<Option<f64>>>,
) -> Vec<TrendSeries> {
  let mut result: Vec<TrendSeries> = Vec::new();
  let mut other: Vec<Vec<Option<f64>>> = Vec::new();
  for acc in order {
    let Some(values) = get(acc) else { continue };
    if is_empty_series(&values) {
      continue;
    }
    match slots.get(acc) {
      Some(slot) => result.push(TrendSeries {
        label: acc.to_string(),
        color_slot: Some(*slot),
        values,
      }),
      None => other.push(values),
    }
  }
  if !other.is_empty() {
    result.push(TrendSeries {
      label: OTHER_LABEL.to_string(),
      color_slot: None,
      values: (0..len)
        .map(|i| sum_options(other.iter().map(|v| v[i])))
        .collect(),
    });
  }
  result
}

pub fn get_trend_data(ledger: &Ledger) -> TrendData {
  let separator = &ledger.separator;
  let rates = ExchangeRates::from_ledger(ledger);
  let currency = main_currency(ledger);

  // Transfers touching the owner's accounts, grouped by day
  let mut by_day: BTreeMap<NaiveDate, Vec<(String, String, f64)>> =
    BTreeMap::new();
  for tx in &ledger.transactions {
    for t in tx.transfers_with_date() {
      let Some(utc) = t.utc else { continue };
      let qty = rational_to_f64(&t.amount.quantity);
      let sides = [(&t.from, -qty), (&t.to, qty)];
      for (acc, delta) in sides {
        let acc_id =
          norm_acc_id(&add_account_default(acc, separator), separator);
        if is_owned(ledger, &acc_id) {
          by_day.entry(utc.date_naive()).or_default().push((
            acc_id,
            t.amount.commodity.clone(),
            delta,
          ));
        }
      }
    }
  }

  let mut dates: BTreeSet<NaiveDate> = by_day.keys().copied().collect();
  if let Some(first) = dates.first().copied() {
    dates.extend(rates.dates().filter(|d| *d > first));
  }
  let days: Vec<NaiveDate> = dates.into_iter().collect();

  // Native balances per (account, commodity)
  let mut current: BTreeMap<(String, String), f64> = BTreeMap::new();
  let mut native: BTreeMap<(String, String), Vec<Option<f64>>> =
    BTreeMap::new();
  for (i, day) in days.iter().enumerate() {
    for (acc, com, delta) in by_day.get(day).into_iter().flatten() {
      *current.entry((acc.clone(), com.clone())).or_default() += delta;
    }
    for (key, value) in &current {
      native
        .entry(key.clone())
        .or_insert_with(|| vec![None; i])
        .push(
          // Avoid floating point noise like 1e-14 after closing an account
          Some(if value.abs() < 1e-9 { 0.0 } else { *value }),
        );
    }
  }

  // Converted balances per account
  let mut unconvertible: BTreeSet<String> = BTreeSet::new();
  let mut converted: BTreeMap<String, Vec<Option<f64>>> = BTreeMap::new();
  if let Some(cur) = &currency {
    for ((acc, com), values) in &native {
      let series = converted
        .entry(acc.clone())
        .or_insert_with(|| vec![None; days.len()]);
      for (i, value) in values.iter().enumerate() {
        let Some(v) = value else { continue };
        match rates.rate(com, cur, days[i]) {
          Some(rate) => {
            series[i] = Some(series[i].unwrap_or(0.0) + v * rate);
          }
          None => {
            if *v != 0.0 {
              unconvertible.insert(com.clone());
            }
          }
        }
      }
    }
  }

  // Rank accounts by their largest absolute balance to decide
  // which ones get their own color and which are folded into "Other"
  let magnitude = |acc: &str| -> f64 {
    let max_abs = |vals: &Vec<Option<f64>>| {
      vals.iter().flatten().fold(0.0_f64, |m, v| m.max(v.abs()))
    };
    match converted.get(acc) {
      Some(vals) if !is_empty_series(vals) => max_abs(vals),
      _ => native
        .iter()
        .filter(|((a, _), _)| a == acc)
        .map(|(_, vals)| max_abs(vals))
        .sum(),
    }
  };
  let mut accounts: Vec<(String, f64)> = native
    .iter()
    .filter(|(_, vals)| !is_empty_series(vals))
    .map(|((acc, _), _)| acc.clone())
    .collect::<BTreeSet<_>>()
    .into_iter()
    .map(|acc| {
      let m = magnitude(&acc);
      (acc, m)
    })
    .collect();
  accounts.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.cmp(&b.0)));
  let fold = accounts.len() > MAX_TREND_ACCOUNTS + 1;
  let slots: HashMap<&str, usize> = accounts
    .iter()
    .take(if fold {
      MAX_TREND_ACCOUNTS
    } else {
      accounts.len()
    })
    .enumerate()
    .map(|(i, (acc, _))| (acc.as_str(), i))
    .collect();
  let order: Vec<&str> = accounts.iter().map(|(a, _)| a.as_str()).collect();

  let build = |get: &dyn Fn(&str) -> Option<Vec<Option<f64>>>| {
    build_series(&order, &slots, days.len(), get)
  };

  let mut commodities: Vec<&String> = native
    .iter()
    .filter(|(_, vals)| !is_empty_series(vals))
    .map(|((_, com), _)| com)
    .collect::<BTreeSet<_>>()
    .into_iter()
    .collect();
  // Main currency first, then alphabetically
  commodities.sort_by_key(|c| Some(*c) != currency.as_ref());

  TrendData {
    days: days.iter().map(|d| d.num_days_from_ce()).collect(),
    native: commodities
      .into_iter()
      .map(|com| CommodityTrend {
        commodity: com.clone(),
        series: build(&|acc| {
          native.get(&(acc.to_string(), com.clone())).cloned()
        }),
      })
      .collect(),
    converted: build(&|acc| converted.get(acc).cloned()),
    currency,
    unconvertible: unconvertible.into_iter().collect(),
  }
}
