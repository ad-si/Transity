//! Balance development of the owner's accounts over time.
//!
//! Produces one value per account (and commodity) for every day on which
//! a balance or an exchange rate changes. Values are converted into the
//! ledger's main currency with the declared `prices` and the exchange rates
//! implied by exchange transactions (two transfers of different commodities
//! between the same two parties in opposite directions).
//! Commodities declared with `price-interpolation: linear` change their
//! price linearly between two observations instead of in steps.
//! With a declared `price-indices` series for the main currency,
//! converted values can also be adjusted for inflation.

use chrono::{Datelike, NaiveDate};
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

use crate::{
  add_account_default, norm_acc_id, rational_to_f64, Ledger,
  PriceInterpolation, Transfer,
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
  /// Adjustment of `converted` for inflation.
  /// `None` without a price index of `currency`.
  pub inflation: Option<InflationAdjustment>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
  any(feature = "ssr", feature = "hydrate"),
  derive(serde::Serialize, serde::Deserialize)
)]
pub struct InflationAdjustment {
  /// Day (like `TrendData::days`) whose purchasing power
  /// the adjusted values are expressed in
  pub base_day: i32,
  /// One factor per entry in `TrendData::days` that converts a value
  /// into the purchasing power of `base_day`.
  /// `None` before the first value of the price index.
  pub factors: Vec<Option<f64>>,
}

/// Values of the price index of a commodity sorted by date.
/// On the same date, the last declared value takes precedence.
#[derive(Debug)]
pub struct PriceIndexSeries(Vec<(NaiveDate, f64)>);

impl PriceIndexSeries {
  pub fn from_ledger(
    ledger: &Ledger,
    commodity: &str,
  ) -> Option<PriceIndexSeries> {
    let mut values: Vec<(NaiveDate, f64)> = ledger
      .price_indices
      .iter()
      .filter(|p| p.commodity == commodity)
      .map(|p| (p.utc.date_naive(), p.value))
      .collect();
    // Stable sort keeps the declaration order within a date
    values.sort_by_key(|(date, _)| *date);
    values.reverse();
    values.dedup_by_key(|(date, _)| *date);
    values.reverse();
    (!values.is_empty()).then_some(PriceIndexSeries(values))
  }

  pub fn dates(&self) -> impl Iterator<Item = NaiveDate> + '_ {
    self.0.iter().map(|(date, _)| *date)
  }

  /// Value at `date`, interpolated linearly between two values.
  /// After the last value it stays constant, before the first it's unknown.
  pub fn value(&self, date: NaiveDate) -> Option<f64> {
    let idx = self.0.partition_point(|(d, _)| *d <= date);
    let (prev_date, prev) = *self.0.get(idx.checked_sub(1)?)?;
    let Some((next_date, next)) = self.0.get(idx).copied() else {
      return Some(prev);
    };
    let span = (next_date - prev_date).num_days() as f64;
    let elapsed = (date - prev_date).num_days() as f64;
    Some(prev + (next - prev) * elapsed / span)
  }

  /// Factors converting values at `days` into the purchasing power
  /// of the last day (or of the last index value, if that is earlier)
  pub fn adjustment(&self, days: &[NaiveDate]) -> Option<InflationAdjustment> {
    let last_index_date = self.0.last()?.0;
    let base_date = (*days.last()?).min(last_index_date);
    let base = self.value(base_date)?;
    Some(InflationAdjustment {
      base_day: base_date.num_days_from_ce(),
      factors: days
        .iter()
        .map(|d| self.value(*d).map(|v| base / v))
        .collect(),
    })
  }
}

/// Exchange rates declared via `prices` or observed in the ledger.
/// `rates[(a, b)]` lists observations of the price of 1 `a` in `b`
/// sorted by date. On the same date, declared prices come after
/// implied ones (and later declarations after earlier ones),
/// so the last observation on a date takes precedence.
#[derive(Debug, Default)]
pub struct ExchangeRates {
  rates: BTreeMap<(String, String), Vec<RateObservation>>,
  /// Commodities whose price is interpolated linearly
  linear: BTreeSet<String>,
}

#[derive(Debug, Clone, Copy)]
struct RateObservation {
  date: NaiveDate,
  rate: f64,
  declared: bool,
}

impl ExchangeRates {
  pub fn from_ledger(ledger: &Ledger) -> ExchangeRates {
    let mut rates = ExchangeRates {
      linear: ledger
        .commodities
        .iter()
        .filter(|c| c.price_interpolation == PriceInterpolation::Linear)
        .map(|c| c.id.clone())
        .collect(),
      ..ExchangeRates::default()
    };
    for tx in &ledger.transactions {
      let transfers = tx.transfers_with_date();
      for (i, a) in transfers.iter().enumerate() {
        for b in &transfers[i + 1..] {
          if let Some((date, rate)) = implied_rate(a, b, &ledger.separator) {
            let (ca, cb) = (&a.amount.commodity, &b.amount.commodity);
            rates.insert(ca, cb, date, rate, false);
          }
        }
      }
    }
    for p in &ledger.prices {
      let rate = rational_to_f64(&p.price.quantity);
      if rate > 0.0 && rate.is_finite() {
        let date = p.utc.date_naive();
        rates.insert(&p.commodity, &p.price.commodity, date, rate, true);
      }
    }
    for obs in rates.rates.values_mut() {
      // Stable sort keeps the declaration order within a date
      obs.sort_by_key(|o| (o.date, o.declared));
    }
    rates
  }

  /// Records the rate and its inverse
  fn insert(
    &mut self,
    from: &str,
    to: &str,
    date: NaiveDate,
    rate: f64,
    declared: bool,
  ) {
    for (key, rate) in [
      ((from.to_string(), to.to_string()), rate),
      ((to.to_string(), from.to_string()), 1.0 / rate),
    ] {
      self.rates.entry(key).or_default().push(RateObservation {
        date,
        rate,
        declared,
      });
    }
  }

  /// Dates on which at least one exchange rate was observed,
  /// plus the first day of every month between two observations
  /// of a linearly interpolated price, so that charts show its development
  pub fn dates(&self) -> impl Iterator<Item = NaiveDate> + '_ {
    let observed = self.rates.values().flatten().map(|o| o.date);
    let interpolated = self
      .rates
      .iter()
      .filter(|((from, _), _)| self.linear.contains(from))
      .flat_map(|(_, obs)| obs.windows(2))
      .flat_map(|w| month_starts_between(w[0].date, w[1].date));
    observed.chain(interpolated)
  }

  /// Rate at `date` derived from the observations of `from` in `to`.
  /// Before the first and after the last observation it stays constant.
  fn direct(&self, from: &str, to: &str, date: NaiveDate) -> Option<f64> {
    if !self.linear.contains(from) && self.linear.contains(to) {
      // Interpolate the price of the linear commodity and invert it
      return self.direct(to, from, date).map(|r| 1.0 / r);
    }
    let obs = self.rates.get(&(from.to_string(), to.to_string()))?;
    let idx = obs.partition_point(|o| o.date <= date);
    if idx == 0 {
      return obs.first().map(|o| o.rate);
    }
    let prev = obs[idx - 1];
    if idx == obs.len() || prev.date == date || !self.linear.contains(from) {
      return Some(prev.rate);
    }
    // The last observation on a date takes precedence
    let next_date = obs[idx].date;
    let next = obs[obs.partition_point(|o| o.date <= next_date) - 1];
    let span = (next.date - prev.date).num_days() as f64;
    let elapsed = (date - prev.date).num_days() as f64;
    Some(prev.rate + (next.rate - prev.rate) * elapsed / span)
  }

  /// Commodities with a direct rate from `from`, in alphabetical order
  fn neighbors<'a>(&'a self, from: &'a str) -> impl Iterator<Item = &'a str> {
    self
      .rates
      .range((from.to_string(), String::new())..)
      .map(|((a, b), _)| (a, b))
      .take_while(move |(a, _)| *a == from)
      .map(|(_, b)| b.as_str())
  }

  /// Price of 1 `from` in `to` at `date`.
  /// Without a direct rate, the shortest chain of conversions
  /// via other commodities is used (e.g. ACME → USD → EUR).
  pub fn rate(&self, from: &str, to: &str, date: NaiveDate) -> Option<f64> {
    if from == to {
      return Some(1.0);
    }
    if let Some(r) = self.direct(from, to, date) {
      return Some(r);
    }
    // Breadth-first search, tracking the accumulated rate to each node
    let mut visited: HashMap<&str, f64> = HashMap::from([(from, 1.0)]);
    let mut queue: VecDeque<&str> = VecDeque::from([from]);
    while let Some(node) = queue.pop_front() {
      let acc = visited[node];
      for next in self.neighbors(node) {
        if visited.contains_key(next) {
          continue;
        }
        let Some(r) = self.direct(node, next, date).map(|r| acc * r) else {
          continue;
        };
        if next == to {
          return Some(r);
        }
        visited.insert(next, r);
        queue.push_back(next);
      }
    }
    None
  }
}

/// First days of the months strictly between `start` and `end`
fn month_starts_between(
  start: NaiveDate,
  end: NaiveDate,
) -> impl Iterator<Item = NaiveDate> {
  let first = NaiveDate::from_ymd_opt(start.year(), start.month(), 1)
    .and_then(|d| d.checked_add_months(chrono::Months::new(1)));
  std::iter::successors(first, |d| d.checked_add_months(chrono::Months::new(1)))
    .take_while(move |d| *d < end)
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
  let price_index = currency
    .as_ref()
    .and_then(|cur| PriceIndexSeries::from_ledger(ledger, cur));

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
    // So that the adjusted values decline gradually with inflation
    if let Some(index) = &price_index {
      dates.extend(index.dates().filter(|d| *d > first));
    }
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

  // Rank accounts by their current absolute balance to decide
  // which ones get their own color and which are folded into "Other"
  let magnitude = |acc: &str| -> f64 {
    let current_abs = |vals: &Vec<Option<f64>>| {
      vals.iter().rev().flatten().next().map_or(0.0, |v| v.abs())
    };
    match converted.get(acc) {
      Some(vals) if !is_empty_series(vals) => current_abs(vals),
      _ => native
        .iter()
        .filter(|((a, _), _)| a == acc)
        .map(|(_, vals)| current_abs(vals))
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
    inflation: price_index.and_then(|index| index.adjustment(&days)),
  }
}
