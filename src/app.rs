use leptos::ev;
use leptos::prelude::*;

use crate::{BalanceEntry, TransactionEntry, TransferEntry};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tab {
  Balance,
  Transactions,
}

fn tab_from_hash() -> Tab {
  match location_hash().as_deref() {
    Some("transactions") => Tab::Transactions,
    _ => Tab::Balance,
  }
}

fn tab_hash(tab: Tab) -> &'static str {
  match tab {
    Tab::Balance => "balance",
    Tab::Transactions => "transactions",
  }
}

fn write_hash(tab: Tab) {
  if !is_server() {
    let _ = window().location().set_hash(tab_hash(tab));
  }
}

#[component]
pub fn App() -> impl IntoView {
  let tab = RwSignal::new(tab_from_hash());

  // Sync browser back/forward and manual hash edits into the signal.
  if !is_server() {
    let handle = window_event_listener(ev::hashchange, move |_| {
      let new_tab = tab_from_hash();
      if tab.get_untracked() != new_tab {
        tab.set(new_tab);
      }
    });
    on_cleanup(move || handle.remove());
  }

  let is_balance = move || tab.get() == Tab::Balance;
  let is_transactions = move || tab.get() == Tab::Transactions;

  view! {
    <nav>
      <h1>"Transity"</h1>
      <div class="tabs">
        <button
          class="tab"
          class:active=is_balance
          on:click=move |_| {
            tab.set(Tab::Balance);
            write_hash(Tab::Balance);
          }
        >"Balance"</button>
        <button
          class="tab"
          class:active=is_transactions
          on:click=move |_| {
            tab.set(Tab::Transactions);
            write_hash(Tab::Transactions);
          }
        >"Transactions"</button>
      </div>
    </nav>
    <main>
      {move || match tab.get() {
        Tab::Balance => view! { <BalancePage /> }.into_any(),
        Tab::Transactions => view! { <TransactionsPage /> }.into_any(),
      }}
    </main>
  }
}

#[server]
pub async fn get_balance() -> Result<Vec<BalanceEntry>, ServerFnError> {
  let ledger = expect_context::<crate::Ledger>();
  let data = crate::get_balance_data(
    crate::BalanceFilter::OnlyOwner,
    &ledger,
    None,
    None,
  );
  Ok(crate::balance_data_to_entries(data))
}

#[server]
pub async fn get_transactions() -> Result<Vec<TransactionEntry>, ServerFnError>
{
  let ledger = expect_context::<crate::Ledger>();
  Ok(crate::ledger_to_transaction_entries(&ledger))
}

#[component]
fn BalancePage() -> impl IntoView {
  let balance = Resource::new(|| (), |_| get_balance());

  view! {
    <Suspense fallback=move || view! { <p class="loading">"Loading..."</p> }>
      {move || Suspend::new(async move {
        match balance.await {
          Ok(entries) => view! { <BalanceTable entries /> }.into_any(),
          Err(e) => view! {
            <p class="error">{format!("Error: {e}")}</p>
          }.into_any(),
        }
      })}
    </Suspense>
  }
}

#[component]
fn BalanceTable(entries: Vec<BalanceEntry>) -> impl IntoView {
  view! {
    <table>
      <thead>
        <tr>
          <th class="col-account">"Account"</th>
          <th class="col-amount" colspan="2">"Amount"</th>
          <th class="col-commodity">"Commodity"</th>
        </tr>
      </thead>
      <tbody>
        {entries
          .into_iter()
          .enumerate()
          .map(|(i, entry)| {
            let stripe = if i % 2 == 0 { "even" } else { "odd" };
            view! { <BalanceRow entry stripe /> }
          })
          .collect_view()}
      </tbody>
    </table>
  }
}

fn sign_class(value: &str) -> &'static str {
  if value.trim_start().starts_with('-') {
    "negative"
  } else if value.trim().is_empty() {
    ""
  } else {
    "positive"
  }
}

fn split_amount(value: &str) -> (String, String) {
  match value.find('.') {
    Some(i) => (value[..i].to_string(), value[i..].to_string()),
    None => (value.to_string(), String::new()),
  }
}

#[component]
fn BalanceRow(entry: BalanceEntry, stripe: &'static str) -> impl IntoView {
  let kind = if entry.is_group { "group" } else { "leaf" };
  let row_class = format!("{kind} {stripe}");
  let mut amounts = entry.amounts;
  if amounts.is_empty() {
    amounts.push(("".to_string(), "".to_string()));
  }
  let first = amounts.remove(0);
  let rowspan = 1 + amounts.len();
  let sign = sign_class(&first.1);
  let int_class = format!("col-amount-int {sign}");
  let frac_class = format!("col-amount-frac {sign}");
  let (int_part, frac_part) = split_amount(&first.1);

  view! {
    <tr class=row_class.clone()>
      <td class="col-account" rowspan=rowspan>
        {entry.account}
      </td>
      <td class=int_class>{int_part}</td>
      <td class=frac_class>{frac_part}</td>
      <td class="col-commodity">{first.0}</td>
    </tr>
    {amounts
      .into_iter()
      .map(|(commodity, value)| {
        let sign = sign_class(&value);
        let int_class = format!("col-amount-int {sign}");
        let frac_class = format!("col-amount-frac {sign}");
        let (int_part, frac_part) = split_amount(&value);
        let row_class = row_class.clone();
        view! {
          <tr class=row_class>
            <td class=int_class>{int_part}</td>
            <td class=frac_class>{frac_part}</td>
            <td class="col-commodity">{commodity}</td>
          </tr>
        }
      })
      .collect_view()}
  }
}

#[component]
fn TransactionsPage() -> impl IntoView {
  let txs = Resource::new(|| (), |_| get_transactions());

  view! {
    <Suspense fallback=move || view! { <p class="loading">"Loading..."</p> }>
      {move || Suspend::new(async move {
        match txs.await {
          Ok(entries) => view! { <TransactionsList entries /> }.into_any(),
          Err(e) => view! {
            <p class="error">{format!("Error: {e}")}</p>
          }.into_any(),
        }
      })}
    </Suspense>
  }
}

#[component]
fn TransactionsList(entries: Vec<TransactionEntry>) -> impl IntoView {
  view! {
    <ul class="transactions">
      {entries
        .into_iter()
        .enumerate()
        .map(|(i, entry)| {
          let stripe = if i % 2 == 0 { "even" } else { "odd" };
          view! { <TransactionItem entry stripe /> }
        })
        .collect_view()}
    </ul>
  }
}

#[component]
fn TransactionItem(
  entry: TransactionEntry,
  stripe: &'static str,
) -> impl IntoView {
  let item_class = format!("transaction {stripe}");
  let date = entry.utc.unwrap_or_default();
  let note = entry.note.unwrap_or_default();
  let id_view = entry
    .id
    .map(|id| view! { <span class="tx-id">"id "{id}</span> }.into_any())
    .unwrap_or_else(|| view! { <span></span> }.into_any());

  view! {
    <li class=item_class>
      <div class="tx-header">
        <span class="tx-date">{date}</span>
        <span class="tx-note">{note}</span>
        {id_view}
      </div>
      <ul class="tx-transfers">
        {entry.transfers
          .into_iter()
          .map(|transfer| view! { <TransferItem transfer /> })
          .collect_view()}
      </ul>
    </li>
  }
}

#[component]
fn TransferItem(transfer: TransferEntry) -> impl IntoView {
  let sign = sign_class(&transfer.amount_int);
  let amount_class = format!("tx-amount {sign}");
  let amount = format!(
    "{}{} {}",
    transfer.amount_int, transfer.amount_frac, transfer.commodity,
  );
  let note = transfer.note.unwrap_or_default();
  view! {
    <li class="transfer">
      <span class="tx-date">{transfer.utc.unwrap_or_default()}</span>
      <span class="tx-from">{transfer.from}</span>
      <span class="tx-arrow">"→"</span>
      <span class="tx-to">{transfer.to}</span>
      <span class=amount_class>{amount}</span>
      <span class="tx-note-inline">{note}</span>
    </li>
  }
}
