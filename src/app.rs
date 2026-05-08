use leptos::prelude::*;
use leptos_router::components::{FlatRoutes, Redirect, Route, Router, A};
use leptos_router::path;

use crate::{BalanceEntry, TransactionEntry};

#[component]
pub fn App() -> impl IntoView {
  view! {
    <Router>
      <nav>
        <h1>"Transity"</h1>
        <div class="tabs">
          <A href="/balance" attr:class="tab">"Balance"</A>
          <A href="/transactions" attr:class="tab">"Transactions"</A>
        </div>
      </nav>
      <main>
        <FlatRoutes fallback=|| view! {
          <p class="error">"Page not found."</p>
        }>
          <Route
            path=path!("")
            view=|| view! { <Redirect path="/balance" /> }
          />
          <Route path=path!("/balance") view=BalancePage />
          <Route path=path!("/transactions") view=TransactionsPage />
        </FlatRoutes>
      </main>
    </Router>
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
  let mut entries = crate::ledger_to_transaction_entries(&ledger);
  entries.sort_by(|a, b| match (&a.utc, &b.utc) {
    (Some(a), Some(b)) => b.cmp(a),
    (Some(_), None) => std::cmp::Ordering::Less,
    (None, Some(_)) => std::cmp::Ordering::Greater,
    (None, None) => std::cmp::Ordering::Equal,
  });
  Ok(entries)
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
    <table class="transactions">
      <thead>
        <tr>
          <th class="col-tx-date">"Timestamp"</th>
          <th class="col-tx-note">"Note"</th>
          <th class="col-tx-transfers">"Transfers"</th>
          <th class="col-tx-files">"Files"</th>
        </tr>
      </thead>
      <tbody>
        {entries
          .into_iter()
          .enumerate()
          .map(|(i, entry)| {
            let stripe = if i % 2 == 0 { "even" } else { "odd" };
            view! { <TransactionRow entry stripe /> }
          })
          .collect_view()}
      </tbody>
    </table>
  }
}

const NOTE_ICON_SVG: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="13" height="13" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round" stroke-linecap="round" aria-hidden="true"><path d="M2.5 3h11v7H6.5l-3 3v-3H2.5V3z"/></svg>"#;

fn note_icon_view(note: Option<String>) -> leptos::prelude::AnyView {
  match note {
    Some(n) if !n.trim().is_empty() => {
      let label = n.clone();
      view! {
        <span
          class="tx-note-icon has-note"
          tabindex="0"
          attr:aria-label=label
        >
          <span class="tx-note-glyph" inner_html=NOTE_ICON_SVG />
          <span class="tx-note-tooltip" role="tooltip">{n}</span>
        </span>
      }
      .into_any()
    }
    _ => view! { <span class="tx-note-icon"></span> }.into_any(),
  }
}

#[component]
fn TransactionRow(
  entry: TransactionEntry,
  stripe: &'static str,
) -> impl IntoView {
  let row_class = format!("transaction {stripe}");
  let date = entry.utc.unwrap_or_default();
  let note = entry.note.unwrap_or_default();
  let files = entry.files;

  view! {
    <tr class=row_class>
      <td class="col-tx-date">{date}</td>
      <td class="col-tx-note">{note}</td>
      <td class="col-tx-transfers">
        <div class="tx-transfers-grid">
          {entry.transfers
            .into_iter()
            .map(|transfer| {
              let sign = sign_class(&transfer.amount_int);
              let int_class = format!("tx-amount-int {sign}");
              let frac_class = format!("tx-amount-frac {sign}");
              let note_view = note_icon_view(transfer.note);
              view! {
                <span class="tx-from">{transfer.from}</span>
                <span class="tx-arrow">"→"</span>
                <span class="tx-to">{transfer.to}</span>
                <span class=int_class>{transfer.amount_int}</span>
                <span class=frac_class>{transfer.amount_frac}</span>
                <span class="tx-commodity">{transfer.commodity}</span>
                {note_view}
              }
            })
            .collect_view()}
        </div>
      </td>
      <td class="col-tx-files">
        <ul class="tx-files-list">
          {files
            .into_iter()
            .map(|f| view! { <li class="tx-file">{f}</li> })
            .collect_view()}
        </ul>
      </td>
    </tr>
  }
}
