use leptos::prelude::*;
use leptos_router::{
  components::{Route, Router, Routes},
  StaticSegment,
};

use crate::BalanceEntry;

pub fn shell(options: LeptosOptions) -> impl IntoView {
  view! {
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="stylesheet" href="/pkg/transity.css" />
        <title>"Transity"</title>
        <AutoReload options=options.clone() />
        <HydrationScripts options />
      </head>
      <body>
        <App />
      </body>
    </html>
  }
}

#[component]
pub fn App() -> impl IntoView {
  view! {
    <Router>
      <nav>
        <h1>"Transity"</h1>
        <span class="subtitle">"Balance"</span>
      </nav>
      <main>
        <Routes fallback=|| "Page not found.".into_view()>
          <Route path=StaticSegment("") view=BalancePage />
        </Routes>
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
          <th class="col-amount">"Amount"</th>
          <th class="col-commodity">"Commodity"</th>
        </tr>
      </thead>
      <tbody>
        {entries
          .into_iter()
          .map(|entry| view! { <BalanceRow entry /> })
          .collect_view()}
      </tbody>
    </table>
  }
}

#[component]
fn BalanceRow(entry: BalanceEntry) -> impl IntoView {
  let class = if entry.is_group { "group" } else { "leaf" };
  let mut amounts = entry.amounts;
  if amounts.is_empty() {
    amounts.push(("".to_string(), "".to_string()));
  }
  let first = amounts.remove(0);
  let rowspan = 1 + amounts.len();

  view! {
    <tr class=class>
      <td class="col-account" rowspan=rowspan>
        {entry.account}
      </td>
      <td class="col-amount">{first.1}</td>
      <td class="col-commodity">{first.0}</td>
    </tr>
    {amounts
      .into_iter()
      .map(|(commodity, value)| {
        view! {
          <tr class=class>
            <td class="col-amount">{value}</td>
            <td class="col-commodity">{commodity}</td>
          </tr>
        }
      })
      .collect_view()}
  }
}
