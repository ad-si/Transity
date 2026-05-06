use leptos::prelude::*;

use crate::BalanceEntry;

#[component]
pub fn App() -> impl IntoView {
  view! {
    <nav>
      <h1>"Transity"</h1>
      <span class="subtitle">"Balance"</span>
    </nav>
    <main>
      <BalancePage />
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
