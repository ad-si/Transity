use leptos::prelude::*;
use leptos_router::components::{FlatRoutes, Redirect, Route, Router, A};
use leptos_router::path;

use crate::{BalanceEntry, TransactionEntry};

#[derive(Clone, Copy)]
struct FilePreview(RwSignal<Option<String>>);

#[component]
pub fn App() -> impl IntoView {
  let preview = FilePreview(RwSignal::new(None));
  provide_context(preview);

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
      <FilePreviewModal />
    </Router>
  }
}

fn file_kind(path: &str) -> &'static str {
  let ext = path
    .rsplit_once('.')
    .map(|(_, e)| e.to_ascii_lowercase())
    .unwrap_or_default();
  match ext.as_str() {
    "png" | "jpg" | "jpeg" | "gif" | "webp" | "svg" | "avif" | "heic" => {
      "image"
    }
    "pdf" => "pdf",
    "txt" | "log" | "md" | "yaml" | "yml" | "json" | "csv" | "html" | "htm" => {
      "text"
    }
    _ => "other",
  }
}

fn file_url(path: &str) -> String {
  // The path is sent as a query parameter rather than as part of the URL
  // path. Browsers normalize `..` segments inside a URL path before the
  // request leaves the client (per the WHATWG URL standard), which would
  // collapse paths like `../receipts/foo.pdf` and never reach our handler.
  // Query strings are not subject to that normalization.
  let encoded = path
    .bytes()
    .map(|b| match b {
      b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
        (b as char).to_string()
      }
      _ => format!("%{:02X}", b),
    })
    .collect::<String>();
  format!("/files?path={}", encoded)
}

#[component]
fn FilePreviewModal() -> impl IntoView {
  let FilePreview(preview) = expect_context::<FilePreview>();
  let close = move |_| preview.set(None);
  let stop = |ev: leptos::ev::MouseEvent| ev.stop_propagation();

  view! {
    {move || preview.get().map(|path| {
      let kind = file_kind(&path);
      let url = file_url(&path);
      let body = match kind {
        "image" => view! {
          <img class="file-preview-image" src=url.clone() alt=path.clone() />
        }.into_any(),
        "pdf" => view! {
          <iframe class="file-preview-frame" src=url.clone() title=path.clone()></iframe>
        }.into_any(),
        "text" | "html" => view! {
          <iframe class="file-preview-frame" src=url.clone() title=path.clone()></iframe>
        }.into_any(),
        _ => view! {
          <div class="file-preview-fallback">
            <p>"No preview available for this file type."</p>
            <a href=url.clone() target="_blank" rel="noopener">
              "Open " {path.clone()}
            </a>
          </div>
        }.into_any(),
      };
      view! {
        <div class="file-preview-overlay" on:click=close>
          <div class="file-preview-dialog" on:click=stop>
            <header class="file-preview-header">
              <span class="file-preview-path">{path.clone()}</span>
              <a
                class="file-preview-open"
                href=url.clone()
                target="_blank"
                rel="noopener"
                title="Open in new tab"
              >
                "↗"
              </a>
              <button
                class="file-preview-close"
                on:click=close
                aria-label="Close preview"
                type="button"
              >
                "×"
              </button>
            </header>
            <div class="file-preview-body">{body}</div>
          </div>
        </div>
      }
    })}
  }
}

#[server]
pub async fn get_balance() -> Result<Vec<BalanceEntry>, ServerFnError> {
  let loader = expect_context::<crate::server::LedgerLoader>();
  let ledger = loader
    .load()
    .map_err(|e| ServerFnError::new(format!("{:#}", e)))?;
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
  let loader = expect_context::<crate::server::LedgerLoader>();
  let ledger = loader
    .load()
    .map_err(|e| ServerFnError::new(format!("{:#}", e)))?;
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
  let highlight_no_files = RwSignal::new(false);
  let toggle = move |_| highlight_no_files.update(|v| *v = !*v);
  let table_class = move || {
    if highlight_no_files.get() {
      "transactions highlight-no-files"
    } else {
      "transactions"
    }
  };
  let button_class = move || {
    if highlight_no_files.get() {
      "tx-toolbar-button active"
    } else {
      "tx-toolbar-button"
    }
  };

  view! {
    <div class="tx-toolbar">
      <button
        class=button_class
        on:click=toggle
        type="button"
        aria-pressed=move || highlight_no_files.get().to_string()
      >
        "Highlight rows without files"
      </button>
    </div>
    <table class=table_class>
      <thead>
        <tr>
          <th class="col-tx-date">"Timestamp"</th>
          <th class="col-tx-transfers">"Transfers"</th>
          <th class="col-tx-note">"Note"</th>
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
  let files = entry.files;
  let files_class = if files.is_empty() { " no-files" } else { "" };
  let row_class = format!("transaction {stripe}{files_class}");
  let date = entry.utc.unwrap_or_default();
  let note = entry.note.unwrap_or_default();

  view! {
    <tr class=row_class>
      <td class="col-tx-date">{date}</td>
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
      <td class="col-tx-note">{note}</td>
      <td class="col-tx-files">
        <ul class="tx-files-list">
          {files
            .into_iter()
            .map(|f| {
              let FilePreview(preview) = expect_context::<FilePreview>();
              let path = f.clone();
              let url = file_url(&f);
              let on_click = move |ev: leptos::ev::MouseEvent| {
                ev.prevent_default();
                preview.set(Some(path.clone()));
              };
              view! {
                <li class="tx-file">
                  <a
                    class="tx-file-link"
                    href=url
                    on:click=on_click
                  >
                    {f}
                  </a>
                </li>
              }
            })
            .collect_view()}
        </ul>
      </td>
    </tr>
  }
}
