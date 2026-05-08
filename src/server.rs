use axum::extract::{Query, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Response};
use axum::Router;
use leptos::prelude::*;
use leptos_axum::handle_server_fns_with_context;
use serde::Deserialize;
use std::collections::HashSet;
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::Ledger;

#[derive(Clone)]
pub struct LedgerLoader {
  pub paths: Vec<PathBuf>,
  pub owner: Option<String>,
}

impl LedgerLoader {
  pub fn load(&self) -> anyhow::Result<Ledger> {
    let mut ledger = crate::load_and_verify(&self.paths)?;
    if let Some(o) = &self.owner {
      ledger.owner = Some(o.clone());
    }
    Ok(ledger)
  }
}

#[allow(dead_code)]
mod embedded_assets {
  include!(concat!(env!("OUT_DIR"), "/embedded_assets.rs"));
}

const SHELL_HTML: &str = r#"<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="/pkg/transity.css">
    <title>Transity</title>
    <link rel="modulepreload" href="/pkg/transity.js">
    <link rel="preload" href="/pkg/transity_bg.wasm" as="fetch" type="application/wasm">
    <script type="module">
      import init, { hydrate } from '/pkg/transity.js';
      init().then(() => hydrate());
    </script>
  </head>
  <body></body>
</html>"#;

fn dev_mode() -> bool {
  std::env::var("LEPTOS_OUTPUT_NAME").is_ok()
}

fn disk_asset_path(name: &str) -> PathBuf {
  let root = std::env::var("LEPTOS_SITE_ROOT")
    .unwrap_or_else(|_| "target/site".to_string());
  let pkg_dir =
    std::env::var("LEPTOS_SITE_PKG_DIR").unwrap_or_else(|_| "pkg".to_string());
  PathBuf::from(root).join(pkg_dir).join(name)
}

fn load_asset(name: &str, embedded: &'static [u8]) -> Vec<u8> {
  if dev_mode() {
    if let Ok(bytes) = std::fs::read(disk_asset_path(name)) {
      return bytes;
    }
  }
  embedded.to_vec()
}

async fn serve_shell() -> impl IntoResponse {
  Html(SHELL_HTML)
}

async fn serve_js() -> impl IntoResponse {
  (
    [("content-type", "application/javascript")],
    load_asset("transity.js", embedded_assets::JS),
  )
}

async fn serve_css() -> impl IntoResponse {
  (
    [("content-type", "text/css")],
    load_asset("transity.css", embedded_assets::CSS),
  )
}

async fn serve_wasm() -> impl IntoResponse {
  (
    [("content-type", "application/wasm")],
    load_asset("transity.wasm", embedded_assets::WASM),
  )
}

async fn serve_favicon() -> impl IntoResponse {
  axum::http::StatusCode::NO_CONTENT
}

fn mime_for(path: &std::path::Path) -> &'static str {
  match path
    .extension()
    .and_then(|e| e.to_str())
    .map(|s| s.to_ascii_lowercase())
    .as_deref()
  {
    Some("png") => "image/png",
    Some("jpg" | "jpeg") => "image/jpeg",
    Some("gif") => "image/gif",
    Some("webp") => "image/webp",
    Some("svg") => "image/svg+xml",
    Some("avif") => "image/avif",
    Some("heic") => "image/heic",
    Some("pdf") => "application/pdf",
    Some("txt" | "log" | "md" | "yaml" | "yml" | "json" | "csv") => {
      "text/plain; charset=utf-8"
    }
    Some("html" | "htm") => "text/html; charset=utf-8",
    _ => "application/octet-stream",
  }
}

#[derive(Deserialize)]
struct FileQuery {
  path: String,
}

#[derive(Clone)]
struct FileServerState {
  loader: LedgerLoader,
  journal_dir: PathBuf,
}

async fn serve_file(
  State(state): State<FileServerState>,
  Query(query): Query<FileQuery>,
) -> Response {
  // Build a whitelist of files actually referenced in the journal so that
  // the server can only serve documents the journal author has explicitly
  // attached to a transaction. Paths are resolved relative to the journal
  // directory (mirroring how the journal author wrote them) and compared
  // after canonicalization so different spellings of the same file collapse.
  let ledger = match state.loader.load() {
    Ok(l) => l,
    Err(_) => return StatusCode::INTERNAL_SERVER_ERROR.into_response(),
  };

  let mut allowed: HashSet<PathBuf> = HashSet::new();
  for tx in &ledger.transactions {
    for f in &tx.files {
      let candidate = state.journal_dir.join(f);
      if let Ok(c) = candidate.canonicalize() {
        allowed.insert(c);
      }
    }
  }

  let candidate = state.journal_dir.join(&query.path);
  let resolved = match candidate.canonicalize() {
    Ok(p) => p,
    Err(_) => return StatusCode::NOT_FOUND.into_response(),
  };

  if !allowed.contains(&resolved) {
    return StatusCode::FORBIDDEN.into_response();
  }

  match tokio::fs::read(&resolved).await {
    Ok(bytes) => {
      ([("content-type", mime_for(&resolved))], bytes).into_response()
    }
    Err(_) => StatusCode::NOT_FOUND.into_response(),
  }
}

pub async fn start(
  loader: LedgerLoader,
  journal_dir: PathBuf,
  port: u16,
) -> anyhow::Result<()> {
  let disk_assets_available = dev_mode()
    && ["transity.js", "transity.css", "transity.wasm"]
      .iter()
      .all(|n| disk_asset_path(n).exists());

  if !embedded_assets::ASSETS_AVAILABLE && !disk_assets_available {
    return Err(anyhow::anyhow!(
      "Frontend assets are not available.\n\
       For development, run `make dev`.\n\
       For installation, run `make server-build && make install`."
    ));
  }

  let addr = SocketAddr::from(([127, 0, 0, 1], port));

  let app = Router::new()
    .route("/pkg/transity.js", axum::routing::get(serve_js))
    .route("/pkg/transity.css", axum::routing::get(serve_css))
    .route("/pkg/transity.wasm", axum::routing::get(serve_wasm))
    .route("/pkg/transity_bg.wasm", axum::routing::get(serve_wasm))
    .route("/favicon.ico", axum::routing::get(serve_favicon))
    .route(
      "/files",
      axum::routing::get(serve_file).with_state(FileServerState {
        loader: loader.clone(),
        journal_dir: journal_dir.clone(),
      }),
    )
    .route(
      "/api/{*fn_name}",
      axum::routing::any({
        let loader = loader.clone();
        move |req| {
          let loader = loader.clone();
          handle_server_fns_with_context(
            move || provide_context(loader.clone()),
            req,
          )
        }
      }),
    )
    .fallback(serve_shell);

  eprintln!("Serving on http://{}", addr);
  let listener = tokio::net::TcpListener::bind(&addr).await?;
  axum::serve(listener, app.into_make_service()).await?;
  Ok(())
}
