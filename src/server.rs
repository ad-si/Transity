use axum::response::{Html, IntoResponse};
use axum::Router;
use leptos::prelude::*;
use leptos_axum::handle_server_fns_with_context;
use std::net::SocketAddr;
use std::path::PathBuf;

use crate::Ledger;

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

pub async fn start(ledger: Ledger, port: u16) -> anyhow::Result<()> {
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
      "/api/{*fn_name}",
      axum::routing::any({
        let ledger = ledger.clone();
        move |req| {
          let ledger = ledger.clone();
          handle_server_fns_with_context(
            move || provide_context(ledger.clone()),
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
