use axum::response::IntoResponse;
use axum::Router;
use leptos::prelude::*;
use leptos_axum::{generate_route_list, LeptosRoutes};
use std::net::SocketAddr;

use crate::app::{shell, App};
use crate::Ledger;

#[allow(dead_code)]
mod embedded_assets {
  include!(concat!(env!("OUT_DIR"), "/embedded_assets.rs"));
}

async fn serve_js() -> impl IntoResponse {
  (
    [("content-type", "application/javascript")],
    embedded_assets::JS,
  )
}

async fn serve_css() -> impl IntoResponse {
  ([("content-type", "text/css")], embedded_assets::CSS)
}

async fn serve_wasm() -> impl IntoResponse {
  (
    [("content-type", "application/wasm")],
    embedded_assets::WASM,
  )
}

async fn serve_favicon() -> impl IntoResponse {
  axum::http::StatusCode::NO_CONTENT
}

pub async fn start(ledger: Ledger, port: u16) -> anyhow::Result<()> {
  let addr = SocketAddr::from(([127, 0, 0, 1], port));

  // Set env vars so get_configuration works even without cargo-leptos
  std::env::set_var("LEPTOS_OUTPUT_NAME", "transity");
  std::env::set_var("LEPTOS_SITE_ROOT", "target/site");
  std::env::set_var("LEPTOS_SITE_PKG_DIR", "pkg");
  std::env::set_var("LEPTOS_SITE_ADDR", addr.to_string());

  let conf = get_configuration(None).unwrap();
  let leptos_options = conf.leptos_options;
  let routes = generate_route_list(App);

  let app = Router::new()
    .route("/pkg/transity.js", axum::routing::get(serve_js))
    .route("/pkg/transity.css", axum::routing::get(serve_css))
    .route("/pkg/transity.wasm", axum::routing::get(serve_wasm))
    .route("/pkg/transity_bg.wasm", axum::routing::get(serve_wasm))
    .route("/favicon.ico", axum::routing::get(serve_favicon))
    .leptos_routes_with_context(
      &leptos_options,
      routes,
      {
        let ledger = ledger.clone();
        move || provide_context(ledger.clone())
      },
      {
        let leptos_options = leptos_options.clone();
        move || shell(leptos_options.clone())
      },
    )
    .fallback(leptos_axum::file_and_error_handler(shell))
    .with_state(leptos_options);

  eprintln!("Serving on http://{}", addr);
  let listener = tokio::net::TcpListener::bind(&addr).await?;
  axum::serve(listener, app.into_make_service()).await?;
  Ok(())
}
