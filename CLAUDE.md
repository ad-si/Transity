# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project shape

Transity is a plain-text accounting tool. The repo contains a Rust port of the
formerly-PureScript implementation (the `purescript/` directory still holds the
legacy source but is not built — it is excluded from the Cargo package and does
not need to be touched for current work). The active code is a single Cargo
crate that produces three artifacts from one shared library (`src/lib.rs`):

- **CLI binary** (`src/main.rs`) — `transity <subcommand> <journal.yaml>`.
- **Leptos web server** (`src/server.rs` + `src/app.rs`) — `transity server …`,
  serves a CSR app whose JS/CSS/WASM bundle is embedded into the binary at
  build time via `build.rs`.
- **Browser-only WASM demo** (`src/wasm.rs`) — used by `webapp/` for the
  in-browser playground that powers the docs site.

Cargo features gate which of these compile:
`cli` (clap + xlsx), `ssr` (axum/tokio/leptos server), `hydrate` (leptos CSR
mount), `wasm` (the standalone webapp). `default = ["ssr"]`, so plain
`cargo build` produces the full server-capable CLI. Anything `#[cfg(...)]`-gated
in `lib.rs` is there to keep WASM/CLI/SSR builds compiling cleanly — keep new
CLI-only code behind `#[cfg(feature = "cli")]`.

## Common commands

The makefile is the source of truth — prefer its targets.

```
make build              # server-build + cargo build
make test               # unit tests + CLI snapshot tests
make test-unit          # cargo test --bin transity
make test-cli           # cargo test --test cli_snapshots
make update-snapshots   # cargo insta review (after intentional output changes)
make format             # cargo clippy --fix && cargo fmt
make server-build       # cargo leptos build (produces target/site/pkg/*)
make dev                # live-reload server against examples/journal.yaml
make wasm-build         # builds webapp/pkg/ for the browser demo
make docs               # builds docs/ via mdbook + the browser demo
make install            # server-build + cargo install --path .
```

Run a single test by name: `cargo test --test cli_snapshots test_balance` or
`cargo test --bin transity <name>`.

`cargo install --path .` requires that the Leptos assets in `target/site/pkg/`
already exist — `build.rs` emits a warning otherwise, and `transity server`
will refuse to start because the assets are `include_bytes!`’d into the binary.
`make build` and `make install` both chain `server-build` first; only plain
`cargo build` / `cargo install` skip it.

## Architecture notes worth knowing up front

- **Domain model** (`src/lib.rs`): `Ledger` → `Entity` → `Account` → `Balance`,
  plus `Transaction` containing `Transfer`s. `LedgerRaw`/`*Raw` types mirror
  the YAML schema 1:1; `Ledger::from_raw` converts and validates. Amounts use
  `BigRational` (not `f64`) so accounting math stays exact — only convert to
  `f64` for display.
- **Account separators are subtle.** `Ledger.separator` defaults to `/`. When
  no `config.separator` is set in the YAML, both `:` and `/` are accepted and
  normalized to the default; when the journal sets one explicitly, only that
  character is a separator and the others are literal. `original_account_ids`
  preserves the as-written form for error messages. Multi-file loads
  (`load_and_verify`) force every additional file to use the first file’s
  separator config so merged ledgers stay consistent.
- **Balance computation** flows through `get_balance_data` →
  `expand_account_hierarchy` → `balance_data_to_entries`. `_default_` is the
  implicit leaf account that appears when an entity is referenced without an
  explicit account; `add_account_default` and `norm_acc_id` add and strip it.
- **Verification balances**: transactions tagged with the magic note
  `"___BALANCE___"` (built from entity balance declarations) are checked but
  not applied — see `verify_ledger_balances`. This intentionally mirrors the
  PureScript behavior; don’t “fix” it by draining the account.
- **Tag filter grammar** (`parse_tag_expr`): `tag`, `not`, `and`, `or`, parens.
  Used by every CLI command that takes `--tag`.
- **Display layer** computes `WidthRecord`s for column alignment and matches
  the PureScript output byte-for-byte — that contract is what the snapshot
  tests in `tests/snapshots/` enforce. If you intentionally change formatting,
  re-run `make update-snapshots` and review the diff; otherwise treat
  snapshot diffs as bugs.
- **Server vs WASM data path**: the server pre-loads a `Ledger` once at
  startup, stashes it in Leptos context, and exposes it via a `#[server]`
  function (`get_balance` in `app.rs`). The standalone WASM build instead
  parses YAML in the browser via `wasm::get_balance`. The two share
  `src/lib.rs` — keep platform-specific code under the appropriate `cfg`.

## Style

- `rustfmt.toml` sets `max_width = 80` and `tab_spaces = 2`. Run `make format`
  before committing; clippy is part of that target.
- Snapshot tests strip ANSI escapes before asserting, so colored output is
  fine in production code.
- Examples in `examples/` (especially `journal.yaml`) are the canonical
  fixtures driving snapshot tests — do not edit casually.
