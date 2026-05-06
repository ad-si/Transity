fn main() {
  let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
  let pkg_dir = std::path::Path::new(&manifest_dir).join("target/site/pkg");

  let out_dir = std::env::var("OUT_DIR").unwrap();
  let dest = std::path::Path::new(&out_dir).join("embedded_assets.rs");

  let js = pkg_dir.join("transity.js");
  let css = pkg_dir.join("transity.css");
  let wasm = pkg_dir.join("transity.wasm");

  let assets_present = js.exists() && css.exists() && wasm.exists();
  let ssr_enabled = std::env::var("CARGO_FEATURE_SSR").is_ok();

  if ssr_enabled && !assets_present {
    println!(
      "cargo:warning=Leptos hydration assets missing in {}. \
       Run `cargo leptos build` (or `make server-build`) before \
       installing — `transity server` will exit at startup otherwise.",
      pkg_dir.display()
    );
  }

  let code = if assets_present {
    format!(
      r#"
pub const JS: &[u8] = include_bytes!("{}");
pub const CSS: &[u8] = include_bytes!("{}");
pub const WASM: &[u8] = include_bytes!("{}");
pub const ASSETS_AVAILABLE: bool = true;
"#,
      js.display(),
      css.display(),
      wasm.display(),
    )
  } else {
    r#"
pub const JS: &[u8] = b"";
pub const CSS: &[u8] = b"";
pub const WASM: &[u8] = b"";
pub const ASSETS_AVAILABLE: bool = false;
"#
    .to_string()
  };

  std::fs::write(&dest, code).unwrap();

  println!("cargo:rerun-if-changed=target/site/pkg/transity.js");
  println!("cargo:rerun-if-changed=target/site/pkg/transity.css");
  println!("cargo:rerun-if-changed=target/site/pkg/transity.wasm");
}
