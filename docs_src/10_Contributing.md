## Contributing

### Getting Started

Check out the `makefile` for all available tasks.
By simply running `make` it will also print a short list of them.


Prerequisites:
[Rust], [cargo-leptos], and [Bun] (for the bundled Apache ECharts).
Building the browser demo and the documentation additionally requires
[wasm-pack] and [mdBook].

[Rust]: https://www.rust-lang.org/tools/install
[cargo-leptos]: https://github.com/leptos-rs/cargo-leptos
[Bun]: https://bun.sh
[wasm-pack]: https://github.com/rustwasm/wasm-pack
[mdBook]: https://github.com/rust-lang/mdBook


**Additional helpful commands:**

Build and run:

```shell
cargo run -- balance examples/journal.yaml
```

Run the web server with live reload:

```shell
make dev
```

Install the `transity` executable from the local source:

```shell
make install
```

Run the tests:

```shell
make test
```

If you intentionally changed the output format,
review and accept the updated snapshots with `make update-snapshots`.


### Documentation

Build the documentation website (including the in-browser demo)
into the `docs` directory:

```sh
make docs
```

Generate and open the API documentation of the Rust crate with:

```sh
cargo doc --open
```


### XLSX Generation

Check out https://stackoverflow.com/q/18334314/1850340 for an explanation
of the XML fields.


### Generate Screenshots

Use [asciinema] to generate the terminal recording:

```shell
asciinema rec \
  --title 'Transity' \
  recording.json
```

Change the size of the terminal in the `recording.json` file to
approximately

```json
  "width": 80,
  "height": 18,
```


Then use [svg-term] to generate the SVG image:

```shell
svg-term \
  --no-cursor \
  --at 99999 \
  --window \
  --term iterm2 \
  --profile ../../dotfiles/terminal/adius.itermcolors \
  < recording.json \
  > recording.svg
```


And lastly convert all CSS styles to inline styles
because of issue https://github.com/marionebl/svg-term-cli/issues/5

[asciinema]: https://github.com/asciinema/asciinema
[svg-term]: https://github.com/marionebl/svg-term-cli
