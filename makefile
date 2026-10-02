.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: server-build
	cargo build


.PHONY: test
test: test-unit test-cli


.PHONY: test-unit
test-unit:
	cargo test --bin transity


.PHONY: test-cli
test-cli:
	cargo test --test cli_snapshots


.PHONY: update-snapshots
update-snapshots:
	cargo insta review


.PHONY: format
format:
	cargo clippy --fix --allow-dirty > /dev/null 2>&1
	cargo fmt


.PHONY: install
install: server-build
	cargo install --path .


# Apache ECharts gets embedded into the binary for the trend charts
node_modules/echarts/dist/echarts.min.js: package.json bun.lock
	bun install
	touch $@


.PHONY: server-build
server-build: node_modules/echarts/dist/echarts.min.js
	cargo leptos build


.PHONY: dev
dev: node_modules/echarts/dist/echarts.min.js
	cargo leptos watch -- server examples/journal.yaml


.PHONY: wasm-build
wasm-build:
	touch src/lib.rs
	wasm-pack build \
		--out-dir webapp/pkg \
		--target web \
		--dev \
		--no-default-features \
		--features wasm


.PHONY: wasm-build-production
wasm-build-production:
	wasm-pack build \
		--out-dir webapp/pkg \
		--target web \
		--release \
		--no-default-features \
		--features wasm


.PHONY: docs
docs: wasm-build-production
	rm -rf docs
	mkdir -p docs/docs
	cp -R webapp/. docs/
	mdbook build docs_src


.PHONY: clean
clean:
	cargo clean
	rm -rf webapp/pkg docs
