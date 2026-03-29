.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"

.PHONY: build
build:
	cargo build

.PHONY: bundle
bundle:
	cargo build --release
	cp target/release/transity .

.PHONY: test
test: test-unit test-cli

.PHONY: test-unit
test-unit:
	cargo test --lib

.PHONY: test-cli
test-cli:
	cargo test --test cli_snapshots

.PHONY: update-snapshots
update-snapshots:
	cargo insta review

.PHONY: test-spago
test-spago: | purescript/node_modules
	cd purescript && bunx spago test

.PHONY: lint-js
lint-js: | purescript/node_modules
	cd purescript && bunx eslint --max-warnings 0 --ignore-pattern .gitignore scripts

purescript/node_modules: purescript/package.json
	cd purescript && bun install

.PHONY: format
format:
	-cargo fmt 2>/dev/null || true
	@if [ -d purescript ] && [ -d purescript/node_modules ]; then \
		cd purescript && bunx pursfmt format-in-place "src/**/*.purs" "test/**/*.purs"; \
	fi

.PHONY: clean
clean:
	cargo clean
	-rm -f transity
	-cd purescript && rm -rf node_modules output .spago .parcel-cache
