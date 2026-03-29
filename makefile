.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build:
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
install:
	cargo install --debug --path .


.PHONY: clean
clean:
	cargo clean
