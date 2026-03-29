.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build:
	cargo build


.PHONY: test
test:
	cargo test


.PHONY: bundle
bundle:
	cargo build --release && cp target/release/transity .


.PHONY: test-spago
test-spago:
	cd purescript && bunx spago test


SNAPSHOTS := purescript/test/snapshots
strip_ansi = sed 's/\x1b\[[0-9;]*m//g'

define check_snapshot
	@cd purescript && bunx spago run -- $(1) 2>/dev/null \
		| $(strip_ansi) \
		| diff - ../$(SNAPSHOTS)/$(2).txt \
		&& echo "✅ $(2)" \
		|| (echo "❌ $(2) output differs from snapshot" && exit 1)
endef

.PHONY: test-cli
test-cli:
	$(call check_snapshot,balance examples/journal.yaml,balance)
	$(call check_snapshot,balance examples/journal.yaml examples/journal-only-transactions.yaml,balance-multi)
	$(call check_snapshot,balance-all examples/journal.yaml,balance-all)
	$(call check_snapshot,transactions examples/journal.yaml,transactions)
	$(call check_snapshot,transactions examples/journal.yaml examples/journal-only-transactions.yaml,transactions-multi)
	$(call check_snapshot,transfers examples/journal.yaml,transfers)
	$(call check_snapshot,entries examples/journal.yaml,entries)
	$(call check_snapshot,entries-by-account examples/journal.yaml,entries-by-account)
	$(call check_snapshot,entities examples/journal.yaml,entities)
	$(call check_snapshot,entities-sorted examples/journal.yaml,entities-sorted)
	$(call check_snapshot,ledger-entries examples/journal.yaml,ledger-entries)
	$(call check_snapshot,csv examples/journal.yaml,csv)
	$(call check_snapshot,tsv examples/journal.yaml,tsv)
	# unused-files: the one unreferenced receipt should be reported on stderr
	@cd purescript && bunx spago run -- unused-files examples/receipts examples/journal.yaml 2>&1 >/dev/null \
		| $(strip_ansi) \
		| grep -q "2020-01-07t1205_lunch.pdf" \
		&& echo "✅ unused-files (unreferenced file detected)" \
		|| (echo "❌ unused-files did not report unreferenced file" && exit 1)

	# unused-files: the referenced receipt should NOT be reported
	@cd purescript && bunx spago run -- unused-files examples/receipts examples/journal.yaml 2>&1 >/dev/null \
		| $(strip_ansi) \
		| grep -qv "2020-01-06t1217_lunch.pdf" \
		&& echo "✅ unused-files (referenced file not reported)" \
		|| (echo "❌ unused-files incorrectly flagged a referenced file" && exit 1)

	# unused-files: multi-journal still reports the same unreferenced receipt
	@cd purescript && bunx spago run -- unused-files examples/receipts examples/journal.yaml examples/journal-only-transactions.yaml 2>&1 >/dev/null \
		| $(strip_ansi) \
		| grep -q "2020-01-07t1205_lunch.pdf" \
		&& echo "✅ unused-files-multi (unreferenced file detected)" \
		|| (echo "❌ unused-files-multi did not report unreferenced file" && exit 1)

	# version command should exit successfully and match semver
	@cd purescript && bunx spago run -- version 2>/dev/null \
		| grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$$' \
		&& echo "✅ version" \
		|| (echo "❌ version output is not a semver string" && exit 1)

	# help command should mention "COMMANDS:"
	@cd purescript && bunx spago run -- help 2>/dev/null \
		| grep -q "COMMANDS:" \
		&& echo "✅ help" \
		|| (echo "❌ help output missing COMMANDS:" && exit 1)

	# broken journal should fail
	@cd purescript && bunx spago run -- \
		balance examples/journal.yaml examples/journal-broken-transaction.yaml \
		2>/dev/null \
		&& echo "❌ balance with broken journal must fail" && exit 1 \
		|| echo "✅ balance (broken journal correctly rejected)"

.PHONY: update-snapshots
update-snapshots:
	cargo insta review


.PHONY: format
format:
	cd purescript && bunx pursfmt format-in-place \
		"src/**/*.purs" \
		"test/**/*.purs"


.PHONY: lint-js
lint-js:
	cd purescript && bunx eslint \
		--max-warnings 0 \
		--ignore-pattern .gitignore \
		scripts


.PHONY: clean
clean:
	-rm -f transity
	-rm -rf target
	-cd purescript && rm -f bun.lockb
	-cd purescript && rm -f index.js
	-cd purescript && rm -f package-lock.json
	-cd purescript && rm -rf .parcel-cache
	-cd purescript && rm -rf .spago
	-cd purescript && rm -rf docs
	-cd purescript && rm -rf docs-dev
	-cd purescript && rm -rf generated-docs
	-cd purescript && rm -rf node_modules
	-cd purescript && rm -rf output
