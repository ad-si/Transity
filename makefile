.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: all
all: changelog.md readme.md index.js docs output


.PHONY: format
format: | node_modules
	bunx pursfmt format-in-place \
		"src/**/*.purs" \
		"test/**/*.purs"


.PHONY: build
build: | node_modules
	bunx spago build


changelog.md: .git | node_modules
	# git config changelog.format '- %s (%h)'
	# git changelog
	bunx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


srcFiles := $(shell find src -type f -name "*.purs")
index.js: $(srcFiles) spago.yaml | node_modules
	bunx spago bundle \
		--platform node \
		--minify

.PHONY: bundle
bundle: index.js


# The specified target is configured in package.json
docs/docs: | node_modules
	bunx spago run --main Build


# The specified target is configured in package.json
docs: output docs/docs | node_modules
	bunx parcel build \
		--no-source-maps \
		webapp/index.html \
		--target $@


.PHONY: docs-watch
docs-watch: output | node_modules
	bunx parcel watch \
		--no-source-maps \
		webapp/index.html \
		--target docs


output: src spago.yaml | node_modules
	bunx spago build


node_modules: package.json
	if test ! -d $@; then bun install; fi


readme.md: | node_modules
	bunx markdown-toc -i $@


##### TESTING ######

.PHONY: lint-js
lint-js: | node_modules
	bunx eslint \
		--max-warnings 0 \
		--ignore-pattern .gitignore \
		scripts


.PHONY: test-spago
test-spago: | node_modules
	bunx spago test


SNAPSHOTS := test/snapshots
strip_ansi = sed 's/\x1b\[[0-9;]*m//g'

define check_snapshot
	@bunx spago run -- $(1) 2>/dev/null \
		| $(strip_ansi) \
		| diff - $(SNAPSHOTS)/$(2).txt \
		&& echo "✅ $(2)" \
		|| (echo "❌ $(2) output differs from snapshot" && exit 1)
endef

.PHONY: test-cli
test-cli: | node_modules
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
	@bunx spago run -- unused-files examples/receipts examples/journal.yaml 2>&1 >/dev/null \
		| $(strip_ansi) \
		| grep -q "2020-01-07t1205_lunch.pdf" \
		&& echo "✅ unused-files (unreferenced file detected)" \
		|| (echo "❌ unused-files did not report unreferenced file" && exit 1)

	# unused-files: the referenced receipt should NOT be reported
	@bunx spago run -- unused-files examples/receipts examples/journal.yaml 2>&1 >/dev/null \
		| $(strip_ansi) \
		| grep -qv "2020-01-06t1217_lunch.pdf" \
		&& echo "✅ unused-files (referenced file not reported)" \
		|| (echo "❌ unused-files incorrectly flagged a referenced file" && exit 1)

	# unused-files: multi-journal still reports the same unreferenced receipt
	@bunx spago run -- unused-files examples/receipts examples/journal.yaml examples/journal-only-transactions.yaml 2>&1 >/dev/null \
		| $(strip_ansi) \
		| grep -q "2020-01-07t1205_lunch.pdf" \
		&& echo "✅ unused-files-multi (unreferenced file detected)" \
		|| (echo "❌ unused-files-multi did not report unreferenced file" && exit 1)

	# version command should exit successfully and match semver
	@bunx spago run -- version 2>/dev/null \
		| grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$$' \
		&& echo "✅ version" \
		|| (echo "❌ version output is not a semver string" && exit 1)

	# help command should mention "COMMANDS:"
	@bunx spago run -- help 2>/dev/null \
		| grep -q "COMMANDS:" \
		&& echo "✅ help" \
		|| (echo "❌ help output missing COMMANDS:" && exit 1)

	# broken journal should fail
	@bunx spago run -- \
		balance examples/journal.yaml examples/journal-broken-transaction.yaml \
		2>/dev/null \
		&& echo "❌ balance with broken journal must fail" && exit 1 \
		|| echo "✅ balance (broken journal correctly rejected)"

.PHONY: update-snapshots
update-snapshots: | node_modules
	mkdir -p $(SNAPSHOTS)
	bunx spago run -- balance examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/balance.txt
	bunx spago run -- balance examples/journal.yaml examples/journal-only-transactions.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/balance-multi.txt
	bunx spago run -- balance-all examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/balance-all.txt
	bunx spago run -- transactions examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/transactions.txt
	bunx spago run -- transactions examples/journal.yaml examples/journal-only-transactions.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/transactions-multi.txt
	bunx spago run -- transfers examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/transfers.txt
	bunx spago run -- entries examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/entries.txt
	bunx spago run -- entries-by-account examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/entries-by-account.txt
	bunx spago run -- entities examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/entities.txt
	bunx spago run -- entities-sorted examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/entities-sorted.txt
	bunx spago run -- ledger-entries examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/ledger-entries.txt
	bunx spago run -- csv examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/csv.txt
	bunx spago run -- tsv examples/journal.yaml 2>/dev/null | $(strip_ansi) > $(SNAPSHOTS)/tsv.txt
	@echo "✅ Snapshots updated"


.PHONY: test
test: test-spago test-cli lint-js


.PHONY: test-watch
test-watch: | node_modules
	watchexec \
		--exts purs \
		'bunx spago test'



.PHONY: install
install: bundle


.PHONY: clean
clean:
	-rm -f bun.lockb
	-rm -f index.js
	-rm -f package-lock.json
	-rm -rf .parcel-cache
	-rm -rf .spago
	-rm -rf docs
	-rm -rf docs-dev
	-rm -rf generated-docs
	-rm -rf node_modules
	-rm -rf output
