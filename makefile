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


.PHONY: test-cli
test-cli: | node_modules
	bunx spago run -- \
		balance examples/journal.yaml \
		> /dev/null

	bunx spago run -- \
		balance examples/journal.yaml examples/journal-only-transactions.yaml \
		> /dev/null

	# Following command should fail
	@bunx spago run -- \
		balance examples/journal.yaml examples/journal-broken-transaction.yaml \
		&& echo "❌ This must fail" && exit 1 \
		|| echo "✅ Balance printed an error"

	bunx spago run -- \
		unused-files examples/receipts examples/journal.yaml \
		2> /dev/null

	bunx spago run -- \
		unused-files \
			examples/receipts \
			examples/journal.yaml \
			examples/journal-only-transactions.yaml \
		2> /dev/null


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
