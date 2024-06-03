.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: all
all: changelog.md readme.md index.js docs output


.PHONY: build
build: | node_modules
	bun x spago build


changelog.md: .git | node_modules
	# git config changelog.format '- %s (%h)'
	# git changelog
	bun x conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


srcFiles := $(shell find src -type f -name "*.purs")
index.js: $(srcFiles) spago.yaml | node_modules
	bun x spago bundle \
		--platform node \
		--minify

.PHONY: bundle
bundle: index.js


# The specified target is configured in package.json
docs: output | node_modules
	bun x parcel build webapp/index.html \
		--public-url /Transity \
		--no-source-maps \
		--target $@


# Correct paths for assets during local development
# Use e.g. Vercel's "serve" like this: `serve docs-dev`.
# The specified target is configured in package.json.
docs-dev: output index.js | node_modules
	bun x parcel build webapp/index.html \
		--no-source-maps \
		--target $@



output: src spago.yaml | node_modules
	bun x spago build


node_modules: package.json
	if test ! -d $@; then bun install; fi


readme.md: | node_modules
	bun x markdown-toc -i $@


##### TESTING ######

.PHONY: lint-js
lint-js: | node_modules
	bun x eslint \
		--max-warnings 0 \
		--ignore-pattern .gitignore \
		scripts


.PHONY: test-spago
test-spago: | node_modules
	bun x spago test


.PHONY: test-cli
test-cli: | node_modules
	bun x spago run -- \
		balance examples/journal.yaml \
		> /dev/null

	bun x spago run -- \
		balance examples/journal.yaml examples/journal-only-transactions.yaml \
		> /dev/null

	bun x spago run -- \
		unused-files examples/receipts examples/journal.yaml \
		2> /dev/null

	bun x spago run -- \
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
		'bun x spago test'


.PHONY: clean
clean:
	-rm -f bun.lockb
	-rm -f index.js
	-rm -f package-lock.json
	-rm -rf .parcel-cache
	-rm -rf .spago
	-rm -rf docs
	-rm -rf docs-dev
	-rm -rf node_modules
	-rm -rf output
