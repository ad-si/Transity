.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: all
all: changelog.md readme.md index.js docs output


.PHONY: build
build: src/CliSpec/JsonEmbed.purs | node_modules
	npx spago build


changelog.md: .git | node_modules
	# git config changelog.format '- %s (%h)'
	# git changelog
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


index.js: $(wildcard src/**/*) src/CliSpec/JsonEmbed.purs spago.yaml | node_modules
	npx spago bundle \
		--platform node \
		--minify

.PHONY: bundle
bundle: index.js


# The specified target is configured in package.json
docs: output | node_modules
	npx parcel build webapp/index.html \
		--public-url /Transity \
		--no-source-maps \
		--target $@


# Correct paths for assets during local development
# Use e.g. Vercel's "serve" like this: `serve docs-dev`.
# The specified target is configured in package.json.
docs-dev: output index.js | node_modules
	npx parcel build webapp/index.html \
		--no-source-maps \
		--target $@


define JsonEmbedStart
module CliSpec.JsonEmbed where

fileContent :: String
fileContent = """
endef
export JsonEmbedStart

src/CliSpec/JsonEmbed.purs: cli-contract.ncl cli-spec.ncl
	echo "$$JsonEmbedStart" > $@

	echo \
		'(import "cli-contract.ncl") & (import "cli-spec.ncl")' \
		| nickel --color=always export --format json \
		>> $@

	echo '"""' >> $@


output: src src/CliSpec/JsonEmbed.purs spago.yaml | node_modules
	npx spago build


node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


readme.md: | node_modules
	npx markdown-toc -i $@


##### TESTING ######

.PHONY: lint-js
lint-js: | node_modules
	npx eslint \
		--max-warnings 0 \
		--ignore-path .gitignore \
		scripts

.PHONY: test-spago
test-spago: src/CliSpec/JsonEmbed.purs | node_modules
	npx spago test

.PHONY: test
test: lint-js test-spago

.PHONY: test-watch
test-watch: src/CliSpec/JsonEmbed.purs | node_modules
	watchexec \
		--exts purs \
		'npx spago test'


.PHONY: clean
clean:
	-rm -rf \
		.parcel-cache \
		.spago \
		docs \
		docs-dev \
		index.js \
		node_modules \
		output
