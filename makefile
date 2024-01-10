.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: all
all: index.js docs


.PHONY: build
build:
	npx spago build


changelog.md: .git
	# git config changelog.format '- %s (%h)'
	# git changelog
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


# TODO: Fix after dynamic requires of "fs" are supported
index.js: src node_modules
	npx spago bundle-app \
		--platform=node \
		--minify


# The specified target is configured in package.json
docs: output
	npx parcel build webapp/index.html \
		--public-url /Transity \
		--no-source-maps \
		--target $@


# Correct paths for assets during local development
# Use e.g. Vercel's "serve" like this: `serve docs-dev`.
# The specified target is configured in package.json.
docs-dev: output index.js
	npx parcel build webapp/index.html \
		--no-source-maps \
		--target $@


.PHONY: bundle-spago
bundle-spago:
	npx spago bundle-module --platform=node

.PHONY: minify
minify:
	npx uglifyjs \
		--compress \
		--mangle \
		--output index.js \
		index.js

.PHONY: bundle
bundle: minify bundle-spago


output: src spago.yaml node_modules
	npx spago build


node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


readme.md:
	npx markdown-toc -i $@


##### TESTING ######

.PHONY: lint-js
lint-js:
	npx eslint \
		--max-warnings 0 \
		--ignore-path .gitignore \
		scripts

.PHONY: test-spago
test-spago:
	npx spago test

.PHONY: test
test: lint-js test-spago

.PHONY: test-watch
test-watch: lint-js output
	npx spago test --watch


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
