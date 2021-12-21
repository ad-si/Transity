all: index.js docs


changelog.md: .git
	# git config changelog.format '- %s (%h)'
	# git changelog
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


index.js: src node_modules
	npm run bundle


# The specified target is configured in package.json
docs: output
	npx parcel build webapp/index.html \
		--public-url /transity \
		--no-source-maps \
		--target $@


# Correct paths for assets during local development
# Use e.g. Vercel's "serve" like this: `serve docs-dev`.
# The specified target is configured in package.json.
docs-dev: output index.js
	npx parcel build webapp/index.html \
		--no-source-maps \
		--target $@


output: src package.json package-lock.json \
 packages.dhall spago.dhall node_modules
	npx --no-install spago build


node_modules: package.json package-lock.json
	if test ! -d $@; then npm install; fi


readme.md:
	npx markdown-toc -i $@


.PHONY: test
test: output
	npx --no-install spago test
	npm run lint-js


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
