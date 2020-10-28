all: docs


changelog.md: .git
	# git config changelog.format '- %s (%h)'
	# git changelog
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


index.js: src
	spago bundle-module


docs: output index.js
	npx parcel build webapp/index.html \
		--public-url /transity \
		--no-source-maps \
		--out-dir $@


docs-dev: output index.js
	npx parcel build webapp/index.html \
		--no-source-maps \
		--out-dir $@


output: src package.json package-lock.json packages.dhall spago.dhall node_modules
	spago build


node_modules: package.json package-lock.json
	npm install


readme.md:
	npx markdown-toc -i $@


.PHONY: test
test: output
	spago test
	npm run lint-js


.PHONY: clean
clean:
	-rm -rf \
		.parcel-cache \
		.spago \
		docs \
		docs-dev \
		node_modules \
		output
