all: index.js docs


changelog.md: .git
	# git config changelog.format '- %s (%h)'
	# git changelog
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


index.js: src
	npm run bundle


docs: output
	npx parcel build webapp/index.html \
		--public-url /transity \
		--no-source-maps \
		--out-dir $@


docs-dev: output index.js
	npx parcel build webapp/index.html \
		--no-source-maps \
		--out-dir $@


output: src package.json package-lock.json packages.dhall spago.dhall node_modules
	npx --no-install spago build


node_modules: package.json package-lock.json
	npm install


readme.md:
	npx markdown-toc -i $@


.PHONY: test
test: output
	npx --no-install spago test
	npm run lint-js


.PHONY: clean
clean:
	-rm -rf \
		.cache \
		.parcel-cache \
		.spago \
		docs \
		docs-dev \
		node_modules \
		output
