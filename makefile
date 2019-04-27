all: docs


changelog.md: .git
	# git config changelog.format '- %s (%h)'
	# git changelog
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


docs: output
	npx parcel build webapp/index.html \
	    --public-url /transity \
		--no-source-maps \
		--out-dir $@


output: src package.json package-lock.json psc-package.json .pulp-cache

.pulp-cache: .psc-package
	npx pulp build

.psc-package: node_modules
	npx psc-package install

node_modules:
	npm install


.PHONY: test
test: output
	npx pulp test


.PHONY: clean
clean:
	-rm -rf \
		.psc-package \
		.psci_modules \
		.pulp-cache \
		docs \
		node_modules \
		output
