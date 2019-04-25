all: output


changelog.md: .git
	npx conventional-changelog \
		--infile $@ \
		--same-file \
		--output-unreleased


output: src package.json package-lock.json psc-package.json
	npx pulp build


.PHONY: postinstall
postinstall: install output


.PHONY: test
test:
	npx pulp test


.PHONY: install
install:
	npx psc-package install


.PHONY: clean
clean:
	-rm -rf output
