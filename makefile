all: output


output:
	npx pulp build


.PHONY: postinstall
postinstall: install output


.PHONY: test
test:
	npx pulp test


.PHONY: install
install:
	npx bower install


.PHONY: clean
clean:
	-rm -rf output
