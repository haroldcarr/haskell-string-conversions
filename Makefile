.PHONY: buld
build:
	hpack
	cabal build

.PHONY: repl
repl:
	hpack
	cabal repl

.PHONY: repl-test
repl-test:
	hpack
	cabal repl test

.PHONY: test
test:
	hpack
	cabal test --test-show-details=direct

.PHONY: run
run:
	hpack
	cabal run main

.PHONY: clean
clean:
	hpack
	cabal clean
