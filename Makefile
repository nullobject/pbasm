build:
	@cabal build

test:
	@cabal test -j --show-details=always

.PHONY: test
