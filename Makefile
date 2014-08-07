.PHONY: build clean test

build:
	@cabal build

clean:
	@cabal clean

test:
	@cabal test -j --show-details=always --test-option=--color
