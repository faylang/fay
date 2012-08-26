PROJECT   = fay
CABAL     = cabal-dev
OPEN      = xdg-open
FAY       = dist/build/fay/fay
FAY-DOCS  = dist/build/fay-docs/fay-docs
FAY-TESTS = dist/build/fay-tests/fay-tests

PATH := $(PWD)/cabal-dev/bin:$(PATH)

.PHONY: all configure install doc test

all: build

configure install:
	$(CABAL) $@ -fdevel --enable-executable-profiling --enable-library-profiling

build: configure
	$(CABAL) build

doc:
	$(CABAL) haddock
	$(OPEN) "dist/doc/html/$(PROJECT)/index.html"

test:
	$(FAY-TESTS) +RTS $(RTS)
