PROJECT     = fay
CABAL       = cabal-dev
CABAL_FLAGS = -fdevel --enable-executable-profiling --enable-library-profiling
OPEN        = xdg-open
FAY         = dist/build/fay/fay
FAY-DOCS    = dist/build/fay-docs/fay-docs
FAY-TESTS   = dist/build/fay-tests/fay-tests

PATH := $(PWD)/cabal-dev/bin:$(PATH)

.PHONY: all configure install doc test

all: test

configure:
	$(CABAL) $@ $(CABAL_FLAGS)

install:
	$(CABAL) $@ $(CABAL_FLAGS) --force-reinstalls

build: configure
	$(CABAL) $@

doc:
	$(CABAL) haddock
	$(OPEN) "dist/doc/html/$(PROJECT)/index.html"

test: build
	$(FAY-TESTS) +RTS $(RTS)
