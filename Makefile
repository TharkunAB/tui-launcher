SHELL := /bin/bash

CABAL := cabal
CABAL_FILE := tuilauncher.cabal
HS_DIRS := app src test
HS_FILES := $(shell find $(HS_DIRS) -type f -name '*.hs' | sort)

.PHONY: build test lint

build:
	$(CABAL) build exe:tuilauncher

test:
	$(CABAL) test

lint:
	fourmolu --mode check $(HS_FILES)
	cabal-gild --mode check --input $(CABAL_FILE)
	hlint $(HS_DIRS)
