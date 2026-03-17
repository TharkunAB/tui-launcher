SHELL := /bin/bash

CABAL := cabal
CABAL_FILE := tui-launcher.cabal
CABAL_DIR := $(CURDIR)/.cabal
XDG_CACHE_HOME := $(CURDIR)/.cache
HS_DIRS := app src test
HS_FILES := $(shell find $(HS_DIRS) -type f -name '*.hs' | sort)

export CABAL_DIR
export XDG_CACHE_HOME

.PHONY: build test lint snapshots setup-cabal

setup-cabal:
	mkdir -p $(CABAL_DIR) $(XDG_CACHE_HOME)

build: setup-cabal
	$(CABAL) build exe:tui-launcher

test: setup-cabal
	$(CABAL) test

lint:
	fourmolu --mode check $(HS_FILES)
	cabal-gild --mode check --input $(CABAL_FILE)
	hlint $(HS_DIRS)

snapshots: setup-cabal
	./scripts/generate-snapshots.sh
