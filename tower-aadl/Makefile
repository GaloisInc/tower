IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ..
include ../Makefile.sandbox

test:
	cabal run test -- --src-dir=test_codegen

.PHONY: test-echronos
test-echronos:
	cabal run test-echronos -- --src-dir=test_echronos_codegen
