IVORY_REPO ?= ../../ivory
TOWER_REPO ?= ..
include ../Makefile.sandbox

test:
	cabal run test -- --help
	cabal run test -- --aadl-dir="aadl-files"
