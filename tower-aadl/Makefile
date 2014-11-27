default: build

IVORY= \
  ivory \
  ivory-artifact \
  ivory-backend-aadl \
  ivory-backend-c \
  ivory-opts \
  ivory-stdlib \

TOWER= \
  ../tower

IVORY_REPO ?= ../../ivory

IVORYLOC=$(foreach p, $(IVORY), $(IVORY_REPO)/$(p)/)

PACKAGES=$(IVORYLOC) $(TOWER)

.PHONY: build
build:
	cabal build

cabal.sandbox.config:
	cabal sandbox init
	echo "tests: True" >> cabal.sandbox.config

.PHONY: install-deps
install-deps: cabal.sandbox.config
	-rm -rf dist
	cabal sandbox add-source $(PACKAGES)
	cabal install $(PACKAGES)
  # Requires a local .cabal file.
	cabal install --dependencies-only

.PHONY: runtest
runtest:
	cabal run test -- --help
	cabal run test -- --aadl-dir="aadl-files"

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist
