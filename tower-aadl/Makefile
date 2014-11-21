all: build

BIN=.cabal-sandbox/bin

IVORY= \
  ivory \
  ivory-artifact \
  ivory-backend-aadl \
  ivory-backend-c \
  ivory-opts \
  ivory-stdlib \

TOWER= \
  ../tower

IVORYLOC=$(foreach p, $(IVORY), ../../ivory/$(p)/)

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

# .PHONY: test
# test: build
# 	./$(BIN)/ivory-c-clang-test clang-test-dir
# 	./$(BIN)/ivory-fibtutorial
# 	./$(BIN)/ivory-concrete
# 	# Created from ivory-model-check
# 	./$(shell find ivory-model-check/dist/ -path "*/test/test")

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist
