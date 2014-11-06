
.PHONY: default
default:
	cabal build

.PHONY: freertos-test
freertos-test:
	rm -rf dist
	cabal sandbox add-source ../ivory-freertos-bindings
	cabal install --dependencies-only
	cabal run freertos-bindings-test -- --src-dir=freertos-test

.PHONY: simple-test
simple-test:
	rm -rf dist
	cabal sandbox add-source ../ivory-freertos-bindings
	cabal sandbox add-source ../tower
	cabal sandbox add-source ../tower-freertos
	cabal install --dependencies-only
	cabal run tower-example-simple -- --src-dir=tower-example-simple

.PHONY: create-sandbox
create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../../ivory/ivory
	cabal sandbox add-source ../../ivory/ivory-artifact
	cabal sandbox add-source ../../ivory/ivory-hw
	cabal sandbox add-source ../../ivory/ivory-opts
	cabal sandbox add-source ../../ivory/ivory-stdlib
	cabal sandbox add-source ../../ivory/ivory-backend-c
	cabal sandbox add-source ../tower
	cabal sandbox add-source ../ivory-freertos-bindings
	cabal sandbox add-source ../tower-freertos
	cabal install --dependencies-only
