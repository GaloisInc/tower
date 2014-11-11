
default:
	cabal build
	cabal test

create-sandbox:
	cabal sandbox init
	cabal configure --enable-tests
	cabal install --dependencies-only --enable-tests
