
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../../ivory/ivory
	cabal sandbox add-source ../../ivory/ivory-artifact
	cabal install --dependencies-only
