
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ~/smaccmpilot-build/ivory/ivory
	cabal sandbox add-source ~/smaccmpilot-build/ivory/ivory-hw
	cabal sandbox add-source ~/smaccmpilot-build/ivory/ivory-opts
	cabal sandbox add-source ~/smaccmpilot-build/ivory/ivory-stdlib
	cabal sandbox add-source ~/smaccmpilot-build/ivory/ivory-backend-c
	cabal install --dependencies-only
