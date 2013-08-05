name:                ivory-tower-frontend
version:             0.0.1.0
author:              Galois, Inc.
maintainer:          pat@galois.com
category:            Language
build-type:          Simple
cabal-version:       >= 1.10
license:             BSD3

library
  exposed-modules:      Ivory.Tower.Frontend
  build-depends:        base >= 4.6,
                        srcloc,
                        template-haskell >= 2.8,
                        directory,
                        filepath,
                        process,
                        cmdlib >= 0.3.5,
                        containers,
                        wl-pprint,
                        monadLib,
                        ivory,
                        ivory-stdlib,
                        ivory-tower,
                        ivory-tower-freertos

  hs-source-dirs:       src
  default-language:     Haskell2010
  ghc-options:          -Wall -fno-warn-orphans
