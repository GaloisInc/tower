#! /bin/sh

DSL=../..
SANDBOX=$DSL/cabal-dev

cabal-dev -s $SANDBOX ghci
