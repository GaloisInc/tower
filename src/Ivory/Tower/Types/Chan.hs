{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Chan where

import qualified Ivory.Tower.AST as AST

data Chan a = Chan AST.Chan

newtype ChanInput a = ChanInput (Chan a)
newtype ChanOutput a = ChanOutput (Chan a)

