{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Chan where

import qualified Ivory.Tower.AST as AST
import Ivory.Language

data Chan (a :: Area *) = Chan AST.Chan
  deriving (Eq)

newtype ChanInput (a :: Area *) = ChanInput (Chan a)
  deriving (Eq)

newtype ChanOutput (a :: Area *) = ChanOutput (Chan a)
  deriving (Eq)
