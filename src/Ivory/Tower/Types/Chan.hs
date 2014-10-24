{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Chan where

import qualified Ivory.Tower.AST as AST
import Ivory.Language

data Chan (a :: Area *) = Chan AST.Chan

newtype ChanInput (a :: Area *) = ChanInput (Chan a)
newtype ChanOutput (a :: Area *) = ChanOutput (Chan a)

