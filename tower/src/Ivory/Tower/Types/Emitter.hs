{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Emitter where

import qualified Ivory.Tower.AST as AST
import Ivory.Language

data Emitter (a :: Area *) = Emitter AST.Emitter

emitterProcName :: Emitter a -> String
emitterProcName (Emitter e) = AST.emitterProcName e
