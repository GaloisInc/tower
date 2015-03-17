{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  ) where

import Data.Monoid
import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.EmitterCode

data HandlerCode (a :: Area *) = HandlerCode
  { handlercode_callbacks :: AST.Thread -> ModuleDef
  , handlercode_emitters :: AST.Tower -> AST.Thread -> [SomeEmitterCode]
  }

instance Monoid (HandlerCode area) where
  mempty = HandlerCode
    { handlercode_callbacks = const $ return ()
    , handlercode_emitters = mempty
    }
  mappend a b = HandlerCode
    { handlercode_callbacks = \ t -> handlercode_callbacks a t >> handlercode_callbacks b t
    , handlercode_emitters = handlercode_emitters a `mappend` handlercode_emitters b
    }
