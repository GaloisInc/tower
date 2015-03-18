{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  ) where

import Data.Monoid
import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.EmitterCode

data HandlerCode (a :: Area *) = HandlerCode
  { handlercode_callbacks :: forall s. AST.Thread -> ([Def ('[ConstRef s a] :-> ())], ModuleDef)
  , handlercode_emitters :: AST.Tower -> AST.Thread -> [SomeEmitterCode]
  }

instance Monoid (HandlerCode area) where
  mempty = HandlerCode
    { handlercode_callbacks = mempty
    , handlercode_emitters = mempty
    }
  mappend a b = HandlerCode
    { handlercode_callbacks = handlercode_callbacks a `mappend` handlercode_callbacks b
    , handlercode_emitters = handlercode_emitters a `mappend` handlercode_emitters b
    }
