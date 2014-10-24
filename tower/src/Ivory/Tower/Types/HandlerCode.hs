{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  , emptyHandlerCode
  , insertHandlerCodeCallback
  , insertHandlerCodeEmitter
  ) where

import Ivory.Language
import Ivory.Tower.Types.EmitterCode

data HandlerCode (a :: Area *) = HandlerCode
  { handlercode_callbacks :: ModuleDef
  , handlercode_emitters :: [SomeEmitterCode]
  }

emptyHandlerCode :: HandlerCode a
emptyHandlerCode = HandlerCode
  { handlercode_callbacks = return ()
  , handlercode_emitters = []
  }

insertHandlerCodeCallback :: ModuleDef -> HandlerCode a -> HandlerCode a
insertHandlerCodeCallback m c =
  c { handlercode_callbacks = handlercode_callbacks c >> m }

insertHandlerCodeEmitter :: EmitterCode a -> HandlerCode b -> HandlerCode b
insertHandlerCodeEmitter e c =
  c { handlercode_emitters = SomeEmitterCode e : handlercode_emitters c}

