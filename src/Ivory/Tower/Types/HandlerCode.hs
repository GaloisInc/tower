
module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  , emptyHandlerCode
  , insertHandlerCodeCallback
  , insertHandlerCodeEmitter
  ) where

import Ivory.Tower.ToyObjLang
import Ivory.Tower.Types.EmitterCode

data HandlerCode = HandlerCode
  { handlercode_callbacks :: ModuleM ()
  , handlercode_emitters :: [EmitterCode]
  }

emptyHandlerCode :: HandlerCode
emptyHandlerCode = HandlerCode
  { handlercode_callbacks = return ()
  , handlercode_emitters = []
  }

insertHandlerCodeCallback :: ModuleM () -> HandlerCode -> HandlerCode
insertHandlerCodeCallback m c =
  c { handlercode_callbacks = handlercode_callbacks c >> m }

insertHandlerCodeEmitter :: EmitterCode -> HandlerCode -> HandlerCode
insertHandlerCodeEmitter e c =
  c { handlercode_emitters = e : handlercode_emitters c}

