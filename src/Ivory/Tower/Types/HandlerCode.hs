
module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  , emptyHandlerCode
  , insertHandlerCode
  , generateHandlerCode
  ) where

import Ivory.Tower.ToyObjLang

-- XXX: the _moddef field really is only storing callbacks
-- for emitter code generation we'll have to expand this record
-- with emitter initialization & delivery
data HandlerCode = HandlerCode
  { handlercode_moddef :: ModuleM ()
  }

emptyHandlerCode :: HandlerCode
emptyHandlerCode = HandlerCode
  { handlercode_moddef = return ()
  }

insertHandlerCode :: ModuleM () -> HandlerCode -> HandlerCode
insertHandlerCode m c = c { handlercode_moddef = handlercode_moddef c >> m }

generateHandlerCode :: HandlerCode -> ModuleM ()
generateHandlerCode = handlercode_moddef
