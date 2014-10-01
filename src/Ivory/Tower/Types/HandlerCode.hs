
module Ivory.Tower.Types.HandlerCode
  ( HandlerContext
  , HandlerCode(..)
  , emptyHandlerCode
  , insertHandlerCode
  , generateHandlerCode
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.ToyObjLang

-- XXX STRING IS A PLACEHOLDER
type HandlerContext = String

data HandlerCode = HandlerCode
  { handlercode_moddef :: HandlerContext -> ModuleM ()
  }

emptyHandlerCode :: HandlerCode
emptyHandlerCode = HandlerCode
  { handlercode_moddef = const (return ())
  }

insertHandlerCode :: (HandlerContext -> ModuleM ())
                  -> HandlerCode -> HandlerCode
insertHandlerCode m c =
  c { handlercode_moddef = \ctx -> handlercode_moddef c ctx >> m ctx }

generateHandlerCode :: HandlerCode
                    -> AST.Tower -> AST.Monitor -> AST.Handler
                    -> [Module]
generateHandlerCode _hc _twr _mon _han = [] -- XXX NEED REAL IMPLEMENTATION.
  -- What are the set of HandlerContexts - derived from AST
  -- Then, generate a Module for each context
  --    with a name given by that
  --    the moddef applied to that context


