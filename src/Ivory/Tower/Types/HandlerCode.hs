
module Ivory.Tower.Types.HandlerCode
  ( HandlerCode(..)
  , emptyHandlerCode
  , insertHandlerCode
  , generateHandlerCode
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.ToyObjLang

data HandlerCode = HandlerCode
  { handlercode_moddef :: AST.Handler -> AST.Thread -> ModuleM ()
  }

emptyHandlerCode :: HandlerCode
emptyHandlerCode = HandlerCode
  { handlercode_moddef = const (const (return ()))
  }

-- XXX implement message delivery on top of this - make sure we
-- have enough context to do it properly.
insertHandlerCode :: (AST.Handler -> AST.Thread -> ModuleM ())
                  -> HandlerCode -> HandlerCode
insertHandlerCode m c =
  c { handlercode_moddef = \ctx -> handlercode_moddef c ctx >> m ctx }

generateHandlerCode :: HandlerCode
                    -> AST.Tower -> AST.Handler
                    -> [(AST.Thread, ModuleM ())]
generateHandlerCode _hc _twr _han = [] -- XXX NEED REAL IMPLEMENTATION.
  -- What are the set of threads that touch this handler?


