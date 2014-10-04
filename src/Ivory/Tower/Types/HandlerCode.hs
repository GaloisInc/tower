
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
generateHandlerCode hc twr han = do
  t <- AST.handlerThreads twr mon han
  return (t, handlercode_moddef hc han t)
  where
  Just mon = AST.towerFindMonitorOfHandler han twr

