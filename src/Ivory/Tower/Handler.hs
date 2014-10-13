
module Ivory.Tower.Handler
  ( emitter
  , callback
  , Handler()
  ) where

import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import qualified Ivory.Tower.AST as AST

emitter :: ChanInput a -> Integer -> Handler ()
emitter (ChanInput (Chan chanast)) bound = do
  handlerPutASTEmitter (AST.Emitter chanast bound)
  -- XXX EMIT CODE

callback :: String -> Handler ()
callback name = do
  handlerPutASTCallback name
  -- XXX EMIT CODE

