
module Ivory.Tower.Handler
  ( emitter
  , callback
  , emit
  , Handler()
  ) where


import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Base

import Ivory.Tower.Codegen.Emitter
import Ivory.Tower.Codegen.Handler

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

emitter :: ChanInput a -> Integer -> Handler (Emitter a)
emitter (ChanInput (Chan chanast)) bound = do
  n <- fresh
  let ast = AST.emitter n chanast bound
      e = Emitter ast
  handlerPutASTEmitter ast
  handlerPutCodeEmitter $ \twr thr -> emitterCode e twr thr
  return e

callback :: ProcM () -> Handler ()
callback b = do
  u <- freshname "callback"
  handlerPutASTCallback u
  hname <- handlerName
  handlerPutCodeCallback $ \t -> do
    defProc (proc (callbackProcName u hname t)  ["msg"] b)


emit :: Emitter a -> ProcM ()
emit e = call (proc (emitterProcName e) [] (return ()))

