
module Ivory.Tower.Handler
  ( emitter
  , callback
  , emit
  , Handler()
  ) where

import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.Chan
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Base
import Ivory.Tower.Types.Unique
import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

emitter :: ChanInput a -> Integer -> Handler (Emitter a)
emitter (ChanInput (Chan chanast)) bound = do
  n <- fresh
  let ast = AST.emitter n chanast bound
  handlerPutASTEmitter ast
  let e = Emitter ast
  handlerPutCodeEmitter $ \t ->
    let tn = AST.threadName t
        iproc = proc ((emitterProcName e) ++ "_" ++ tn ++ "_init") []
                     (stmt ("init in thread " ++ tn ))
        eproc = proc (emitterProcName e) ["msg"]
                  (stmt ("emitter for chan " ++ show chanast))
        dproc = proc ((emitterProcName e) ++ "_" ++ tn ++ "_deliver") []
                     (stmt ("XXX unimplemented delivery in thread " ++ tn))
    in EmitterCode
        { emittercode_init = iproc
        , emittercode_emit = eproc
        , emittercode_deliver = dproc
        , emittercode_moddef = do
            defProc iproc
            defProc eproc
            defProc dproc
        }
  return e

callback :: ProcM () -> Handler ()
callback b = do
  u <- freshname "callback"
  handlerPutASTCallback u
  hname <- handlerName
  handlerPutCodeCallback $ \t -> do
    defProc (proc (callbackName u hname t)  ["msg"] b)

callbackName :: Unique -> Unique -> AST.Thread -> String
callbackName callbackname handlername tast
  = showUnique callbackname
  ++ "_"
  ++ showUnique handlername
  ++ "_"
  ++ AST.threadName tast

emit :: Emitter a -> ProcM ()
emit e = stmt $ "call emitter " ++ emitterProcName e

