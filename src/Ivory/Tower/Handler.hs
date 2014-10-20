
module Ivory.Tower.Handler
  ( emitter
  , callback
  , emit
  , Handler()
  ) where

import Control.Monad (forM_)

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
      ename = emitterProcName e
      e_per_thread tn suffix = ename ++ "_" ++ tn ++ "_" ++ suffix
  handlerPutCodeEmitter $ \twr thr ->
    let tn = AST.threadName thr
        iproc = proc (e_per_thread tn "init") []
                     (stmt ("init in thread " ++ tn ))
        trampoline = proc ename ["msg"]
                  (stmt ("call " ++ (e_per_thread tn "emit")))
        eproc' = proc (e_per_thread tn "emit") ["msg"]
                  (stmt ("store messages for delivery"))
        dproc = proc (e_per_thread tn "deliver") [] $ do
                     stmt ("XXX unimplemented delivery in thread " ++ tn)
                     forM_ (AST.towerChanHandlers twr chanast) $ \(m,h) ->
                       stmt ( "deliver to: "
                            ++ (showUnique (AST.monitor_name m)) ++ " "
                            ++ (showUnique (AST.handler_name h)))
    in EmitterCode
        { emittercode_init = iproc
        , emittercode_emit = trampoline
        , emittercode_deliver = dproc
        , emittercode_user = do
            defProc trampoline -- XXX make sure this is private to c module.
        , emittercode_gen = do
            defProc iproc
            defProc eproc'
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

