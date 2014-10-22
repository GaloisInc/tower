
module Ivory.Tower.Codegen.Emitter where

import Control.Monad (forM_)

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Codegen.Handler

import Ivory.Tower.ToyObjLang

emitterCode :: Emitter a -> AST.Tower -> AST.Thread -> EmitterCode
emitterCode e twr thr = EmitterCode
  { emittercode_init = iproc
  , emittercode_emit = trampoline
  , emittercode_deliver = dproc
  , emittercode_user = do
      defProc trampoline -- XXX make sure this is private to c module.
  , emittercode_gen = do
      defProc iproc
      defProc eproc
      defProc dproc
  }
  where
  tn = AST.threadName thr
  trampoline = proc ename ["msg"] (call eproc)
  iproc = proc (e_per_thread "init") []
               (stmt ("init in thread " ++ tn ))
  eproc = proc (e_per_thread "emit") ["msg"]
               (stmt ("store messages for delivery"))
  dproc = proc (e_per_thread "deliver") [] $ do
               forM_ (AST.towerChanHandlers twr chanast) $ \(_,h) ->
                 call (proc (handlerProcName h thr) [] (return ()))
  chanast = case e of Emitter (AST.Emitter _ ast _) -> ast
  ename = emitterProcName e
  e_per_thread suffix = ename ++ "_" ++ tn ++ "_" ++ suffix
