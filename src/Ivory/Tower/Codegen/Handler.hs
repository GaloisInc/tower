
module Ivory.Tower.Codegen.Handler
  ( emptyHandlerThreadCode
  , generateHandlerThreadCode
  , handlerProcName
  , callbackProcName
  ) where

import Control.Monad (forM_)

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Types.HandlerCode
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Unique

import Ivory.Tower.Codegen.Monitor

import Ivory.Tower.ToyObjLang

emptyHandlerThreadCode :: AST.Handler -> AST.Tower
                       -> [(AST.Thread, HandlerCode)]
emptyHandlerThreadCode handlerast towerast =
  [ (t, emptyHandlerCode)
  | t <- AST.handlerThreads towerast handlerast
  ]

generateHandlerThreadCode :: (AST.Tower -> [(AST.Thread, HandlerCode)])
                          -> AST.Tower -> AST.Handler -> [ThreadCode]
generateHandlerThreadCode thcs twr h =
  [ handlerCodeToThreadCode t m h hc
  | (t, hc) <- thcs twr
  ]
  where
  m = maybe (error msg) id (AST.towerFindMonitorOfHandler h twr)
  msg = "generateHandlerThreadCode: broken invariant, monitor of handler "
     ++ show h ++ "must exist in " ++ show twr

userHandlerCode :: HandlerCode -> ModuleM ()
userHandlerCode hc = handlercode_callbacks hc >>
  foldl appenduser (return ()) (handlercode_emitters hc)
  where
  appenduser acc ec = acc >> emittercode_user ec

generatedHandlerCode :: HandlerCode -> AST.Thread -> AST.Monitor -> AST.Handler
                     -> ModuleM ()
generatedHandlerCode hc t m h =
  foldl appendgen (return ()) (handlercode_emitters hc)
  >> defProc runner
  where
  appendgen acc ec = acc >> emittercode_gen ec
  runner = proc (handlerProcName h t) ["msg"] $ do
    comment "init emitters"
    forM_ (handlercode_emitters hc)
      (\e -> call (emittercode_init e))
    comment "take monitor lock"
    call (monitorLockProc m)
    comment "run callbacks"
    mapM_ (call . cbproc) (AST.handler_callbacks h)
    comment "release monitor lock"
    call (monitorUnlockProc m)
    comment "deliver emitters"
    forM_ (handlercode_emitters hc)
      (\e -> call (emittercode_deliver e))

  cbproc cbname = proc (callbackProcName cbname (AST.handler_name h) t)
                       ["msg"] (return ())

handlerProcName :: AST.Handler -> AST.Thread -> String
handlerProcName h t = "handler_run_" ++ AST.handlerName h 
                     ++ "_" ++ AST.threadName t

handlerCodeToThreadCode :: AST.Thread -> AST.Monitor -> AST.Handler
                        -> HandlerCode -> ThreadCode
handlerCodeToThreadCode t m h hc
  = insertUserThreadCode (userHandlerCode hc)
  $ insertGenThreadCode (generatedHandlerCode hc t m h)
  $ emptyThreadCode t

callbackProcName :: Unique -> Unique -> AST.Thread -> String
callbackProcName callbackname handlername tast
  =  showUnique callbackname
  ++ "_"
  ++ showUnique handlername
  ++ "_"
  ++ AST.threadName tast
