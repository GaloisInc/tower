{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Ivory.Language

emptyHandlerThreadCode :: AST.Handler -> AST.Tower
                       -> [(AST.Thread, HandlerCode a)]
emptyHandlerThreadCode handlerast towerast =
  [ (t, emptyHandlerCode)
  | t <- AST.handlerThreads towerast handlerast
  ]

generateHandlerThreadCode :: (IvoryArea a, IvoryZero a)
                          => (AST.Tower -> [(AST.Thread, HandlerCode a)])
                          -> AST.Tower -> AST.Handler -> [ThreadCode]
generateHandlerThreadCode thcs twr h =
  [ handlerCodeToThreadCode t m h hc
  | (t, hc) <- thcs twr
  ]
  where
  m = maybe (error msg) id (AST.towerFindMonitorOfHandler h twr)
  msg = "generateHandlerThreadCode: broken invariant, monitor of handler "
     ++ show h ++ "must exist in " ++ show twr

userHandlerCode :: HandlerCode a -> ModuleDef
userHandlerCode hc = handlercode_callbacks hc >>
  foldl appenduser (return ()) (handlercode_emitters hc)
  where
  appenduser acc ec = acc >> someemittercode_user ec

generatedHandlerCode :: forall a
                      . (IvoryArea a, IvoryZero a)
                     => HandlerCode a
                     -> AST.Thread -> AST.Monitor -> AST.Handler
                     -> ModuleDef
generatedHandlerCode hc t m h =
  foldl appendgen (return ()) (handlercode_emitters hc)
  >> incl runner
  where
  appendgen acc ec = acc >> someemittercode_gen ec
  runner :: Def('[ConstRef s a]:->())
  runner = proc (handlerProcName h t) $ \msg -> body $ do
    comment "init emitters"
    forM_ (handlercode_emitters hc)
      (\e -> call_ (someemittercode_init e))
    comment "take monitor lock"
    call_ monitorLockProc
    comment "run callbacks"
    forM_ (AST.handler_callbacks h) (\ast -> call_ (cbproc ast) msg)
    comment "release monitor lock"
    call_ monitorUnlockProc
    comment "deliver emitters"
    forM_ (handlercode_emitters hc)
      (\e -> call_ (someemittercode_deliver e))

  monitorUnlockProc :: Def('[]:->())
  monitorUnlockProc = proc (monitorUnlockProcName m) (body (return ()))
  monitorLockProc :: Def('[]:->())
  monitorLockProc = proc (monitorLockProcName m) (body (return ()))

  -- Dummy proc body, just need to call by name
  cbproc :: Unique -> Def('[ConstRef s a]:->())
  cbproc cbname = proc (callbackProcName cbname (AST.handler_name h) t)
                       (const (body (return ())))

handlerProcName :: AST.Handler -> AST.Thread -> String
handlerProcName h t = "handler_run_" ++ AST.handlerName h
                     ++ "_" ++ AST.threadName t

handlerCodeToThreadCode :: (IvoryArea a, IvoryZero a)
                        => AST.Thread -> AST.Monitor -> AST.Handler
                        -> HandlerCode a -> ThreadCode
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
