{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Codegen.Handler
  ( generateHandlerThreadCode
  , handlerProcName
  , callbackCode
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

generateHandlerThreadCode :: (IvoryArea a, IvoryZero a)
                          => HandlerCode a
                          -> AST.Tower -> AST.Handler -> [(AST.Thread, ThreadCode)]
generateHandlerThreadCode hc twr h =
  [ (t, handlerCodeToThreadCode twr t m h hc)
  | t <- AST.handlerThreads twr h
  ]
  where
  m = maybe (error msg) id (AST.towerFindMonitorOfHandler h twr)
  msg = "generateHandlerThreadCode: broken invariant, monitor of handler "
     ++ show h ++ "must exist in " ++ show twr

emitterCode :: AST.Tower -> AST.Thread -> HandlerCode a -> ModuleDef
emitterCode twr t hc = mapM_ someemittercode_user (handlercode_emitters hc twr t)

generatedHandlerCode :: forall a
                      . (IvoryArea a, IvoryZero a)
                     => HandlerCode a
                     -> AST.Tower -> AST.Thread -> AST.Monitor -> AST.Handler
                     -> ModuleDef
generatedHandlerCode hc twr t m h =
  foldl appendgen (return ()) (handlercode_emitters hc twr t)
  >> incl runner
  where
  appendgen acc ec = acc >> someemittercode_gen ec
  runner :: Def('[ConstRef s a]:->())
  runner = proc (handlerProcName h t) $ \msg -> body $ do
    comment "init emitters"
    forM_ (handlercode_emitters hc twr t)
      (\e -> someemittercode_init e)
    comment "take monitor lock"
    call_ monitorLockProc
    comment "run callbacks"
    forM_ (AST.handler_callbacks h) (\ast -> call_ (cbproc ast) msg)
    comment "release monitor lock"
    call_ monitorUnlockProc
    comment "deliver emitters"
    forM_ (handlercode_emitters hc twr t)
      (\e -> someemittercode_deliver e)

  monitorUnlockProc :: Def('[]:->())
  monitorUnlockProc = proc (monitorUnlockProcName m) (body (return ()))
  monitorLockProc :: Def('[]:->())
  monitorLockProc = proc (monitorLockProcName m) (body (return ()))

  -- Dummy proc body, just need to call by name
  cbproc :: Unique -> Def('[ConstRef s a]:->())
  cbproc cbname = callbackProc cbname (AST.handler_name h) (const $ return ()) t

handlerProcName :: AST.Handler -> AST.Thread -> String
handlerProcName h t = "handler_run_" ++ AST.handlerName h
                     ++ "_" ++ AST.threadName t

handlerCodeToThreadCode :: (IvoryArea a, IvoryZero a)
                        => AST.Tower -> AST.Thread -> AST.Monitor -> AST.Handler
                        -> HandlerCode a -> ThreadCode
handlerCodeToThreadCode twr t m h hc = ThreadCode
  { threadcode_user = handlercode_callbacks hc t
  , threadcode_emitter = emitterCode twr t hc
  , threadcode_gen = generatedHandlerCode hc twr t m h
  }

callbackCode :: IvoryArea a
             => Unique
             -> Unique
             -> (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
             -> AST.Thread
             -> ModuleDef
callbackCode u hname f t = incl $ callbackProc u hname f t

callbackProc :: IvoryArea a
             => Unique
             -> Unique
             -> (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
             -> AST.Thread
             -> Def ('[ConstRef s a] :-> ())
callbackProc u hname f t =
  proc (callbackProcName u hname t) $ \ r -> body $ noReturn $ f r

callbackProcName :: Unique -> Unique -> AST.Thread -> String
callbackProcName callbackname handlername tast
  =  showUnique callbackname
  ++ "_"
  ++ showUnique handlername
  ++ "_"
  ++ AST.threadName tast
