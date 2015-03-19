{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Codegen.Handler
  ( handlerCodeToThreadCode
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

emitterCode :: AST.Tower -> AST.Thread -> HandlerCode a -> ModuleDef
emitterCode twr t hc = mapM_ someemittercode_user (handlercode_emitters hc twr t)

generatedHandlerCode :: forall a s
                      . (IvoryArea a, IvoryZero a)
                     => HandlerCode a
                     -> AST.Tower -> AST.Thread -> AST.Monitor -> AST.Handler
                     -> (Def ('[ConstRef s a] :-> ()), ModuleDef)
generatedHandlerCode hc twr t m h = (runner,
  foldl appendgen (return ()) (handlercode_emitters hc twr t)
  >> incl runner)
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
    forM_ (fst $ handlercode_callbacks hc t) $ \ cb -> call_ cb msg
    comment "release monitor lock"
    call_ monitorUnlockProc
    comment "deliver emitters"
    forM_ (handlercode_emitters hc twr t)
      (\e -> someemittercode_deliver e)

  monitorUnlockProc :: Def('[]:->())
  monitorUnlockProc = proc (monitorUnlockProcName m) (body (return ()))
  monitorLockProc :: Def('[]:->())
  monitorLockProc = proc (monitorLockProcName m) (body (return ()))

handlerProcName :: AST.Handler -> AST.Thread -> String
handlerProcName h t = "handler_run_" ++ AST.handlerName h
                     ++ "_" ++ AST.threadName t

handlerCodeToThreadCode :: (IvoryArea a, IvoryZero a)
                        => AST.Tower -> AST.Thread -> AST.Monitor -> AST.Handler
                        -> HandlerCode a -> (Def ('[ConstRef s a] :-> ()), ThreadCode)
handlerCodeToThreadCode twr t m h hc = (runner, ThreadCode
  { threadcode_user = snd $ handlercode_callbacks hc t
  , threadcode_emitter = emitterCode twr t hc
  , threadcode_gen = def
  })
  where
  (runner, def) = generatedHandlerCode hc twr t m h

callbackCode :: IvoryArea a
             => Unique
             -> Unique
             -> (forall s' . ConstRef s a -> Ivory (AllocEffects s') ())
             -> AST.Thread
             -> (Def ('[ConstRef s a] :-> ()), ModuleDef)
callbackCode u hname f t = (p, incl p)
  where p = proc (callbackProcName u hname t) $ \ r -> body $ noReturn $ f r

callbackProcName :: Unique -> Unique -> AST.Thread -> String
callbackProcName callbackname handlername tast
  =  showUnique callbackname
  ++ "_"
  ++ showUnique handlername
  ++ "_"
  ++ AST.threadName tast
