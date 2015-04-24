{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Backend.Compat
  ( CompatBackend(..)
  , TowerBackendOutput(..)
  , emitterProcName
  , callbackProcName
  , handlerProcName
  , monitorLockProcName
  , monitorUnlockProcName
  , monitorName
  ) where

import MonadLib hiding (when)

import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Monoid
import Ivory.Language
import Ivory.Stdlib (when)
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Unique

data CompatBackend = CompatBackend

instance TowerBackend CompatBackend where
  newtype TowerBackendCallback CompatBackend a = CompatCallback (forall s. AST.Handler -> AST.Thread -> (Def ('[ConstRef s a] :-> ()), ModuleDef))
  newtype TowerBackendEmitter CompatBackend = CompatEmitter (Maybe (AST.Monitor -> AST.Thread -> EmitterCode))
  data TowerBackendHandler CompatBackend a = CompatHandler AST.Handler (forall s. AST.Monitor -> AST.Thread -> (Def ('[ConstRef s a] :-> ()), ThreadCode))
  newtype TowerBackendMonitor CompatBackend = CompatMonitor (AST.Tower -> TowerBackendOutput CompatBackend)
    deriving Monoid
  data TowerBackendOutput CompatBackend = CompatOutput
    { compatoutput_threads :: Map.Map AST.Thread ThreadCode
    , compatoutput_monitors :: Map.Map AST.Monitor ModuleDef
    }

  uniqueImpl u = return (showUnique u)
  -- callbackImpl be ast f =
  --   ( be
  --   , CompatCallback $ \ h t ->
  --       let p = proc (callbackProcName ast (AST.handler_name h) t) $ \ r -> body $ noReturn $ f r
  --       in (p, incl p)
  --   )

  -- emitterImpl be _ [] = (be, Emitter $ const $ return (), CompatEmitter Nothing)
  -- emitterImpl be ast handlers =
  --   ( be
  --   , Emitter $ call_ $ trampolineProc ast $ const $ return ()
  --   , CompatEmitter $ Just $ \ mon thd -> emitterCode ast thd
  --       [ fst $ h mon thd | CompatHandler _ h <- handlers ]
  --   )

  handlerImpl be ast emitters callbacks = return $
    CompatHandler ast $ \ mon thd ->
      let ems = [ e mon thd | CompatEmitter (Just e) <- emitters ]
          (cbs, cbdefs) = unzip [ c ast thd | CompatCallback c <- callbacks ]
          runner = handlerProc cbs ems thd mon ast
      in (runner, ThreadCode
        { threadcode_user = sequence_ cbdefs
        , threadcode_emitter = mapM_ emittercode_user ems
        , threadcode_gen = mapM_ emittercode_gen ems >> private (incl runner)
        })

  monitorImpl ast handlers moddef = return $
    CompatMonitor $ \ twr -> CompatOutput
       { compatoutput_threads = Map.fromListWith mappend
           [ (thd, snd $ h ast thd)
             -- handlers are reversed to match old output for convenient diffs
           | SomeHandler (CompatHandler hast h) <- reverse handlers
           , thd <- AST.handlerThreads twr hast
           ]
       , compatoutput_monitors = Map.singleton ast moddef
       }

  -- towerImpl be ast monitors = case mconcat monitors of CompatMonitor f -> (be, f ast)

instance Monoid (TowerBackendOutput CompatBackend) where
  mempty = CompatOutput mempty mempty
  mappend a b = CompatOutput
    { compatoutput_threads = Map.unionWith mappend (compatoutput_threads a) (compatoutput_threads b)
    , compatoutput_monitors = Map.unionWith (>>) (compatoutput_monitors a) (compatoutput_monitors b)
    }

data EmitterCode = EmitterCode
  { emittercode_init :: forall eff. Ivory eff ()
  , emittercode_deliver :: forall eff. Ivory eff ()
  , emittercode_user :: ModuleDef
  , emittercode_gen :: ModuleDef
  }

emitterCode :: (IvoryArea a, IvoryZero a)
            => AST.Emitter
            -> AST.Thread
            -> (forall s. [Def ('[ConstRef s a] :-> ())])
            -> EmitterCode
emitterCode ast thr sinks = EmitterCode
  { emittercode_init = store (addrOf messageCount) 0
  , emittercode_deliver = do
      mc <- deref (addrOf messageCount)
      forM_ (zip messages [0..]) $ \ (m, index) ->
        when (fromInteger index <? mc) $
          forM_ sinks $ \ p ->
            call_ p (constRef (addrOf m))

  , emittercode_user = do
      private $ incl trampoline
  , emittercode_gen = do
      incl eproc
      private $ do
        mapM_ defMemArea messages
        defMemArea messageCount
  }
  where
  max_messages = AST.emitter_bound ast - 1
  messageCount :: MemArea (Stored Uint32)
  messageCount = area (e_per_thread "message_count") Nothing

  messages = [ area (e_per_thread ("message_" ++ show d)) Nothing
             | d <- [0..max_messages] ]

  messageAt idx = foldl aux dflt (zip messages [0..])
    where
    dflt = addrOf (messages !! 0) -- Should be impossible.
    aux basecase (msg, midx) =
      (fromInteger midx ==? idx) ? (addrOf msg, basecase)

  trampoline = trampolineProc ast $ call_ eproc

  eproc = voidProc (e_per_thread "emit")  $ \ msg -> body $ do
               mc <- deref (addrOf messageCount)
               when (mc <=? fromInteger max_messages) $ do
                 store (addrOf messageCount) (mc + 1)
                 storedmsg <- assign (messageAt mc)
                 refCopy storedmsg msg

  e_per_thread suffix =
    emitterProcName ast ++ "_" ++ AST.threadName thr ++ "_" ++ suffix

trampolineProc :: IvoryArea a
               => AST.Emitter
               -> (forall eff. ConstRef s a -> Ivory eff ())
               -> Def ('[ConstRef s a] :-> ())
trampolineProc ast f = proc (emitterProcName ast) $ \ r -> body $ f r

handlerProc :: (IvoryArea a, IvoryZero a)
            => [Def ('[ConstRef s a] :-> ())]
            -> [EmitterCode]
            -> AST.Thread -> AST.Monitor -> AST.Handler
            -> Def ('[ConstRef s a] :-> ())
handlerProc callbacks emitters t m h =
  proc (handlerProcName h t) $ \ msg -> body $ do
    comment "init emitters"
    mapM_ emittercode_init emitters
    comment "take monitor lock"
    call_ monitorLockProc
    comment "run callbacks"
    forM_ callbacks $ \ cb -> call_ cb msg
    comment "release monitor lock"
    call_ monitorUnlockProc
    comment "deliver emitters"
    mapM_ emittercode_deliver emitters
  where
  monitorUnlockProc :: Def('[]:->())
  monitorUnlockProc = proc (monitorUnlockProcName m) (body (return ()))
  monitorLockProc :: Def('[]:->())
  monitorLockProc = proc (monitorLockProcName m) (body (return ()))

emitterProcName :: AST.Emitter -> String
emitterProcName e = showUnique (AST.emitter_name e)
  ++ "_chan_" ++ show (AST.sync_chan_label $ AST.emitter_chan e)

callbackProcName :: Unique -> Unique -> AST.Thread -> String
callbackProcName callbackname handlername tast
  =  showUnique callbackname
  ++ "_"
  ++ showUnique handlername
  ++ "_"
  ++ AST.threadName tast

handlerName :: AST.Handler -> String
handlerName h = fst $ runId $ runStateT CompatBackend $ uniqueImpl (AST.handler_name h)

monitorName :: AST.Monitor -> String
monitorName h = fst $ runId $ runStateT CompatBackend $ uniqueImpl (AST.monitor_name h)

handlerProcName :: AST.Handler -> AST.Thread -> String
handlerProcName h t = "handler_run_" ++ handlerName h
                   ++ "_" ++ AST.threadName t

monitorUnlockProcName :: AST.Monitor -> String
monitorUnlockProcName mon = "monitor_unlock_" ++ monitorName mon

monitorLockProcName :: AST.Monitor -> String
monitorLockProcName mon = "monitor_lock_" ++ monitorName mon
