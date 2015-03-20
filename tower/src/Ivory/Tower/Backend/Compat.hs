{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ) where

import Control.Arrow (second)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Monoid
import Ivory.Language
import Ivory.Stdlib (when)
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Backend
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.MonitorCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Unique

data CompatBackend = CompatBackend

instance TowerBackend CompatBackend where
  newtype TowerBackendCallback CompatBackend a = CompatCallback (forall s. AST.Handler -> AST.Thread -> (Def ('[ConstRef s a] :-> ()), ModuleDef))
  newtype TowerBackendEmitter CompatBackend = CompatEmitter (AST.Monitor -> AST.Tower -> AST.Thread -> SomeEmitterCode)
  data TowerBackendHandler CompatBackend a = CompatHandler AST.Handler (forall s. AST.Monitor -> AST.Tower -> AST.Thread -> (Def ('[ConstRef s a] :-> ()), ThreadCode))
  newtype TowerBackendMonitor CompatBackend = CompatMonitor (AST.Tower -> GeneratedCode)
  newtype TowerBackendOutput CompatBackend = CompatOutput GeneratedCode

  callbackImpl _ ast f = CompatCallback $ \ h -> callbackCode ast (AST.handler_name h) f

  emitterImpl _ ast handlers =
    let (e, code) = emitterCode ast $ \ mon twr thd -> [ fst $ h mon twr thd | CompatHandler _ h <- handlers ]
    in (e, CompatEmitter $ \ mon twr thd -> SomeEmitterCode $ code mon twr thd)

  handlerImpl _ ast emitters callbacks = CompatHandler ast $ \ mon twr thd -> handlerCodeToThreadCode twr thd mon ast $ hc mon
    where
    hc mon = HandlerCode
      { handlercode_callbacks = \ t -> second mconcat $ unzip [ c ast t | CompatCallback c <- callbacks ]
      , handlercode_emitters = \ twr t -> [ e mon twr t | CompatEmitter e <- emitters ]
      }

  monitorImpl _ ast handlers moddef = CompatMonitor $ \ twr -> mempty
    { generatedcode_threads = Map.fromListWith mappend
        [ (thd, snd $ h ast twr thd)
        -- handlers are reversed to match old output for convenient diffs
        | SomeHandler (CompatHandler hast h) <- reverse handlers
        , thd <- AST.handlerThreads twr hast
        ]
    , generatedcode_monitors = Map.singleton ast $ MonitorCode moddef
    }

  towerImpl _ ast monitors = CompatOutput $ mconcat [ m ast | CompatMonitor m <- monitors ]

data SomeEmitterCode = forall a . SomeEmitterCode (EmitterCode a)

someemittercode_deliver :: SomeEmitterCode -> Ivory eff ()
someemittercode_deliver (SomeEmitterCode c) = emittercode_deliver c
someemittercode_init :: SomeEmitterCode -> Ivory eff ()
someemittercode_init (SomeEmitterCode c) = emittercode_init c
someemittercode_gen :: SomeEmitterCode -> ModuleDef
someemittercode_gen (SomeEmitterCode c) = emittercode_gen c
someemittercode_user :: SomeEmitterCode -> ModuleDef
someemittercode_user (SomeEmitterCode c) = emittercode_user c

data EmitterCode (a :: Area *) = EmitterCode
  { emittercode_init :: forall eff. Ivory eff ()
  , emittercode_deliver :: forall eff. Ivory eff ()
  , emittercode_user :: ModuleDef
  , emittercode_gen :: ModuleDef
  }

-- | Emitter actions.
data EmitState =
    Init
  | Emit
  | Deliver
  | Msg Integer
  | MsgCnt
  deriving (Show, Read, Eq)

-- | Create consistent suffixes from actions.
prettyEmitSuffix :: EmitState -> String
prettyEmitSuffix e =
  case e of
    Init    -> "init"
    Emit    -> "emit"
    Deliver -> "deliver"
    Msg bnd -> "message_" ++ show bnd
    MsgCnt  -> "message_count"

emitterCode :: (IvoryArea a, IvoryZero a)
            => AST.Emitter
            -> (forall s. AST.Monitor -> AST.Tower -> AST.Thread -> [Def ('[ConstRef s a] :-> ())])
            -> (Emitter a, AST.Monitor -> AST.Tower -> AST.Thread -> EmitterCode a)
emitterCode ast sinks =
  ( Emitter $ call_ $ trampolineProc ast $ const $ return ()
  , emitterCodePerThread ast sinks
  )

emitterCodePerThread :: forall a
                      . (IvoryArea a, IvoryZero a)
                     => AST.Emitter
                     -> (forall s. AST.Monitor -> AST.Tower -> AST.Thread -> [Def ('[ConstRef s a] :-> ())])
                     -> AST.Monitor -> AST.Tower -> AST.Thread -> EmitterCode a
emitterCodePerThread ast sinks mon twr thr = EmitterCode
  { emittercode_init = call_ iproc
  , emittercode_deliver = call_ dproc
  , emittercode_user = do
      private $ incl trampoline
  , emittercode_gen = do
      mapM_ defMemArea messages
      defMemArea messageCount
      incl iproc
      incl eproc
      incl dproc
  }
  where
  max_messages = AST.emitter_bound ast - 1
  messageCount :: MemArea (Stored Uint32)
  messageCount = area (e_per_thread MsgCnt) Nothing
  messages :: [MemArea a]
  messages = [ area (e_per_thread (Msg d)) Nothing
             | d <- [0..max_messages] ]

  messageAt :: Uint32 -> Ref Global a
  messageAt idx = foldl aux dflt (zip messages [0..])
    where
    dflt = addrOf (messages !! 0) -- Should be impossible.
    aux basecase (msg, midx) =
      (fromIntegral (midx :: Integer) ==? idx) ? (addrOf msg, basecase)

  trampoline :: Def('[ConstRef s a]:->())
  trampoline = trampolineProc ast $ call_ eproc
  iproc :: Def('[]:->())
  iproc = proc (e_per_thread Init) $ body $
               store (addrOf messageCount) 0
  eproc :: Def('[ConstRef s a]:->())
  eproc = proc (e_per_thread Emit)  $ \msg -> body $ do
               mc <- deref (addrOf messageCount)
               when (mc <=? fromIntegral max_messages) $ do
                 store (addrOf messageCount) (mc + 1)
                 storedmsg <- assign (messageAt mc)
                 refCopy storedmsg msg

  dproc :: Def('[]:->())
  dproc = proc (e_per_thread Deliver) $ body $ do
            mc <- deref (addrOf messageCount)
            forM_ (zip messages [0..]) $ \(m, (index :: Integer)) ->
               when (fromIntegral index <? mc) $
                  forM_ (sinks mon twr thr) $ \ p ->
                    call_ p (constRef (addrOf m))

  e_per_thread = emitterThreadProcName thr ast

trampolineProc :: IvoryArea a
               => AST.Emitter
               -> (forall eff. ConstRef s a -> Ivory eff ())
               -> Def ('[ConstRef s a] :-> ())
trampolineProc ast f = proc (emitterProcName ast) $ \ r -> body $ f r

emitterThreadProcName :: AST.Thread -> AST.Emitter -> EmitState -> String
emitterThreadProcName thr ast suffix =
  emitterProcName ast ++ "_" ++ tn ++ "_" ++ prettyEmitSuffix suffix
  where
  tn = AST.threadName thr

data HandlerCode (a :: Area *) = HandlerCode
  { handlercode_callbacks :: forall s. AST.Thread -> ([Def ('[ConstRef s a] :-> ())], ModuleDef)
  , handlercode_emitters :: AST.Tower -> AST.Thread -> [SomeEmitterCode]
  }

emitterCodeFromHandler :: AST.Tower -> AST.Thread -> HandlerCode a -> ModuleDef
emitterCodeFromHandler twr t hc = mapM_ someemittercode_user (handlercode_emitters hc twr t)

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

handlerCodeToThreadCode :: (IvoryArea a, IvoryZero a)
                        => AST.Tower -> AST.Thread -> AST.Monitor -> AST.Handler
                        -> HandlerCode a -> (Def ('[ConstRef s a] :-> ()), ThreadCode)
handlerCodeToThreadCode twr t m h hc = (runner, ThreadCode
  { threadcode_user = snd $ handlercode_callbacks hc t
  , threadcode_emitter = emitterCodeFromHandler twr t hc
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

emitterProcName :: AST.Emitter -> String
emitterProcName e = showUnique (AST.emitter_name e)
  ++ case AST.emitter_chan e of
       AST.ChanSync c -> "_chan_" ++ show (AST.sync_chan_label c)
       _ -> error ("impossible: emitterProcName invariant broken @ " ++ show e)

callbackProcName :: Unique -> Unique -> AST.Thread -> String
callbackProcName callbackname handlername tast
  =  showUnique callbackname
  ++ "_"
  ++ showUnique handlername
  ++ "_"
  ++ AST.threadName tast

handlerProcName :: AST.Handler -> AST.Thread -> String
handlerProcName h t = "handler_run_" ++ AST.handlerName h
                     ++ "_" ++ AST.threadName t

monitorUnlockProcName :: AST.Monitor -> String
monitorUnlockProcName mon = "monitor_unlock_" ++ AST.monitorName mon

monitorLockProcName :: AST.Monitor -> String
monitorLockProcName mon = "monitor_lock_" ++ AST.monitorName mon
