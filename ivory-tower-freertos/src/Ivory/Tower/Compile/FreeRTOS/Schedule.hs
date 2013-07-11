{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Compile.FreeRTOS.Schedule where

import GHC.TypeLits
import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower.Types

import qualified Ivory.OS.FreeRTOS.Queue as Q

import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.Types

mkSystemSchedule :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule tnodes _signodes = (md, initDef)
  where
  allguards = map eventGuard tnodes
  initDef = proc "freertos_towerschedule_init" $ body $ do
    -- Initialize all task guards
    mapM_ (call_ . guard_initDef) allguards
    retVoid

  md = do
    incl initDef
    -- own all task guards
    mapM_ guard_moduleDef allguards

endpointNodes :: [NodeSt a] -> ChannelId -> [NodeSt a]
endpointNodes nodes ch = filter hasref nodes
  where hasref n = elem ch (inboundChannels n)
        inboundChannels n = map unLabeled (nodest_receivers n)

-- Schedule emitter: create the emitter macro for the channels. Returns
-- failure value.
mkEmitter :: forall n area eff cs s
           . (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
          => [TaskNode]
          -> [SigNode]
          -> Ctx -- System
          -> ChannelEmitter n area -- Codegen
          -> ConstRef s area
          -> Ivory eff IBool
mkEmitter tnodes snodes ctx emitter ref = do
    -- with all of the endpoints for chref, create an ivory
    --   monad that calls emit on each one, noting failure if it occurs
    f <- local (ival false)
    forM_ endEmitters $ \fch -> do
      s <- fch_emit fch ctx ref
      unless s (store f true)
    --   then calls notify on each of the appropriate guards
    forM_ endGuards $ \g -> guard_notify g ctx
    deref f
  where
  (endEmitters, endGuards) = mkEmitterPrims tnodes snodes emitter

mkEmitterPrims :: forall n area
               . (SingI n, IvoryArea area)
              => [TaskNode]
              -> [SigNode]
              -> ChannelEmitter n area -- Codegen
              -> ([FreeRTOSChannel area],[FreeRTOSGuard])
mkEmitterPrims tnodes snodes emitter = (chans, guards)
  where
  channel = ce_chid emitter
  ets = endpointNodes tnodes channel
  ess = endpointNodes snodes channel
  guards :: [FreeRTOSGuard]
  guards = map eventGuard ets
  chans :: [FreeRTOSChannel area]
  chans = (map (eventQueue channel (sing :: Sing n)) ets)
       ++ (map (eventQueue channel (sing :: Sing n)) ess)

mkReceiver :: forall n s eff cs area i
            . (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
           => [TaskNode]  -- All system tasknodes
           -> [SigNode]   -- All system signodes
           -> Ctx         -- receiver execution ctx
           -> NodeSt i    -- receiving node
           -> ChannelReceiver n area  -- receiving channel
           -> Ref s area
           -> Ivory eff IBool
mkReceiver _tnodes _snodes ctx noderx chrx ref =
  fch_receive fch ctx ref
  where
  fch = eventQueue (cr_chid chrx) (sing :: Sing n) noderx

mkSigSchedule :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule
mkSigSchedule tnodes signodes tnode = SigSchedule
    { ssch_mkEmitter    = mkSigEmitter
    , ssch_mkReceiver   = mkSigReceiver
    , ssch_mkSigBody    = mkSigBody
    }
  where
  mkSigEmitter :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
               => ChannelEmitter n area
               -> ConstRef s area
               -> Ivory eff IBool
  mkSigEmitter emitter ref = mkEmitter tnodes signodes ISR emitter ref

  mkSigReceiver :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
                => ChannelReceiver n area
                -> Ref s area
                -> Ivory eff IBool
  mkSigReceiver chrxer k = mkReceiver tnodes signodes ISR tnode chrxer k

  mkSigBody :: (forall eff cs . (GetAlloc eff ~ Scope cs) => Ivory eff ())
            -> Def('[]:->())
  mkSigBody b = proc name (body b)
    where
    name = case signalst_cname (nodest_impl tnode)  of
      Just n  -> n
      Nothing -> nodest_name tnode

mkTaskSchedule :: [TaskNode] -> [SigNode] -> TaskNode -> TaskSchedule
mkTaskSchedule tnodes signodes tnode = TaskSchedule
    { tsch_mkDataReader = mkDataReader
    , tsch_mkDataWriter = mkDataWriter
    , tsch_mkEmitter    = mkEmitter tnodes signodes User
    , tsch_mkReceiver   = mkReceiver tnodes signodes User tnode
    , tsch_mkEventLoop  = mkEventLoop
    , tsch_mkTaskBody   = mkTaskBody
    }
  where
  _tasks = map nodest_impl tnodes
  task  =     nodest_impl tnode

  mkEventLoop :: forall eff cs
               . ( GetAlloc eff ~ Scope cs
                 , eff ~ ClearBreak (AllowBreak eff))
               => [Ivory eff ()] -> Ivory eff ()
  mkEventLoop loopBodies = do
    forever (noBreak $ guard >> sequence_ loopBodies)
    where
    guard = guard_block (eventGuard tnode) period_gcd
    period_gcd = case taskst_periods task of
                    [] -> Q.maxWait
                    ps -> fromInteger $ foldl1 gcd ps

  -- scheduleTaskBody: create task def from a TaskBody
  mkTaskBody :: (forall eff cs
                . ( GetAlloc eff ~ Scope cs
                  , eff ~ ClearBreak (AllowBreak eff))
             => Ivory eff ())
             -> Def('[]:->())
  mkTaskBody tb = proc ("taskbody_" ++ (nodest_name tnode)) $ body tb

mkDataReader :: (IvoryArea area)
             => DataReader area -> Ref s area -> Ivory eff ()
mkDataReader reader = fdp_read fdp
  where fdp = sharedState (unDataReader reader)

mkDataWriter :: (IvoryArea area)
             => DataWriter area -> ConstRef s area -> Ivory eff ()
mkDataWriter writer = fdp_write fdp
  where fdp = sharedState (unDataWriter writer)


