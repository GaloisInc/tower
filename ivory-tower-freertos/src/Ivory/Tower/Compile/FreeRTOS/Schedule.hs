{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS.Schedule where

import GHC.TypeLits
import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib (when, unless)
import Ivory.Tower.Types

import qualified Ivory.OS.FreeRTOS.Task  as Task
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

-- Schedule emitter: create the emitter macro for the channels.
mkEmitter :: forall n area eff cs s
           . (SingI n, IvoryArea area, eff `AllocsIn` cs)
          => [TaskNode]
          -> [SigNode]
          -> Ctx -- System
          -> ChannelEmitter n area -- Codegen
          -> ConstRef s area
          -> IBoolRef eff cs
mkEmitter tnodes snodes ctx emitter ref = do
    -- with all of the endpoints for chref, create an ivory
    --   monad that calls emit on each one, noting failure if it occurs
    f <- local (ival false)
    forM_ endEmitters $ \fch -> do
      s <- fch_emit fch ctx ref
      unless s (store f true)
    --   then calls notify on each of the appropriate guards
    forM_ endGuards $ \g -> guard_notify g ctx
    return f
  where
  channel = unChannelEmitter emitter
  ets = endpointNodes tnodes channel
  ess = endpointNodes snodes channel
  endGuards :: [FreeRTOSGuard]
  endGuards = map eventGuard ets
  endEmitters :: [FreeRTOSChannel area]
  endEmitters = (map (eventQueue channel (sing :: Sing n)) ets)
             ++ (map (eventQueue channel (sing :: Sing n)) ess)

mkReceiver :: forall n s eff cs area i
            . (SingI n, IvoryArea area, eff `AllocsIn` cs)
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
  fch = eventQueue (unChannelReceiver chrx) (sing :: Sing n) noderx

mkSigSchedule :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule
mkSigSchedule tnodes signodes tnode = SigSchedule
    { ssch_mkEmitter    = mkSigEmitter
    , ssch_mkReceiver   = mkSigReceiver
    , ssch_mkSigBody    = mkSigBody
    }
  where
  mkSigEmitter :: (SingI n, IvoryArea area, eff `AllocsIn` cs)
               => ChannelEmitter n area
               -> ConstRef s area
               -> IBoolRef eff cs
  mkSigEmitter emitter ref = mkEmitter tnodes signodes ISR emitter ref

  mkSigReceiver :: (SingI n, IvoryArea area, eff `AllocsIn` cs)
                => ChannelReceiver n area
                -> Ref s area
                -> Ivory eff IBool
  mkSigReceiver chrxer k = mkReceiver tnodes signodes ISR tnode chrxer k

  mkSigBody :: (forall eff cs . (eff `AllocsIn` cs) => Ivory eff ())
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
    , tsch_mkPeriodic   = mkPeriodic
    , tsch_mkEventLoop  = mkEventLoop
    , tsch_mkTaskBody   = mkTaskBody
    }
  where
  _tasks = map nodest_impl tnodes
  task  =     nodest_impl tnode

  mkEventLoop :: forall eff cs . (eff `AllocsIn` cs)
               => [Ivory eff (Ivory eff ())] -> Ivory eff ()
  mkEventLoop loopConstructors = do
    loopBodies <- sequence loopConstructors
    -- Double nested forever: hack to ensure loop never terminates
    -- even if user code improperly uses `break`
    forever $ do
      forever $ do
        guard
        sequence_ loopBodies
    where
    guard = guard_block (eventGuard tnode) period_gcd
    period_gcd = case taskst_periods task of
                    [] -> Q.maxWait
                    ps -> fromInteger $ foldl1 gcd ps

  -- scheduleTaskBody: create task def from a TaskBody
  mkTaskBody :: (forall eff cs . (eff `AllocsIn` cs ) => Ivory eff ()) -> Def('[]:->())
  mkTaskBody tb = proc ("taskbody_" ++ (nodest_name tnode)) $ body tb

mkPeriodic :: (eff `AllocsIn` cs)
              => Period -> (Uint32 -> Ivory eff ()) -> Ivory eff (Ivory eff ())
mkPeriodic (Period p) k = do
  initTime <- call Task.getTimeMillis
  lastTime <- local (ival initTime)
  return $ do
    now  <- call Task.getTimeMillis
    prev <- deref lastTime
    assume (now >=? prev) -- The abstract clock should be monotonic.
    when (now >=? (prev + fromInteger p)) $ do
      store lastTime now
      k now

mkDataReader :: (IvoryArea area) => DataReader area -> Ref s area -> Ivory eff ()
mkDataReader reader = fdp_read fdp
  where fdp = sharedState (unDataReader reader)

mkDataWriter :: (IvoryArea area) => DataWriter area -> ConstRef s area -> Ivory eff ()
mkDataWriter writer = fdp_write fdp
  where fdp = sharedState (unDataWriter writer)


