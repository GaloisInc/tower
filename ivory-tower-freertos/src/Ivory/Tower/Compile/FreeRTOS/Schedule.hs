
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS.Schedule where

import Ivory.Language
import Ivory.Stdlib (when)
import Ivory.Tower.Types

import qualified Ivory.OS.FreeRTOS.Task  as Task
import qualified Ivory.OS.FreeRTOS.Queue as Q

import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.SharedState

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

mkSigSchedule :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule
mkSigSchedule tnodes _signodes tnode = SigSchedule
    { ssch_mkEmitter    = mkSigEmitter
    , ssch_mkReceiver   = mkSigReceiver
    , ssch_mkSigBody    = mkSigBody
    }
  where
  mkSigEmitter :: (IvoryArea area, eff `AllocsIn` cs)
               => ChannelEmitter area -> ConstRef s area -> Ivory eff ()
  mkSigEmitter emitter ref = error "mkSigEmitter still undefined"
  -- XXX emitter implementation is just like task, but with a fromISR primitive.
  -- We need to factor out the endpoint / guard definitions from mkTaskSchedule
  -- and make new guard_notify_isr and fch_emit_isr primitives.
  mkSigReceiver :: (IvoryArea area, eff `AllocsIn` cs)
                => ChannelReceiver area -> Ref s area -> Ivory eff IBool
  mkSigReceiver rxer ref = error "mkSigReceiver still undefined"
  -- XXX receiver implementation is similar but does not depend on a guard. We
  -- need to add fch_receive_isr as a primitive under the hood.
  mkSigBody :: (forall eff cs . (eff `AllocsIn` cs) => Ivory eff ())
            -> Def('[]:->())
  mkSigBody b = error "mkSigBody still undefined"
  -- XXX sig body is pretty simple: we just wrap up the code into a Def with the
  -- name appropriate for the system interrupt. that information is not attached
  -- to the SigNode at this time. Do we defer dispatch and uniqueness guarantees
  -- elsewhere insted?

mkTaskSchedule :: [TaskNode] -> [SigNode] -> TaskNode -> TaskSchedule
mkTaskSchedule tnodes _signodes tnode = TaskSchedule
    { tsch_mkDataReader = mkDataReader
    , tsch_mkDataWriter = mkDataWriter
    , tsch_mkEmitter    = mkEmitter
    , tsch_mkReceiver   = mkReceiver
    , tsch_mkPeriodic   = mkPeriodic
    , tsch_mkEventLoop  = mkEventLoop
    , tsch_mkTaskBody   = mkTaskBody
    }
  where
  _tasks = map nodest_impl tnodes
  task  =     nodest_impl tnode
  -- Schedule emitter: create the emitter macro for the channels.
  mkEmitter :: forall area eff cs s . (IvoryArea area, eff `AllocsIn` cs)
                  => ChannelEmitter area -> ConstRef s area -> Ivory eff () 
  mkEmitter emitter ref = do 
      -- with all of the endpoints for chref, create an ivory
      --   monad that calls emit on each one, throwing away the result (assume
      --   successful for now)
      mapM_ (\fch -> fch_emit fch ref) endpointEmitters
      --   then calls notify on each of the appropriate guards
      mapM_ guard_notify endpointGuards
    where
    channel = unChannelEmitter emitter
    endpoints = endpointTasks channel
    endpointGuards :: [FreeRTOSGuard]
    endpointGuards = map eventGuard endpoints
    endpointEmitters :: [FreeRTOSChannel area]
    endpointEmitters = map (eventQueue channel) endpoints

  -- endpointTasks: find all of the uncompiled tasks which use channel
  endpointTasks :: ChannelId -> [TaskNode]
  endpointTasks ch = filter hasref tnodes
    where
    hasref n = elem ch (inboundChannels n)
    inboundChannels n = map unLabeled (nodest_receivers n)

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
    guard = guard_block (eventGuard tnode) period_gcd >> return ()
    period_gcd = case taskst_periods task of
                    [] -> Q.maxWait
                    ps -> fromInteger $ foldl1 gcd ps

  -- scheduleTaskBody: create task def from a TaskBody
  mkTaskBody :: (forall eff cs . (eff `AllocsIn` cs ) => Ivory eff ()) -> Def('[]:->())
  mkTaskBody tb = proc ("taskbody_" ++ (nodest_name tnode)) $ body tb

  mkReceiver :: forall eff cs area . (IvoryArea area, IvoryZero area, eff `AllocsIn` cs)
             => ChannelReceiver area
             -> (ConstRef (Stack cs) area -> Ivory eff ())
             -> Ivory eff (Ivory eff ())
  mkReceiver rxer k = return $ do
    v <- local izero
    s <- fch_receive fch v
    when s (k (constRef v))
    where
    fch = eventQueue (unChannelReceiver rxer) tnode

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


