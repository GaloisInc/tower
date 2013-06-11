
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

mkSystemSchedule :: [TaskSt] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule tasks = (md, initDef)
  where
  allguards = map eventGuard tasks
  initDef = proc "freertos_towerschedule_init" $ body $ do
    -- Initialize all task guards
    mapM_ (call_ . guard_initDef) allguards
    retVoid

  md = do
    incl initDef
    -- own all task guards
    mapM_ guard_moduleDef allguards

mkTaskSchedule :: [TaskSt] -> TaskSt -> Schedule
mkTaskSchedule tasks task = Schedule
    { sch_mkDataReader = mkDataReader
    , sch_mkDataWriter = mkDataWriter
    , sch_mkEmitter  = mkEmitter
    , sch_mkReceiver = mkReceiver
    , sch_mkPeriodic  = mkPeriodic
    , sch_mkEventLoop = mkEventLoop
    , sch_mkTaskBody  = mkTaskBody
    }
  where
  -- Schedule emitter: create the emitter macro for the channels.
  mkEmitter :: forall area eff cs s . (IvoryType area, eff `AllocsIn` cs)
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
  endpointTasks :: ChannelId -> [TaskSt]
  endpointTasks ch = filter hasref tasks
    where
    hasref t = elem ch (inboundChannels t)
    inboundChannels t = map unLabeled (taskst_receivers t)

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
    guard = guard_block (eventGuard task) period_gcd >> return ()
    period_gcd = case taskst_periods task of
                    [] -> Q.maxWait
                    ps -> fromInteger $ foldl1 gcd ps

  -- scheduleTaskBody: create task def from a TaskBody
  mkTaskBody :: (forall eff cs . (eff `AllocsIn` cs ) => Ivory eff ()) -> Def('[]:->())
  mkTaskBody tb = proc ("taskbody_" ++ (taskst_name task)) $ body tb

  mkReceiver :: forall eff cs area . (IvoryType area, IvoryZero area, eff `AllocsIn` cs)
             => ChannelReceiver area
             -> (ConstRef (Stack cs) area -> Ivory eff ())
             -> Ivory eff (Ivory eff ())
  mkReceiver rxer k = return $ do
    v <- local izero
    s <- fch_receive fch v
    when s (k (constRef v))
    where
    fch = eventQueue (unChannelReceiver rxer) task

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

mkDataReader :: (IvoryType area) => DataReader area -> Ref s area -> Ivory eff ()
mkDataReader reader = fdp_read fdp
  where fdp = sharedState (unDataReader reader)

mkDataWriter :: (IvoryType area) => DataWriter area -> ConstRef s area -> Ivory eff ()
mkDataWriter writer = fdp_write fdp
  where fdp = sharedState (unDataWriter writer)


