{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS
  ( compile
  , os
  ) where

import Data.Maybe (catMaybes)

import Ivory.Language
import qualified Ivory.Language.Type as I

import qualified Ivory.Tower.Types as T
import qualified Ivory.Tower.Channel as T
import qualified Ivory.Tower.Monad as T
import qualified Ivory.Tower.Tower as T


import qualified Ivory.OS.FreeRTOS.Task  as Task
import qualified Ivory.OS.FreeRTOS.Queue as Q
import qualified Ivory.OS.FreeRTOS       as FreeRTOS

import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.ChannelQueues


compile :: T.Tower () -> (T.Assembly, [Module])
compile t = (asm, ms)
  where
  asm      = T.tower t os
  channels = T.asm_channels asm
  tasks    = T.asm_tasks asm
  deps     = T.asm_deps asm
  sched    = T.asm_schedule asm

  ms = [ twr_entry ]
    ++ FreeRTOS.modules
    ++ deps
    ++ concatMap T.taskres_extern_mods tasks


  twr_entry = package "tower" $ do
    mapM_ depend FreeRTOS.modules

    mapM_ T.cch_moddefs            channels
    mapM_ (incl . tldef_getter) tasks
    mapM_ T.taskres_moddef         tasks

    T.scheduleModuleDef sched
    incl towerentry

  towerentry :: Def ('[]:->())
  towerentry = proc "tower_entry" $ body $ do
    call_ $ T.scheduleInitializer sched
    mapM_ (call_ . T.cch_initializer) channels
    mapM_ taskCreate tasks
    retVoid

taskCreate :: T.TaskResult -> Ivory eff ()
taskCreate t = call_ Task.create pointer stacksize priority
  where
  pointer = procPtr (tldef_getter t)
  stacksize = maybe defaultstacksize fromIntegral (T.taskres_stacksize t)
  priority = defaulttaskpriority + (maybe 0 fromIntegral (T.taskres_priority t))

os :: T.OS
os = T.OS
  { T.osDataPort      = sharedState
  , T.osGetTimeMillis = call Task.getTimeMillis
  , T.osSchedule      = schedule
  , T.osCreateChannel = createChannel
  }

createChannel :: forall area . (IvoryType area)
              => T.ChannelRef area -> T.CompiledChannelName -> T.ScheduledReceiver area
createChannel ref channelname =
  T.ScheduledReceiver
    { T.sr_receiver = fch_receive fch
    , T.sr_utref    = T.unChannelRef ref
    , T.sr_compiledchannel  = cch
    }
  where
  fch = eventQueue ref channelname
  cch = T.CompiledChannel
    { T.cch_name = channelname
    , T.cch_initializer = fch_initDef fch
    , T.cch_moddefs     = fch_moduleDef fch
    , T.cch_type = show (I.ivoryType (Proxy :: Proxy area))
    }

-- XXX may turn out we dont need utchannels.
schedule :: [T.UTChannelRef] -> [T.UncompiledTask] -> T.TowerSchedule
schedule _utchannels utasks = T.TowerSchedule
    { T.scheduleEmitter     = scheduleEmitter
    , T.scheduleTaskLoop    = scheduleTaskLoop
    , T.scheduleInitializer = initDef
    , T.scheduleModuleDef   = smd
    }
  where
  -- Schedule emitter: create the emitter macro for the channels.
  scheduleEmitter :: forall area . (IvoryType area)
                  => T.ChannelRef area -> T.ChannelEmitter area
  scheduleEmitter chref = T.ChannelEmitter $ \v -> do -- v is a ConstRef s area
      -- with all of the endpoints for chref, create an ivory
      --   monad that calls emit on each one, throwing away the result (assume
      --   successful for now)
      mapM_ (\fch -> fch_emit fch v) endpointEmitters
      --   then calls notify on each of the appropriate guards
      mapM_ guard_notify endpointGuards
    where
    endpoints = endpointTasks chref
    endpointGuards :: [FreeRTOSGuard]
    endpointGuards = map taskGuardOfUnsched endpoints
    endpointEmitters :: [FreeRTOSChannel area]
    endpointEmitters = map (endpointQueue chref) endpoints

  -- Helpers for scheduleEmitter

  -- endpointTasks: find all of the uncompiled tasks which use channel
  endpointTasks :: T.ChannelRef area -> [T.UncompiledTask]
  endpointTasks r = filter hasref utasks
    where
    hasref t = elem (T.untypedChannel r) (inboundChannels t)
    inboundChannels t = map T.unLabeled (T.tasksch_receivers (T.ut_taskSch t))

  -- taskGuard: find guard for task
  taskGuard :: T.TaskSchedule -> FreeRTOSGuard
  taskGuard t = eventGuard (T.tasksch_name t)
  taskGuardOfUnsched :: T.UncompiledTask -> FreeRTOSGuard
  taskGuardOfUnsched = taskGuard . T.ut_taskSch
  -- endpointQueue: find queue corresponding to channel/endpointTask  pair
  endpointQueue :: (IvoryType area)
                => T.ChannelRef area -> T.UncompiledTask -> FreeRTOSChannel area
  endpointQueue cr t = eventQueue cr channelname
    where channelname  = T.channelNameForEndpoint (T.untypedChannel cr) (T.ut_taskSch t)

  -- scheduleTaskLoop: create task def from a TaskLoop
  scheduleTaskLoop :: T.TaskSchedule -> T.TaskLoop -> T.CompiledTaskLoop
  scheduleTaskLoop tskschedule tskloop = T.CompiledTaskLoop $
    proc ("taskbody_" ++ (T.tasksch_name tskschedule)) $ body $ do
      -- first run the user's initializer of the task. The computation
      -- returned is the EventLoop
      evtloop <- T.unTaskLoop tskloop
      -- then we need to compile the event loop into a guard and loop bodies
      let (guard, loopConstructors) = scheduleEventLoop evtloop
      loopBodies <- sequence loopConstructors
      forever $ do
        guard
        sequence_ loopBodies
    where
    scheduleEventLoop :: (eff `AllocsIn` cs)
                      => T.EventLoop eff -> ((Ivory eff ()), [Ivory eff (Ivory eff ())])
    scheduleEventLoop (T.EventLoop els) = (guard, map eventLoopBody els)
      where
      -- throw away bool result of guard_block
      guard = guard_block (taskGuard tskschedule) period_gcd >> return ()
      period_gcd = case periods of
                      [] -> Q.maxWait
                      _ -> fromInteger $ foldl1 gcd periods
      periods = catMaybes (map eventLoopPeriod els)

  allguards = map taskGuardOfUnsched utasks
  initDef = proc "freertos_towerschedule_init" $ body $ do
    -- Initialize all task guards
    mapM_ (call_ . guard_initDef) allguards
    retVoid

  smd = do
    incl initDef
    -- own all task guards
    mapM_ guard_moduleDef allguards


eventLoopPeriod :: T.EventLoopImpl eff -> Maybe Integer
eventLoopPeriod (T.EventLoopPeriod p _) = Just p
eventLoopPeriod _ = Nothing

eventLoopBody :: (eff `AllocsIn` cs)
              => T.EventLoopImpl eff -> Ivory eff (Ivory eff ())
eventLoopBody (T.EventLoopPeriod p c) = do
  initTime <- call Task.getTimeMillis
  lastTime <- local (ival initTime)
  return $ do
    now <- call Task.getTimeMillis
    last <- deref lastTime
    assume (now >=? last) -- The abstract clock should be monotonic.
    ifte (now >=? (last + (fromInteger p)))
      (store lastTime now >> c now)
      (return ())

eventLoopBody (T.EventLoopChannel sr c) = do
  v <- local izero
  return $ do
    s <- T.sr_receiver sr v
    ifte s
      (c (constRef v))
      (return ())

defaultstacksize :: Uint32
defaultstacksize = 256

-- XXX since our scheduler now depends on round robin to schedule the loop
-- bodies, better make sure that every eventloop task has the default priority
defaulttaskpriority :: Uint8
defaulttaskpriority = 1

tldef_getter :: T.TaskResult -> Def('[]:->())
tldef_getter t = maybe err id (T.taskres_tldef t)
  where err = error ("freertos compile: empty taskref_tldef should be impossible in task "
                    ++ T.taskres_name t)
