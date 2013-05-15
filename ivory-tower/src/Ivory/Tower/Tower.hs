{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Tower where

import Ivory.Language

import Ivory.Tower.Types

import Ivory.Tower.Channel
import Ivory.Tower.Monad

-- Public Tower functions ------------------------------------------------------

-- | Tower assembler. Given a complete 'Tower' monad, apply the operating system
--   ('OS') and collect the generated components into an 'Assembly'
tower :: Tower () -> OS -> Assembly
tower t os = runBase (runTower t) os

-- | Instantiate a 'TaskConstructor' into a task. Provide a name as a
--   human-readable debugging aid.
task :: Name -> TaskConstructor -> Tower ()
task name tc = do
  ut <- runTaskConstructor name tc
  writeUncompiledComponent (UTask ut)

-- | Instantiate a data port. Result is a matching pair of 'DataSource' and
--   'DataSink'.
dataport :: (IvoryType area) => Tower (DataSource area, DataSink area)
dataport = do
  os <- getOS
  n <- freshname
  let dp = osDataPort os n
  writeUncompiledComponent (UCompiledData (data_cch dp))
  return (DataSource dp, DataSink dp)

-- | Instantiate a channel. Result is a matching pair of 'ChannelSource' and
--   'ChannelSink'.
channel :: (IvoryType area) => Tower (ChannelSource area, ChannelSink area)
channel = do
  chref <- freshChannelRef
  let lbld = Labeled (untypedChannel chref) "construction"
  writeUncompiledComponent (UChannelRef lbld)
  return (ChannelSource chref, ChannelSink chref)

-- | Add an arbitrary Ivory 'Module' to Tower. The module will be present in the
--   compiled 'Assembly'. This is provided as a convenience so users do not have
--   to append to an 'Assembly' at a later stage.
addModule :: Module -> Tower ()
addModule m = writeUncompiledComponent (UModule m)

-- Task functions --------------------------------------------------------------

-- | Transform a 'ChannelSink' into a 'ScheduledReceiver' in the context of a
--   'Task'. Receivers must be unwrapped in the 'Task' context so that fan-out
--   of a Channel (the list of its destination 'Task's) is known when
--   'ChannelSource's are unwrapped in the resulting 'Scheduled' context.
--   A human-readable name is provided to aid in debugging.
withChannelReceiver :: (IvoryType area)
      => ChannelSink area -> String -> Task (ScheduledReceiver area)
withChannelReceiver esink label = do
  let chref = unChannelSink esink
      utref = untypedChannel chref
  -- Label receiver ref. Receiver ref used for creating schedule,
  -- label is only for human-readable display of schedule.
  taskScheduleAddReceiver (Labeled utref label)
  os <- getOS
  chname <- makeChannelName chref
  -- Compile a channel using this task context to define the endpoint::
  let rxer = osCreateChannel os chref chname
      -- Store the compiled channel for build
  taskScheduleAddCompiledChannel (sr_compiledchannel rxer)
      -- and use the compiled receiver
  return rxer

-- Scheduled Task functions ----------------------------------------------------

-- | Create a 'Period' in the context of a 'Scheduled' task. Integer argument
--   declares period in milliseconds.
withPeriod :: Integer -> Scheduled Period
withPeriod per = do
  res <- getTaskResult
  setTaskResult (res { taskres_periodic = per : (taskres_periodic res)})
  return (Period per)

-- | Create an 'Ivory.Tower.Types.OSGetTimeMillis' in the context of a 'Scheduled'
--   task. We need to use monadic form because the 'OS' which implements this
--   function is not available until 'Tower' compilation time.
withGetTimeMillis :: Scheduled OSGetTimeMillis
withGetTimeMillis = do
  os <- getOS
  return (OSGetTimeMillis (osGetTimeMillis os))

-- | Use an 'Ivory.Tower.Types.OSGetTimeMillis' implementation in an Ivory
--   monad context. We unwrap so the implementation can bind to the
--   Ivory effect scope
getTimeMillis :: OSGetTimeMillis -> Ivory eff Uint32
getTimeMillis = unOSGetTimeMillis

-- | Transform a 'ChannelSource' into a 'ChannelEmitter' in the context of a
--   'Scheduled' task. Emitters must be unwrapped in the 'Scheduled' context
--   because they use the underlying 'TowerSchedule' created by the 'OS'
--   using the fan-out of Channels described in the 'Task' context.
--   Provide a human-readable name as a debugging aid.
withChannelEmitter :: (IvoryType area)
      => ChannelSource area -> String -> Scheduled (ChannelEmitter area)
withChannelEmitter e label = do
  let ch = (unChannelSource e)
  addTaggedChannel (TagChannelEmitter label (untypedChannel ch))
  sch <- withTowerSchedule
  return $ scheduleEmitter sch ch

-- | Transform a 'DataSink' into a 'DataReader' in the context of a
--   'Scheduled' task. Provide a human-readable name as a debugging aid.
withDataReader :: (IvoryType area)
               => DataSink area -> String -> Scheduled (DataReader area)
withDataReader ds label = do
  let dp = unDataSink ds
  addTaggedChannel (TagDataReader label (data_cch dp))
  return (DataReader dp)

-- | Transform a 'DataSource' into a 'DataWriter' in the context of a
--   'Scheduled' task. Provide a human-readable name as a debugging aid.
withDataWriter :: (IvoryType area)
               => DataSource area -> String -> Scheduled (DataWriter area)
withDataWriter ds label = do
  let dp = unDataSource ds
  addTaggedChannel (TagDataWriter label (data_cch dp))
  return (DataWriter dp)

-- | Declare a task body for a 'Scheduled' task. The task body is an 'Ivory'
--   computation which initializes the task, and gives an 'EventLoop' as its
--   result. The 'EventLoop' computation is compiled inside the 'Scheduled'
--   context to create the implementation of the task loop.
--   Only one 'taskBody' may be given per 'Scheduled' context.
taskBody :: (forall eff cs . (eff `AllocsIn` cs) => Ivory eff (EventLoop eff))
         -> Scheduled ()
taskBody tb = do
  res <- getTaskResult
  sch <- withTowerSchedule
  let ctl = scheduleTaskBody sch (taskres_schedule res) (TaskBody tb)
  setTaskResult $ res { taskres_tldef = Just (unCompiledTaskBody ctl) }

