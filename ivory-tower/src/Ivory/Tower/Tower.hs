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

tower :: Tower () -> OS -> Assembly
tower t os = runBase (runTower t) os

task :: Name -> TaskConstructor -> Tower ()
task name tc = do
  ut <- runTaskConstructor name tc
  writeUncompiledComponent (UTask ut)

dataport :: (IvoryType area) => Tower (DataSource area, DataSink area)
dataport = do
  os <- getOS
  n <- freshname
  let dp = osDataPort os n
  writeUncompiledComponent (UCompiledData (data_cch dp))
  return (DataSource dp, DataSink dp)

channel :: (IvoryType area) => Tower (ChannelSource area, ChannelSink area)
channel = do
  chref <- freshChannelRef
  let lbld = Labeled (untypedChannel chref) "construction"
  writeUncompiledComponent (UChannelRef lbld)
  return (ChannelSource chref, ChannelSink chref)

addModule :: Module -> Tower ()
addModule m = writeUncompiledComponent (UModule m)

-- Task functions --------------------------------------------------------------

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

withPeriod :: Integer -> Scheduled Period
withPeriod per = do
  res <- getTaskResult
  setTaskResult (res { taskres_periodic = per : (taskres_periodic res)})
  return (Period per)

withGetTimeMillis :: Scheduled OSGetTimeMillis
withGetTimeMillis = do
  os <- getOS
  return (OSGetTimeMillis (osGetTimeMillis os))

withChannelEmitter :: (IvoryType area)
      => ChannelSource area -> String -> Scheduled (ChannelEmitter area)
withChannelEmitter e label = do
  let ch = (unChannelSource e)
  addTaggedChannel (TagChannelEmitter label (untypedChannel ch))
  sch <- withTowerSchedule
  return $ scheduleEmitter sch ch


withDataReader :: (IvoryType area)
               => DataSink area -> String -> Scheduled (DataReader area)
withDataReader ds label = do
  let dp = unDataSink ds
  addTaggedChannel (TagDataReader label (data_cch dp))
  return (DataReader dp)

withDataWriter :: (IvoryType area)
               => DataSource area -> String -> Scheduled (DataWriter area)
withDataWriter ds label = do
  let dp = unDataSource ds
  addTaggedChannel (TagDataWriter label (data_cch dp))
  return (DataWriter dp)

taskLoop :: (forall eff cs . (eff `AllocsIn` cs) => Ivory eff (EventLoop eff))
         -> Scheduled ()
taskLoop tloop = do
  res <- getTaskResult
  sch <- withTowerSchedule
  let ctl = scheduleTaskLoop sch (taskres_schedule res) (TaskLoop tloop)
  setTaskResult $ res { taskres_tldef = Just (unCompiledTaskLoop ctl) }

