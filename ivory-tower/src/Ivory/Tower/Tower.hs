{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

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
task :: Name -> Task () -> Tower ()
task name t = do
  taskSt <- runTask name t
  towerSt <- getTowerSt
  setTowerSt $ towerSt { towerst_tasksts = taskSt : (towerst_tasksts towerSt) }

-- | Instantiate a data port. Result is a matching pair of 'DataSource' and
--   'DataSink'.
dataport :: (IvoryType area) => Tower (DataSource area, DataSink area)
dataport = do
  os <- getOS
  n <- freshname
  let dp = os_mkDataPort os n -- XXX fix eventaully.
  s <- getTowerSt
  setTowerSt $ s { towerst_dataports = (data_cch dp) : (towerst_dataports s) }
  return (DataSource dp, DataSink dp)

-- | Instantiate a channel. Result is a matching pair of 'ChannelSource' and
--   'ChannelSink'.
channel :: (IvoryType area) => Tower (ChannelSource area, ChannelSink area)
channel = do
  cid <- freshChannelId
  st <- getTowerSt
  setTowerSt $ st { towerst_channels = cid : towerst_channels st }
  return (ChannelSource cid, ChannelSink cid)
  where
  freshChannelId = fresh >>= \n -> return (ChannelId n)

-- | Add an arbitrary Ivory 'Module' to Tower. The module will be present in the
--   compiled 'Assembly'. This is provided as a convenience so users do not have
--   to append to an 'Assembly' at a later stage.
addModule :: Module -> Tower ()
addModule m = do
  s <- getTowerSt
  setTowerSt $ s { towerst_modules = m : (towerst_modules s) }

-- Task functions --------------------------------------------------------------

-- | Transform a 'ChannelSink' into a 'ChannelReceiver' in the context of a
--   'Task'.
--   A human-readable name is provided to aid in debugging.
withChannelReceiver :: (IvoryType area, IvoryZero area)
      => ChannelSink area -> String -> Task (ChannelReceiver area)
withChannelReceiver chsink label = do
  let cid  = unChannelSink chsink
      rxer = ChannelReceiver cid
  -- Register the receiver into the graph context
  taskStAddReceiver cid label
  -- Generate code implementing the channel for this receiver.
  os <- getOS
  st <- getTaskSt
  case os_mkChannel os st rxer of
    (channelinit,mdef) -> do
      taskStAddChannelInit channelinit
      taskStAddModuleDef mdef
  return rxer

-- | Transform a 'ChannelSource' into a 'ChannelEmitter' in the context of a
--   'Task'.
--   Provide a human-readable name as a debugging aid.
withChannelEmitter :: (IvoryType area)
      => ChannelSource area -> String -> Task (ChannelEmitter area)
withChannelEmitter chsrc label = do
  let cid     = unChannelSource chsrc
      emitter = ChannelEmitter cid
  taskStAddEmitter cid label
  return emitter

-- | Transform a 'DataSink' into a 'DataReader' in the context of a
--   'Task'. Provide a human-readable name as a debugging aid.
withDataReader :: (IvoryType area)
               => DataSink area -> String -> Task (DataReader area)
withDataReader ds label = do
  let dp = unDataSink ds
  taskStAddDataReader (data_cch dp) label
  return (DataReader dp)

-- | Transform a 'DataSource' into a 'DataWriter' in the context of a
--   'Task '. Provide a human-readable name as a debugging aid.
withDataWriter :: (IvoryType area)
               => DataSource area -> String -> Task (DataWriter area)
withDataWriter ds label = do
  let dp = unDataSource ds
  taskStAddDataWriter (data_cch dp) label
  return (DataWriter dp)


-- | Create a 'Period' in the context of a 'Task'. Integer argument
--   declares period in milliseconds.
withPeriod :: Integer -> Task Period
withPeriod per = do
  st <- getTaskSt
  setTaskSt $ st { taskst_periods = per : (taskst_periods st)}
  return (Period per)

-- | Create an 'Ivory.Tower.Types.OSGetTimeMillis' in the context of a 'Scheduled'
--   task. We need to use monadic form because the 'OS' which implements this
--   function is not available until 'Tower' compilation time.
withGetTimeMillis :: Task OSGetTimeMillis
withGetTimeMillis = do
  os <- getOS
  return (OSGetTimeMillis (os_getTimeMillis os))

-- | Use an 'Ivory.Tower.Types.OSGetTimeMillis' implementation in an Ivory
--   monad context. We unwrap so the implementation can bind to the
--   Ivory effect scope
getTimeMillis :: OSGetTimeMillis -> Ivory eff Uint32
getTimeMillis = unOSGetTimeMillis

-- | Declare a task body for a 'Task'. The task body is an 'Ivory'
--   computation which initializes the task, and gives an 'EventLoop' as its
--   result.
taskBody :: (Schedule -> (forall eff cs . (eff `AllocsIn` cs) => Ivory eff ()))
         -> Task ()
taskBody k = do
  s <- getTaskSt
  case taskst_taskbody s of
    Nothing -> setTaskSt $ s { taskst_taskbody = Just taskbody }
    Just _ -> error "terrible thing occured"
 where
 taskbody sch = sch_mkTaskBody sch (k sch)

