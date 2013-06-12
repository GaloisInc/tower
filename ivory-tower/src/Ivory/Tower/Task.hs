{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task where

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Monad

-- Public Task Definitions -----------------------------------------------------

-- | Track Ivory dependencies used by the 'Ivory.Tower.Tower.taskBody' created
--   in the 'Ivory.Tower.Types.Task' context.
taskModuleDef :: (Schedule -> ModuleDef) -> Task ()
taskModuleDef = taskStAddModuleDef

-- | Specify the stack size, in bytes, of the 'Ivory.Tower.Tower.taskBody'
--   created in the 'Ivory.Tower.Types.Task' context.
withStackSize :: Integer -> Task ()
withStackSize stacksize = do
  s <- getTaskSt
  case taskst_stacksize s of
    Nothing -> setTaskSt $ s { taskst_stacksize = Just stacksize }
    Just _  -> getTaskName >>= \name ->
               fail ("Cannot use withStackSize more than once in task named "
                  ++  name)

-- | Specify an OS priority level of the 'Ivory.Tower.Tower.taskBody' created in
--   the 'Ivory.Tower.Types.Task' context. Implementation at the backend
--   defined by the 'Ivory.Tower.Types.OS' implementation.
withPriority :: Integer -> Task ()
withPriority p = do
  s <- getTaskSt
  case taskst_priority s of
    Nothing -> setTaskSt $ s { taskst_priority = Just p }
    Just _  -> getTaskName >>= \name ->
               fail ("Cannot use withPriority more than once in task named "
                     ++ name)

-- | Add an Ivory Module to the result of this Tower compilation, from the
--   Task context.
withModule :: Module -> Task ()
withModule m = do
  s <- getTaskSt
  setTaskSt $ s { taskst_extern_mods = m:(taskst_extern_mods s)}

-- | Transform a 'ChannelSource' into a 'ChannelEmitter' in the context of a
--   'Task'.
--   Provide a human-readable name as a debugging aid.
withChannelEmitter :: (IvoryArea area)
      => ChannelSource area -> String -> Task (ChannelEmitter area)
withChannelEmitter chsrc label = do
  let cid     = unChannelSource chsrc
      emitter = ChannelEmitter cid
  tnode <- getTaskNode
  setTaskNode $ nodeStAddEmitter cid label tnode
  return emitter

-- | Transform a 'DataSink' into a 'DataReader' in the context of a
--   'Task'. Provide a human-readable name as a debugging aid.
withDataReader :: (IvoryArea area)
               => DataSink area -> String -> Task (DataReader area)
withDataReader ds label = do
  let dpid = unDataSink ds
  tnode <- getTaskNode
  setTaskNode $ nodeStAddDataReader dpid label tnode
  return (DataReader dpid)

-- | Transform a 'DataSource' into a 'DataWriter' in the context of a
--   'Task '. Provide a human-readable name as a debugging aid.
withDataWriter :: (IvoryArea area)
               => DataSource area -> String -> Task (DataWriter area)
withDataWriter ds label = do
  let dpid = unDataSource ds
  tnode <- getTaskNode
  setTaskNode $ nodeStAddDataWriter dpid label tnode
  return (DataWriter dpid)

-- | Transform a 'ChannelSink' into a 'ChannelReceiver' in the context of a
--   'Task'.
--   A human-readable name is provided to aid in debugging.
withChannelReceiver :: (IvoryArea area, IvoryZero area)
      => ChannelSink area -> String -> Task (ChannelReceiver area)
withChannelReceiver chsink label = do
  let cid  = unChannelSink chsink
      rxer = toReceiver chsink
  -- Register the receiver into the graph context
  tnode <- getTaskNode
  setTaskNode $ nodeStAddReceiver cid label tnode
  -- Generate code implementing the channel for this receiver.
  codegenChannelReceiver rxer
  return rxer
  where
  toReceiver :: ChannelSink area -> ChannelReceiver area
  toReceiver sink = ChannelReceiver $ unChannelSink sink
  codegenChannelReceiver :: (IvoryArea area, IvoryZero area)
                         => ChannelReceiver area -> Task ()
  codegenChannelReceiver rxer = do
    os <- getOS
    thisnode <- getTaskNode
    let (channelinit, mdef) = os_mkChannel os rxer thisnode
    taskStAddChannelInit channelinit
    taskStAddModuleDef (\_ -> mdef)


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
