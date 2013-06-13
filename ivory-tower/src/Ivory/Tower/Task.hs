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
taskModuleDef :: (TaskSchedule -> ModuleDef) -> Task ()
taskModuleDef = taskStAddModuleDef

-- | Specify the stack size, in bytes, of the 'Ivory.Tower.Tower.taskBody'
--   created in the 'Ivory.Tower.Types.Task' context.
withStackSize :: Integer -> Task ()
withStackSize stacksize = do
  s <- getTaskSt
  case taskst_stacksize s of
    Nothing -> setTaskSt $ s { taskst_stacksize = Just stacksize }
    Just _  -> getNodeName >>= \name ->
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
    Just _  -> getNodeName >>= \name ->
               fail ("Cannot use withPriority more than once in task named "
                     ++ name)

-- | Add an Ivory Module to the result of this Tower compilation, from the
--   Task context.
withModule :: Module -> Task ()
withModule m = do
  s <- getTaskSt
  setTaskSt $ s { taskst_extern_mods = m:(taskst_extern_mods s)}

-- | Create a 'Period' in the context of a 'Task'. Integer argument
--   declares period in milliseconds.
withPeriod :: Integer -> Task Period
withPeriod per = do
  st <- getTaskSt
  setTaskSt $ st { taskst_periods = per : (taskst_periods st)}
  return (Period per)

-- | Create an 'Ivory.Tower.Types.OSGetTimeMillis' in the context of a 'Task'.
withGetTimeMillis :: Task OSGetTimeMillis
withGetTimeMillis = do
  os <- getOS
  return (OSGetTimeMillis (os_getTimeMillis os))

-- | Declare a task body for a 'Task'. The task body is an 'Ivory'
--   computation which initializes the task, and gives an 'EventLoop' as its
--   result.
taskBody :: (TaskSchedule -> (forall eff cs . (eff `AllocsIn` cs) => Ivory eff ()))
         -> Task ()
taskBody k = do
  s <- getTaskSt
  case taskst_taskbody s of
    Nothing -> setTaskSt $ s { taskst_taskbody = Just taskbody }
    Just _ -> error "terrible thing occured"
 where
 taskbody sch = tsch_mkTaskBody sch (k sch)
