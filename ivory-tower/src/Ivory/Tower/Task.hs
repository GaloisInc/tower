{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task where

import Text.Printf

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Monad
import Ivory.Tower.Node

-- Public Task Definitions -----------------------------------------------------
instance ChannelEmittable TaskSt where
  withChannelEmitter = taskChannelEmitter

taskChannelEmitter :: forall n area . (SingI n, IvoryArea area)
        => ChannelSource n area -> String -> Node TaskSt (ChannelEmitter n area)
taskChannelEmitter chsrc label = do
  nodename <- getNodeName
  unique   <- freshname -- May not be needed.
  let chid    = unChannelSource chsrc
      emitName = printf "emitFromTask_%s_chan%d%s" nodename (chan_id chid) unique
      externEmit :: Def ('[ConstRef s area] :-> IBool)
      externEmit = externProc emitName
      procEmit :: TaskSchedule -> Def ('[ConstRef s area] :-> IBool)
      procEmit schedule = proc emitName $ \ref -> body $ do
        r <- tsch_mkEmitter schedule emitter ref
        ret r
      emitter  = ChannelEmitter
        { ce_chid         = chid
        , ce_extern_emit  = call  externEmit
        , ce_extern_emit_ = call_ externEmit
        }
  taskModuleDef $ \sch -> do
    incl (procEmit sch)
  nodeStAddEmitter chid label
  return emitter


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
--   computation which initializes the task and runs an `eventLoop`.
--   The Ivory computation Should Not terminate.
taskBody :: (   TaskSchedule
             -> (forall eff cs .
                     (GetAlloc eff ~ Scope cs
                     , eff ~ ClearBreak (AllowBreak eff))
                  => Ivory eff ())
            )
         -> Task ()
taskBody k = do
  s <- getTaskSt
  case taskst_taskbody s of
    Nothing -> setTaskSt $ s { taskst_taskbody = Just taskbody }
    Just _ -> getNodeName >>= \name ->
              error ("multiple taskBody definitions in task named " ++ name)
 where
 taskbody sch = tsch_mkTaskBody sch (k sch)

taskLocal :: (IvoryArea area) => Name -> Task (Ref Global area)
taskLocal n = tlocalAux n Nothing

taskLocalInit :: (IvoryArea area) => Name -> Init area -> Task (Ref Global area)
taskLocalInit n i = tlocalAux n (Just i)

tlocalAux :: (IvoryArea area) => Name -> Maybe (Init area) -> Task (Ref Global area)
tlocalAux n i = do
  f <- freshname
  let m = area (n ++ f) i
  taskStAddModuleDef (const (defMemArea m))
  return (addrOf m)

