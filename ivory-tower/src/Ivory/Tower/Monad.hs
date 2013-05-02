{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Monad where

import MonadLib

import Data.Maybe (mapMaybe)

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Channel

-- Uncompiled Tower helper functions -----------------------------------------

uModules :: [UncompiledTower] -> [Module]
uModules as = mapMaybe aux as
  where aux (UModule m) = Just m
        aux _ = Nothing

uCompiledDatas :: [UncompiledTower] -> [CompiledChannel]
uCompiledDatas as = mapMaybe aux as
  where aux (UCompiledData c) = Just c
        aux _ = Nothing

uLbldChannelRefs :: [UncompiledTower] -> [Labeled UTChannelRef]
uLbldChannelRefs as = mapMaybe aux as
  where aux (UChannelRef c) = Just c
        aux _ = Nothing

uTasks :: [UncompiledTower] -> [UncompiledTask]
uTasks as = mapMaybe aux as
  where aux (UTask t) = Just t
        aux _ = Nothing

-- Monad Runners ---------------------------------------------------------------

runBase :: Base a -> OS -> a
runBase b os = fst (runM (unBase b) os 0)

runTower :: Tower () -> Base Assembly
runTower t = do
  (_, uctowers) <- runWriterT (unTower t)
  let cdata  = uCompiledDatas uctowers
      utasks = uTasks uctowers
      mods   = uModules uctowers
      chrefs = uLbldChannelRefs uctowers
      cchannels = concatMap (tasksch_channels . ut_taskSch) utasks
      allccs = cdata ++ cchannels
  sch <- schedule (map unLabeled chrefs) utasks
  assemble sch utasks mods allccs

runUnscheduledTask :: Name -> TaskConstructor -> Base UncompiledTask
runUnscheduledTask name t = do
  -- Guarantee uniqueness of names in TaskSchedule and UncompiledTask
  n <- freshname
  let uniquename = name ++ n
  (scheduledRunner, taskSchedule) <- runStateT (emptyTaskSchedule uniquename) (unTask t)
  return $ UncompiledTask taskSchedule scheduledRunner

runTaskConstructor :: Name -> TaskConstructor -> Tower UncompiledTask
runTaskConstructor n tc = Tower $ lift $ runUnscheduledTask n tc

runScheduled :: TaskSchedule -> TowerSchedule -> Scheduled () -> Base TaskResult
runScheduled tskSch twrSch ctask = do
  (_, a) <- runStateT (emptyTaskResult tskSch)
                      (runReaderT twrSch (unScheduled ctask))
  return a

-- Assembler -------------------------------------------------------------------

schedule :: [UTChannelRef] -> [UncompiledTask] -> Base TowerSchedule
schedule crs uts = do
  os <- getOS
  return $ osSchedule os crs uts 

assemble :: TowerSchedule -> [UncompiledTask] -> [Module] -> [CompiledChannel]
         -> Base Assembly
assemble ts uts ms ccs = do
  trs <- mapM (assembleTask ts) uts
  return $ Assembly { asm_channels = ccs
                    , asm_tasks = trs
                    , asm_deps = ms
                    , asm_schedule = ts
                    }

assembleTask  :: TowerSchedule -> UncompiledTask -> Base TaskResult
assembleTask towerSchedule (UncompiledTask taskSchedule stsk) =
  runScheduled taskSchedule towerSchedule stsk

-- Transformer -----------------------------------------------------------------

-- Convenience function to make code more readable - TaskConstructor is just
-- a type alias to hide the unsightly signature
withContext :: Scheduled () -> TaskConstructor
withContext = return

-- Task Getters/Setters --------------------------------------------------------

getTaskSchedule :: Task TaskSchedule
getTaskSchedule  = Task get

setTaskSchedule :: TaskSchedule -> Task ()
setTaskSchedule s = Task $ set s

getTaskName :: Task Name
getTaskName = do
  s <- getTaskSchedule
  return (tasksch_name s) 

-- Form a name for a channel with an endpoint in this task.
makeChannelName :: ChannelRef area -> Task CompiledChannelName
makeChannelName typedchannel = do
  s <- getTaskSchedule
  return (channelNameForEndpoint (untypedChannel typedchannel) s)

taskScheduleAddReceiver :: Labeled UTChannelRef -> Task ()
taskScheduleAddReceiver r = do
  s <- getTaskSchedule
  setTaskSchedule $ s { tasksch_receivers = r : (tasksch_receivers s)}

taskScheduleAddCompiledChannel :: CompiledChannel -> Task ()
taskScheduleAddCompiledChannel c = do
  s <- getTaskSchedule
  setTaskSchedule $ s { tasksch_channels = c : (tasksch_channels s)}

-- Scheduled Getters/Setters ---------------------------------------------------

withTowerSchedule :: Scheduled TowerSchedule
withTowerSchedule  = Scheduled ask

getTaskResult :: Scheduled TaskResult
getTaskResult  = Scheduled get

setTaskResult :: TaskResult -> Scheduled ()
setTaskResult r = Scheduled $ set r

addTaggedChannel :: TaggedChannel -> Scheduled ()
addTaggedChannel tc = do
  r <- getTaskResult
  setTaskResult (r { taskres_taggedchs = tc : (taskres_taggedchs r) })

-- Tower Getters/Setters -------------------------------------------------------

freshChannelRef :: (IvoryType area) => Tower (ChannelRef area)
freshChannelRef = do
  n <- freshname
  return (ChannelRef (UTChannelRef ("channel" ++ n)))

writeUncompiledComponent :: UncompiledTower -> Tower ()
writeUncompiledComponent c = Tower $ put [c]

-- Operating System definitions ------------------------------------------------

newtype OSGetTimeMillis =
  OSGetTimeMillis { unOSGetTimeMillis :: forall eff . Ivory eff Uint32 }

getTimeMillis :: OSGetTimeMillis -> Ivory eff Uint32
getTimeMillis = unOSGetTimeMillis

