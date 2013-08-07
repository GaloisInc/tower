{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Compile.AADL.OS (os) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Tower.Types

os :: OS
os = OS
  { os_mkDataPort     = mkDataPort
  , os_mkChannel      = mkChannel
  , os_mkPeriodic     = mkPeriodic

  , os_assembleTask   = assembleTask
  , os_assembleSignal = assembleSignal

  , os_mkSysSchedule  = mkSystemSchedule
  , os_getTimeMillis  = call getTimeMillis
  }

assembleTask :: [TaskNode] -> [SigNode] -> TaskNode -> AssembledNode TaskSt
assembleTask tnodes snodes tnode = AssembledNode
  { an_nodest = tnode
  , an_entry = externProc (named "assembleTask_entry_unneeded_")
  , an_modules = \sysdeps -> [ taskLoopMod sysdeps, taskUserCodeMod sysdeps ]
  }
  where
  named n = (n ++ nodest_name tnode)
  taskst = nodest_impl tnode
  taskUserCodeMod sysdeps = package (named "tower_task_usercode_") $ do
    case taskst_taskinit taskst of
      Just t -> incl t
      Nothing -> return ()
    mapM_ th_moddef $ taskst_taskhandlers taskst
    taskst_moddef_user taskst
    depend (taskLoopMod sysdeps)
    sysdeps

  taskLoopMod sysdeps = package (named "tower_task_loop_") $ do
    taskst_moddef taskst schedule
    depend (taskUserCodeMod sysdeps)
  schedule = mkTaskSchedule tnodes snodes tnode

-- Task Schedule is the vehicle for generating backend specific code, but we
-- aren't generating code, so just give the trivial implementation for each of
-- these. (This implementation will be emitted into the task_loop c files, but
-- those files are not to be compiled)
mkTaskSchedule :: [TaskNode] -> [SigNode] -> TaskNode -> TaskSchedule
mkTaskSchedule _ _ _ = TaskSchedule
  { tsch_mkDataReader = \_ _ -> return ()
  , tsch_mkDataWriter = \_ _ -> return ()
  , tsch_mkEmitter    = \_ _ -> return true
  , tsch_mkReceiver   = \_ _ -> return true
  }


assembleSignal :: [TaskNode] -> [SigNode] -> SigNode -> AssembledNode SignalSt
assembleSignal tnodes snodes snode = AssembledNode
  { an_nodest = snode
  , an_entry = externProc (named "assembleSignal_entry_unneeded_")
  , an_modules = \sysdeps -> [ signalCommMod sysdeps, signalUserCodeMod sysdeps ]
  }
  where
  named n = (n ++ nodest_name snode)
  sigst = nodest_impl snode
  nodename = nodest_name snode
  schedule = mkSigSchedule tnodes snodes snode -- XXX figure this out
  procname = case signalst_cname sigst of
      Just n  -> n
      Nothing -> nodest_name snode
  entry = proc procname $ body $ do
    case signalst_body sigst of
      Just b -> b
      Nothing -> error (   "assembleSignal: no body present in signal named "
                        ++ nodename)
  signalCommMod sysdeps = package ("tower_signal_comm_" ++ nodename ) $ do
    signalst_moddef sigst schedule
    depend (signalUserCodeMod sysdeps)
    sysdeps

  signalUserCodeMod sysdeps = package ("tower_signal_usercode_" ++ nodename ) $ do
    signalst_moddef_user sigst
    incl entry
    depend (signalCommMod sysdeps)
    sysdeps

-- SigSchedule is the vehicle for generating backend specific code, but we
-- aren't generating code, so just give the trivial implementation for each of
-- these. (This implementation will be emitted into the user code c files, but
-- those files are not to be compiled)
mkSigSchedule :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule 
mkSigSchedule tnodes snodes snode = SigSchedule
  { ssch_mkEmitter  = \_ _ -> return true
  , ssch_mkReceiver = \_ _ -> return true
  }

-- This may actually end up being called.
getTimeMillis :: Def('[]:->Uint32)
getTimeMillis = importProc "tower_gettimemillis" "tower_gettimemillis.h"

-- The following functions are all empty because we will never call or
-- generate this backend-specific code.

mkSystemSchedule :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule _ _ = (return (), externProc "mkSystemSchedule_unneeded")


mkDataPort :: forall (area :: Area) . (IvoryArea area)
           => DataSource area -> (Def ('[]:->()), ModuleDef)
mkDataPort source = (externProc "mkDataPort_unneeded", return ())

mkChannel :: forall (n :: Nat) (area :: Area) i
           . (SingI n, IvoryArea area, IvoryZero area)
           => ChannelReceiver n area
           -> NodeSt i
           -> (Def('[]:->()), ModuleDef)
mkChannel rxer destNode = (externProc "mkChannel_unneeded", return ())

-- In this case we might end up needing the gettimemillis or interval number
mkPeriodic :: Integer -> Name -> (Period, Def('[]:->()), ModuleDef)
mkPeriodic p _ = (period, externProc "mkPeriodic_unneeded", return ())
  where
  period = Period
    { per_tick = call per_tick_proc
    , per_tnow = call getTimeMillis
    , per_interval = p
    }
  per_tick_proc :: Def('[]:->IBool)
  per_tick_proc = externProc "mkPeriodic_perTickProc_unneeded"

