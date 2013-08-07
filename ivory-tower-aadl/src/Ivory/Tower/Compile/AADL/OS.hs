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
  , an_modules = \sysdeps -> [ taskUserCodeMod sysdeps ]
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
    inclHeader (named "tower_task_loop_")
    sysdeps


assembleSignal :: [TaskNode] -> [SigNode] -> SigNode -> AssembledNode SignalSt
assembleSignal tnodes snodes snode = AssembledNode
  { an_nodest = snode
  , an_entry = externProc (named "assembleSignal_entry_unneeded_")
  , an_modules = \sysdeps -> [ signalMod sysdeps ]
  }
  where
  named n = (n ++ nodest_name snode)
  sigst = nodest_impl snode
  nodename = nodest_name snode
  schedule = mkSigSchedule tnodes snodes snode -- XXX figure this out
  mkSigSchedule = undefined
  procname = case signalst_cname sigst of
      Just n  -> n
      Nothing -> nodest_name snode
  entry = proc procname $ body $ do
    case signalst_body sigst of
      Just b -> b
      Nothing -> error (   "assembleSignal: no body present in signal named "
                        ++ nodename)
  signalMod sysdeps = package ("tower_signal_" ++ nodename ) $ do
    signalst_moddef sigst schedule
    incl entry
    sysdeps


mkSystemSchedule :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule = undefined

getTimeMillis :: Def('[]:->Uint32)
getTimeMillis = externProc "tower_gettimemillis"

mkDataPort :: forall (area :: Area) . (IvoryArea area)
           => DataSource area -> (Def ('[]:->()), ModuleDef)
mkDataPort source = (externProc "mkDataPort_unneeded", return ())

mkChannel :: forall (n :: Nat) (area :: Area) i
           . (SingI n, IvoryArea area, IvoryZero area)
           => ChannelReceiver n area
           -> NodeSt i
           -> (Def('[]:->()), ModuleDef)
mkChannel rxer destNode = (externProc "mkChannel_unneeded", return ())

mkPeriodic :: Integer -> Name -> (Period, Def('[]:->()), ModuleDef)
mkPeriodic p n = undefined -- (Period tick time p, initDef, mDef)
{-

  where
  unique i = i ++ n
  lastTimeArea = area (unique "periodicLastTime") Nothing
  lastTime = addrOf lastTimeArea
  mDef = do
    incl initDef
    incl tickDef
    private $ defMemArea lastTimeArea
  initDef = proc (unique "initPeriodic") $ body $ do
    initTime <- call Task.getTimeMillis
    store lastTime initTime
  tickDef :: Def ('[]:->IBool)
  tickDef = proc (unique "tickPeriodic") $ body $ do
    now  <- call Task.getTimeMillis
    prev <- deref lastTime
    assume (now >=? prev) -- The abstract clock should be monotonic.
    ticked <- assign (now >=? (prev + fromInteger p))
    when ticked $
      store lastTime now
    ret ticked
  tick = call tickDef
  time = call Task.getTimeMillis
-}
