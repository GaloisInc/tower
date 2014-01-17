{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Compile.EChronos.Schedule where

import Prelude hiding (rem)
import GHC.TypeLits
import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower.Types

import qualified Ivory.OS.EChronos.Task  as Task

import Ivory.Tower.Compile.EChronos.ChannelQueues
import Ivory.Tower.Compile.EChronos.SharedState
import Ivory.Tower.Compile.EChronos.Types

mkSystemSchedule :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule tnodes _signodes = (md, initDef)
  where
  allguards = map eventGuard tnodes
  initDef = proc "echronos_towerschedule_init" $ body $ do
    -- Initialize all task guards
    mapM_ (call_ . guard_initDef) allguards
    retVoid

  md = do
    incl initDef
    -- own all task guards
    mapM_ guard_moduleDef allguards

endpointNodes :: [NodeSt a] -> ChannelId -> [NodeSt a]
endpointNodes nodes ch = filter hasref nodes
  where hasref n = elem ch (inboundChannels n)
        inboundChannels n = map unLabeled (nodest_receivers n)

-- Schedule emitter: create the emitter macro for the channels. Returns
-- failure value.
mkEmitter :: forall n area eff cs s
           . (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
          => [TaskNode]
          -> [SigNode]
          -> Ctx -- System
          -> ChannelEmitter n area -- Codegen
          -> ConstRef s area
          -> Ivory eff IBool
mkEmitter tnodes snodes ctx emitter ref = do
    -- with all of the endpoints for chref, create an ivory
    --   monad that calls emit on each one, noting failure if it occurs
    f <- local (ival false)
    forM_ endEmitters $ \fch -> do
      s <- fch_emit fch ref
      unless s (store f true)
    --   then calls notify on each of the appropriate guards
    forM_ endGuards $ \g -> guard_notify g ctx
    deref f
  where
  (endEmitters, endGuards) = mkEmitterPrims tnodes snodes emitter

mkEmitterPrims :: forall n area
               . (SingI n, IvoryArea area)
              => [TaskNode]
              -> [SigNode]
              -> ChannelEmitter n area -- Codegen
              -> ([EChronosChannel area],[EChronosGuard])
mkEmitterPrims tnodes snodes emitter = (chans, guards)
  where
  channel = ce_chid emitter
  ets = endpointNodes tnodes channel
  ess = endpointNodes snodes channel
  guards :: [EChronosGuard]
  guards = map eventGuard ets
  chans :: [EChronosChannel area]
  chans = (map (eventQueue channel (sing :: Sing n)) ets)
       ++ (map (eventQueue channel (sing :: Sing n)) ess)

mkReceiver :: forall n s eff cs area i
            . (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
           => [TaskNode]  -- All system tasknodes
           -> [SigNode]   -- All system signodes
           -> Ctx         -- receiver execution ctx
           -> NodeSt i    -- receiving node
           -> ChannelReceiver n area  -- receiving channel
           -> Ref s area
           -> Ivory eff IBool
mkReceiver _tnodes _snodes _ctx noderx chrx ref =
  fch_receive fch ref
  where
  fch = eventQueue (cr_chid chrx) (sing :: Sing n) noderx

mkSigSchedule :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule
mkSigSchedule tnodes signodes tnode = SigSchedule
    { ssch_mkEmitter    = mkSigEmitter
    , ssch_mkReceiver   = mkSigReceiver
    }
  where
  mkSigEmitter :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
               => ChannelEmitter n area
               -> ConstRef s area
               -> Ivory eff IBool
  mkSigEmitter emitter ref = mkEmitter tnodes signodes ISR emitter ref

  mkSigReceiver :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
                => ChannelReceiver n area
                -> Ref s area
                -> Ivory eff IBool
  mkSigReceiver chrxer k = mkReceiver tnodes signodes ISR tnode chrxer k

mkTaskSchedule :: [TaskNode] -> [SigNode] -> TaskNode -> TaskSchedule
mkTaskSchedule tnodes signodes tnode = TaskSchedule
    { tsch_mkDataReader = mkDataReader
    , tsch_mkDataWriter = mkDataWriter
    , tsch_mkEmitter    = mkEmitter tnodes signodes User
    , tsch_mkReceiver   = mkReceiver tnodes signodes User tnode
    }

mkDataReader :: (IvoryArea area)
             => DataSink area -> Ref s area -> Ivory eff ()
mkDataReader dsnk = fdp_read fdp
  where
  -- INVARIANT: the fdp_read method should be independent of the Init val
  -- given to sharedState.
  fdp = sharedState (unDataSink dsnk) Nothing

mkDataWriter :: (IvoryArea area)
             => DataSource area -> ConstRef s area -> Ivory eff ()
mkDataWriter dsrc = fdp_write fdp
  where
  -- INVARIANT: the fdp_write method should be independent of the Init val
  -- given to sharedState.
  fdp = sharedState (unDataSource dsrc) Nothing

-- Assemble:

assembleTask :: [TaskNode] -> [SigNode] -> TaskNode -> AssembledNode TaskSt
assembleTask tnodes snodes tnode = AssembledNode
  { an_nodest  = tnode
  , an_init    = nodest_nodeinit tnode
  , an_entry   = entry
  , an_modules = \sysdeps -> [ taskLoopMod sysdeps, taskUserCodeMod sysdeps ]
  }
  where
  schedule = mkTaskSchedule tnodes snodes tnode
  taskst = nodest_impl tnode
  taskLoopMod sysdeps = package (taskst_pkgname_loop tnode) $ do
    incl entry
    taskst_moddef taskst schedule
    depend (taskUserCodeMod sysdeps)
    private (defMemArea overrun_area)
    sysdeps

  overrun_area = area ("period_overrun_" ++ (taskst_pkgname_loop tnode))
    (Just (ival 0))

  taskUserCodeMod sysdeps = package (taskst_pkgname_user tnode) $ do
    case nodest_nodeinit tnode of
      Just t -> incl t
      Nothing -> return ()
    taskst_moddef_user taskst
    depend (taskLoopMod sysdeps)
    sysdeps

  entry = proc ((taskst_pkgname_loop tnode) ++ "_proc") $ body $ noReturn $ do
    -- Initialize a variable for when the task finishes computation.
    timeExitGuard <- local izero
    timeNextDue <- local izero
    updateTime timeExitGuard
    forever $ noBreak $ do
      -- Get the current time.
      timeEnterGuard <- call Task.getTimeMillis
      tExitG <- deref timeExitGuard
      dt <- assign (timeEnterGuard - tExitG)
      assert (dt >=? 0)

      tnd <- deref timeNextDue
      rem <- timeRemaining tnd timeEnterGuard
      guard_block (eventGuard tnode) rem

      -- Update our finish time variable.
      updateTime timeExitGuard

      store timeNextDue uint32_max
      -- Pull events off the channels/timers. Timers will update the timeNextDue
      -- if they are due before the value there.
      mapM_ (\a -> act_code a timeNextDue) (taskst_evt_rxers taskst)
      -- Run event handlers. These shouldn't need access to timeNextDue
      garbageTime <- local izero
      mapM_ (\a -> act_code a garbageTime) (taskst_evt_handlers taskst)

    where
    uint32_max :: Uint32
    uint32_max = maxBound
    updateTime = resultInto (call Task.getTimeMillis)
    timeRemaining :: Uint32 -> Uint32 -> Ivory eff Uint32
    timeRemaining nextdue now = do
      (nextdue' :: Sint64)  <- assign (safeCast nextdue)
      (now'     :: Sint64)  <- assign (safeCast now)
      remaining             <- assign (nextdue' - now')
      when (remaining <? 0) $ store (addrOf overrun_area) now
      return ((remaining >? 0) ? (castWith 0 remaining, 0))


assembleSignal :: [TaskNode] -> [SigNode] -> SigNode -> AssembledNode SignalSt
assembleSignal tnodes snodes snode = AssembledNode
  { an_nodest  = snode
  , an_init    = nodest_nodeinit snode
  , an_entry   = entry
  , an_modules = \sysdeps -> [ signalCommMod sysdeps, signalUserCodeMod sysdeps ]
  }
  where
  sigst = nodest_impl snode
  schedule = mkSigSchedule tnodes snodes snode
  procname = case signalst_cname sigst of
      Just n  -> n
      Nothing -> nodest_name snode
  entry = proc procname $ body $ do
    case signalst_body sigst of
      Just b -> b
      Nothing -> error (   "assembleSignal: no body present in signal named "
                        ++ (nodest_name snode))
  signalCommMod sysdeps = package (sigst_pkgname_comm snode) $ do
    signalst_moddef sigst schedule
    depend (signalUserCodeMod sysdeps)
    sysdeps

  signalUserCodeMod sysdeps = package (sigst_pkgname_user snode) $ do
    case nodest_nodeinit snode of
      Just t -> incl t
      Nothing -> return ()
    signalst_moddef_user sigst
    incl entry
    depend (signalCommMod sysdeps)
    sysdeps


