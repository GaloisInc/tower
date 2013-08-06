{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.AADL
  ( compile
  , os
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Tower (assembleTower)

compile :: Tower p () -> (Assembly, [Module])
compile t = (asm, ms)
  where
  asm = assembleTower t os
  ms = buildModules asm

buildModules :: Assembly -> [Module]
buildModules asm = ms
  where
  towerst = asm_towerst asm
  tasks   = asm_tasks   asm
  signals = asm_sigs    asm
  (sys_mdef, _) = asm_system asm

  node_genchannels = getNodeCodegen tasks ++ getNodeCodegen signals

  ms = [ tower_entry ] ++  tower_tasks ++ [ tower_commprim ]
    ++ towerst_modules towerst
    ++ concatMap (taskst_extern_mods . nodest_impl . an_nodest) tasks

  tower_commprim = package "tower_commprim" $ do
    -- Generated code: channels, dataports
    mapM_ cgen_mdef node_genchannels
    mapM_ cgen_mdef (towerst_dataportgen towerst)
    -- System code
    sys_mdef
    -- Dependencies
    mapM_ depend (towerst_depends towerst)

  systemDeps :: ModuleDef
  systemDeps = do
    depend tower_commprim
    mapM_ depend (towerst_depends towerst)

  nodeModules :: AssembledNode a -> [Module]
  nodeModules n = an_modules n systemDeps

  tower_tasks :: [Module]
  tower_tasks = concatMap nodeModules tasks ++ concatMap nodeModules signals

  tower_entry = package "tower" $ do
    -- System-wide code
    incl towerentry
    -- Dependencies
    mapM_ depend tower_tasks
    depend tower_commprim

  towerentry :: Def ('[]:->())
  towerentry = externProc "tower_entry" 


getNodeCodegen :: [AssembledNode a] -> [Codegen]
getNodeCodegen as = concatMap (nodest_codegen . an_nodest) as

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
assembleTask = undefined

assembleSignal :: [TaskNode] -> [SigNode] -> SigNode -> AssembledNode SignalSt
assembleSignal = undefined

mkSystemSchedule :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule = undefined

getTimeMillis :: Def('[]:->Uint32)
getTimeMillis = externProc "tower_gettimemillis"

mkDataPort :: forall (area :: Area) . (IvoryArea area)
           => DataSource area -> (Def ('[]:->()), ModuleDef)
mkDataPort source = undefined -- (fdp_initDef fdp, fdp_moduleDef fdp)
  -- where
  -- fdp :: FreeRTOSDataport area
  -- fdp = sharedState (unDataSource source)

mkChannel :: forall (n :: Nat) (area :: Area) i
           . (SingI n, IvoryArea area, IvoryZero area)
           => ChannelReceiver n area
           -> NodeSt i
           -> (Def('[]:->()), ModuleDef)
mkChannel rxer destNode = undefined --  (fch_initDef fch, fch_moduleDef fch)
  -- where
  -- fch :: FreeRTOSChannel area
  -- fch = eventQueue (cr_chid rxer) (sing :: Sing n) destNode

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

defaultstacksize :: Uint32
defaultstacksize = 256

defaulttaskpriority :: Uint8
defaulttaskpriority = 1
-}

