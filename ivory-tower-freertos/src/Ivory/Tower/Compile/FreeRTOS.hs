{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS
  ( compile
  , os
  , searchDir
  ) where

import GHC.TypeLits

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower.Types
import Ivory.Tower.Tower (assembleTower)

import qualified Ivory.OS.FreeRTOS.Task  as Task

import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.Schedule

import Ivory.Tower.Compile.FreeRTOS.SearchDir

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
  (sys_mdef, sys_initdef) = asm_system asm

  node_genchannels = getNodeCodegen tasks ++ getNodeCodegen signals

  ms = [ tower_entry ] ++  tower_tasks ++ [ tower_commprim ]
    ++ towerst_modules towerst
    ++ concatMap (taskst_extern_mods . nodest_impl . an_nodest) tasks

  tower_commprim = package "tower_commprim" $ do
    -- External C code dependencies
    mapM_ inclHeader commprim_headers
    mapM_ sourceDep  commprim_headers
    mapM_ sourceDep  commprim_sources
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
  towerentry = proc "tower_entry" $ body $ do
    call_ sys_initdef
    mapM_ (call_ . cgen_init) node_genchannels
    mapM_ (call_ . cgen_init) (towerst_dataportgen towerst)
    mapM_ taskInit   tasks
    mapM_ taskInit   signals
    mapM_ taskCreate tasks
    retVoid


getNodeCodegen :: [AssembledNode a] -> [Codegen]
getNodeCodegen as = concatMap (nodest_codegen . an_nodest) as

taskInit :: AssembledNode i -> Ivory eff ()
taskInit a = case an_init a of
  Just p -> call_ p
  Nothing -> return ()

taskCreate :: AssembledNode TaskSt -> Ivory eff ()
taskCreate a = call_ Task.create pointer stacksize priority
  where
  taskst    = nodest_impl (an_nodest a)
  pointer   = procPtr (an_entry a)
  stacksize = fromIntegral (taskst_stacksize taskst)
  priority  = defaulttaskpriority + (maybe 0 fromIntegral (taskst_priority taskst))

os :: OS
os = OS
  { os_mkDataPort     = mkDataPort
  , os_mkChannel      = mkChannel
  , os_mkPeriodic     = mkPeriodic

  , os_assembleTask   = assembleTask
  , os_assembleSignal = assembleSignal

  , os_mkSysSchedule  = mkSystemSchedule
  , os_getTimeMillis  = call Task.getTimeMillis
  }


mkDataPort :: forall (area :: Area *) . (IvoryArea area)
           => DataSource area -> Maybe (Init area) -> (Def ('[]:->()), ModuleDef)
mkDataPort source i = (fdp_initDef fdp, fdp_moduleDef fdp)
  where
  fdp :: FreeRTOSDataport area
  fdp = sharedState (unDataSource source) i

mkChannel :: forall (n :: Nat) (area :: Area *) i
           . (SingI n, IvoryArea area, IvoryZero area)
           => ChannelReceiver n area
           -> NodeSt i
           -> (Def('[]:->()), ModuleDef)
mkChannel rxer destNode = (fch_initDef fch, fch_moduleDef fch)
  where
  fch :: FreeRTOSChannel area
  fch = eventQueue (cr_chid rxer) (sing :: Sing n) destNode

mkPeriodic :: Integer -> Name -> (Period, Def('[]:->()), ModuleDef)
mkPeriodic p n = (Period tick time p, initDef, mDef)
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

defaulttaskpriority :: Uint8
defaulttaskpriority = 1

-- ivory-freertos-wrapper

commprim_headers :: [FilePath]
commprim_headers =
  [ "freertos_semaphore_wrapper.h"
  , "freertos_task_wrapper.h"
  , "freertos_atomic_wrapper.h"
  ]

commprim_sources :: [FilePath]
commprim_sources =
  [ "freertos_semaphore_wrapper.c"
  , "freertos_task_wrapper.c"
  , "freertos_atomic_wrapper.c"
  ]


