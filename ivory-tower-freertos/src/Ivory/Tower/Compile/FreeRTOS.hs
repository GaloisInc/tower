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

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Tower (assembleTower)

import qualified Ivory.OS.FreeRTOS.Task  as Task

import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.Schedule

import Ivory.Tower.Compile.FreeRTOS.SearchDir

compile :: Tower () -> (Assembly, [Module])
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

  gencode = getgeneratedcode tasks ++ getgeneratedcode signals

  ms = [ twr_entry ]
    ++ towerst_modules towerst
    ++ concatMap (taskst_extern_mods . nodest_impl . asmnode_nodest) tasks

  twr_entry = package "tower" $ do
    -- External C code dependencies
    mapM_ inclHeader headerdeps
    mapM_ sourceDep  headerdeps
    mapM_ sourceDep  sourcedeps
    -- Assembled code
    mapM_ (incl . asmnode_tldef) tasks
    mapM_ (incl . asmnode_tldef) signals
    mapM_ asmnode_moddef tasks
    mapM_ asmnode_moddef signals
    -- Generated code
    mapM_ ncg_mdef gencode
    -- User specified code
    towerst_moddef towerst
    -- System-wide code
    sys_mdef
    incl towerentry

  towerentry :: Def ('[]:->())
  towerentry = proc "tower_entry" $ body $ do
    call_ sys_initdef
    mapM_ (call_ . ncg_init) gencode
    mapM_ call_ $ towerst_dataportinit towerst
    mapM_ taskCreate tasks
    retVoid


getgeneratedcode :: [AssembledNode a] -> [NodeCodegen]
getgeneratedcode as = concatMap (nodest_codegen . asmnode_nodest) as

taskCreate :: AssembledNode TaskSt -> Ivory eff ()
taskCreate a = call_ Task.create pointer stacksize priority
  where
  taskst    = nodest_impl (asmnode_nodest a)
  pointer   = procPtr (asmnode_tldef a)
  stacksize = maybe defaultstacksize fromIntegral (taskst_stacksize taskst)
  priority  = defaulttaskpriority + (maybe 0 fromIntegral (taskst_priority taskst))

os :: OS
os = OS
  { os_mkDataPort     = mkDataPort
  , os_mkTaskSchedule = mkTaskSchedule
  , os_mkSysSchedule  = mkSystemSchedule
  , os_mkSigSchedule  = mkSigSchedule
  , os_mkChannel      = mkChannel
  , os_getTimeMillis  = call Task.getTimeMillis
  }

mkDataPort :: forall (area :: Area) . (IvoryArea area)
           => DataSource area -> (Def ('[]:->()), ModuleDef)
mkDataPort source = (fdp_initDef fdp, fdp_moduleDef fdp)
  where
  fdp :: FreeRTOSDataport area
  fdp = sharedState (unDataSource source)

mkChannel :: forall (area :: Area) i . (IvoryArea area, IvoryZero area)
              => ChannelReceiver area
              -> NodeSt i
              -> (Def('[]:->()), ModuleDef)
mkChannel rxer destNode = (fch_initDef fch, fch_moduleDef fch)
  where
  fch :: FreeRTOSChannel area
  fch = eventQueue (unChannelReceiver rxer) destNode

defaultstacksize :: Uint32
defaultstacksize = 256

defaulttaskpriority :: Uint8
defaulttaskpriority = 1

-- ivory-freertos-wrapper

headerdeps :: [FilePath]
headerdeps =
  [ "freertos_queue_wrapper.h"
  , "freertos_semaphore_wrapper.h"
  , "freertos_task_wrapper.h"
  ]

sourcedeps :: [FilePath]
sourcedeps =
  [ "freertos_queue_wrapper.c"
  , "freertos_semaphore_wrapper.c"
  , "freertos_task_wrapper.c"
  ]


