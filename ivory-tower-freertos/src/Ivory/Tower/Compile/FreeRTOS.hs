{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS
  ( compile
  , os
  ) where

import Data.Maybe (catMaybes)

import Ivory.Language
import qualified Ivory.Language.Type as I
import Ivory.Tower.Types
import Ivory.Tower.Channel
import Ivory.Tower.Tower


import qualified Ivory.OS.FreeRTOS.Task  as Task
import qualified Ivory.OS.FreeRTOS.Queue as Q
import qualified Ivory.OS.FreeRTOS       as FreeRTOS

import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.Schedule


compile :: Assembly -> [Module]
compile asm = ms
  where
  towerst = asm_towerst asm
  (tasks, taskentrys) = unzip $ asm_taskdefs asm
  (sys_mdef, sys_initdef) = asm_system asm

-- XXX fix channel moduledefs:
--  channels = asm_channels asm

  ms = [ twr_entry ]
    ++ FreeRTOS.modules
    ++ towerst_modules towerst
    ++ concatMap taskst_extern_mods tasks


  twr_entry = package "tower" $ do
    mapM_ depend FreeRTOS.modules
-- XXX these are taken care of for channels, but not for data?
--    mapM_ T.cch_moddefs            channels

    mapM_ incl taskentrys
    mapM_ taskst_moddef tasks

    sys_mdef
    incl towerentry

  towerentry :: Def ('[]:->())
  towerentry = proc "tower_entry" $ body $ do
    call_ sys_initdef
-- XXX
--    mapM_ (call_ . T.cch_initializer) channels
    mapM_ taskCreate $ asm_taskdefs asm
    retVoid

taskCreate :: (TaskSt, Def('[]:->())) -> Ivory eff ()
taskCreate (taskst, entry) = call_ Task.create pointer stacksize priority
  where
  pointer = procPtr entry
  stacksize = maybe defaultstacksize fromIntegral (taskst_stacksize taskst)
  priority = defaulttaskpriority + (maybe 0 fromIntegral (taskst_priority taskst))

os :: OS
os = OS
  { os_mkDataPort     = sharedState
  , os_mkTaskSchedule = scheduleTask
  , os_mkSysSchedule  = scheduleSystem
  , os_mkChannel      = mkChannel
  , os_getTimeMillis  = call Task.getTimeMillis
  }

mkChannel :: forall (area :: Area) . (IvoryType area, IvoryZero area)
              => ChannelReceiver area
              -> TaskSt
              -> (Def('[]:->()), ModuleDef)
mkChannel rxer desttask = (fch_initDef fch, fch_moduleDef fch)
  where
  fch :: FreeRTOSChannel area
  fch = eventQueue (unChannelReceiver rxer) desttask

defaultstacksize :: Uint32
defaultstacksize = 256

-- XXX since our scheduler now depends on round robin to schedule the loop
-- bodies, better make sure that every eventloop task has the default priority
defaulttaskpriority :: Uint8
defaulttaskpriority = 1

