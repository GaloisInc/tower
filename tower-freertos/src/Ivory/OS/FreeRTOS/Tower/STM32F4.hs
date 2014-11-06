
module Ivory.OS.FreeRTOS.Tower.STM32F4 where

import Ivory.Language
import Ivory.Artifact
import Ivory.Tower
import Ivory.HW
import qualified Ivory.Tower.AST as AST

import qualified Ivory.OS.FreeRTOS as FreeRTOS
import qualified Ivory.Tower.Types.TowerPlatform as T

import Ivory.OS.FreeRTOS.Tower.System
import qualified Ivory.OS.FreeRTOS.Tower.STM32F4.Build as STM32F4

stm32f4FreeRTOS :: T.TowerPlatform
stm32f4FreeRTOS = T.TowerPlatform
  { T.platformName     = "stm32f4FreeRTOS"
  , T.threadModules    = threadModules
  , T.monitorModules   = monitorModules
  , T.systemModules    = systemModules
  , T.systemArtifacts  = stm32f4Artifacts
  }

stm32f4Artifacts :: AST.Tower -> [Module] -> [Artifact]
stm32f4Artifacts ast ms = (systemArtifacts ast ms) ++ as
  where
  as = [ STM32F4.makefile objs ] ++ STM32F4.artifacts
    ++ FreeRTOS.kernel config ++ FreeRTOS.wrapper
    ++ hw_artifacts

  objs = FreeRTOS.objects ++ [ moduleName m ++ ".o" | m <- ms ]
  config = FreeRTOS.defaultConfig
    { FreeRTOS.max_priorities = fromIntegral (length (AST.towerThreads ast)) + 1
    }
