
module Ivory.OS.FreeRTOS.Tower.Codegen
  ( stm32f4FreeRTOS
  ) where

import qualified Ivory.Tower.Types.TowerPlatform as T
import Ivory.OS.FreeRTOS.Tower.Codegen.System

stm32f4FreeRTOS :: T.TowerPlatform
stm32f4FreeRTOS = T.TowerPlatform
  { T.platformName     = "stm32f4FreeRTOS"
  , T.threadModules    = threadModules
  , T.monitorModules   = monitorModules
  , T.systemModules    = systemModules
  , T.systemArtifacts  = systemArtifacts
  }

