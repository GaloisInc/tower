
module Ivory.OS.FreeRTOS.Tower.Codegen
  ( stm32f4FreeRTOS
  ) where

import qualified Ivory.Tower.Types.TowerPlatform as T
import Ivory.OS.FreeRTOS.Tower.Codegen.System
import Ivory.OS.FreeRTOS.SearchDir (searchDir)

stm32f4FreeRTOS :: T.TowerPlatform
stm32f4FreeRTOS = T.TowerPlatform
  { T.platformName     = "stm32f4FreeRTOS"
  , T.platformSPath    = [searchDir]
  , T.threadModules    = threadModules
  , T.monitorModules   = monitorModules
  , T.systemModules    = systemModules
  , T.systemArtifacts  = systemArtifacts
  }

