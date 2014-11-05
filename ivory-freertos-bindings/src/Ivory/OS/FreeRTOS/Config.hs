
module Ivory.OS.FreeRTOS.Config
  ( Config(..)
  , configHeader
  ) where

import Ivory.Artifact
import qualified Paths_ivory_freertos_bindings as P

data Config = Config
  { cpu_clock_hz        :: Integer
  , tick_rate_hz        :: Integer
  , max_priorities      :: Integer
  , minimal_stack_size  :: Integer
  , total_heap_size     :: Integer
  }

configHeader :: Config -> Artifact
configHeader _c =
  artifactTransformErrString applyconf af
  where
  af = artifactFile "FreeRTOSConfig.h" $ fmap suffix P.getDataDir
  suffix f = f ++ "/freertos-sources/FreeRTOSConfig.h.template"
  applyconf = Right -- XXX FIXME: MAKE CONFIG A PROPER TEMPLATE AND APPLY THE CONF VARS

