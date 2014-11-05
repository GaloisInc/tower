
module Ivory.OS.FreeRTOS.Config
  ( Config(..)
  , configHeader
  ) where

import Ivory.Artifact
import qualified Paths_ivory_freertos_bindings as P

import Text.StringTemplate

data Config = Config
  { cpu_clock_hz        :: Integer
  , tick_rate_hz        :: Integer
  , max_priorities      :: Integer
  , minimal_stack_size  :: Integer
  , total_heap_size     :: Integer
  }

configHeader :: Config -> Artifact
configHeader c =
  artifactTransformErrString applyconf af
  where
  af = artifactFile "FreeRTOSConfig.h" $ fmap suffix P.getDataDir
  suffix f = f ++ "/freertos-sources/FreeRTOSConfig.h.template"
  applyconf s =
    let t  = newSTMP s :: StringTemplate String
        t' = setManyAttrib attrs t
    in case checkTemplate t' of
      (Just e, _, _) -> Left (parseErr e)
      (_, Just e, _) -> Left (missingAttrErr e)
      (_, _, Just e) -> Left (missingTemplate e)
      (_, _, _) -> Right (toString t')

  attrs = [ ("cpu_clock_hz",       show (cpu_clock_hz c))
          , ("tick_rate_hz",       show (tick_rate_hz c))
          , ("max_priorities",     show (max_priorities c))
          , ("minimal_stack_size", show (minimal_stack_size c))
          , ("total_heap_size",    show (total_heap_size c))
          ]

  prefix = "Error in FreeRTOSConfig.h.template: "
  parseErr e = prefix ++ "Failed to parse: \n" ++ e
  missingAttrErr es =  prefix ++ "The following attributes are missing:\n"
                    ++ unlines es
  missingTemplate es =  prefix ++ "Failed to lookup invoked templates: \n"
                     ++ unlines es

