
module Ivory.Tower.Compile.FreeRTOS
  ( os
  , compile
  , searchDir
  ) where

import           Ivory.Language
import           Ivory.Tower
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.OS
import           Ivory.Tower.Types.Assembly

import Ivory.Tower.Compile.FreeRTOS.SearchDir

os :: OS
os = OS
  { gen_channel = \sys chan proxy -> (proc "garbage" $ body $ return (), return ())
  , get_emitter = \sys chan ref -> return ()
  , get_receiver = \sys task chan ref -> return false
  }

compile :: Tower () -> (Assembly, [Module])
compile _ = error "freertos.compile undefined"

