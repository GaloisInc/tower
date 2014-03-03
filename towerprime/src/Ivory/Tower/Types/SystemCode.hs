{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.SystemCode
  ( SystemCode(..)
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.TaskCode

data SystemCode =
  SystemCode
    { systemcode_tasks :: [(AST.Task,TaskCode)]
    , systemcode_moddef :: ModuleDef
    , systemcode_comm_initializers :: forall s . Ivory (AllocEffects s) ()
    }
