{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.SystemCode
  ( SystemCode(..)
  ) where

import           Ivory.Language
import           Ivory.Tower.Types.TaskCode

data SystemCode =
  SystemCode
    { systemcode_tasks :: [TaskCode]
    , systemcode_moddef :: ModuleDef
    , systemcode_comm_initializers :: forall s . Ivory (AllocEffects s) ()
    }
