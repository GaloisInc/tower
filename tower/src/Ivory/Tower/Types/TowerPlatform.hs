{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.TowerPlatform
  ( TowerPlatform(..)
  ) where

import Ivory.Language
import Ivory.Artifact
import Ivory.Tower.Types.GeneratedCode
import qualified Ivory.Tower.AST as AST

data TowerPlatform a =
  TowerPlatform
    { threadModules   :: GeneratedCode -> AST.Tower -> [Module]
    , monitorModules  :: GeneratedCode -> AST.Tower -> [Module]
    , systemModules   ::                  AST.Tower -> [Module]
    , systemArtifacts ::                  AST.Tower -> [Module] -> [Artifact] -> [Artifact]
    , platformEnv     :: a
    }

