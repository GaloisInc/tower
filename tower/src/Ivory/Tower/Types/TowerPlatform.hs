{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.TowerPlatform
  ( TowerPlatform(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.Artifact
import qualified Ivory.Tower.AST as AST

data TowerPlatform =
  TowerPlatform
    { platformName    :: String
    , platformSPath   :: [IO FilePath]
    , threadModules   :: GeneratedCode -> AST.Tower -> [Module]
    , monitorModules  :: GeneratedCode -> AST.Tower -> [Module]
    , systemModules   ::                  AST.Tower -> [Module]
    , systemArtifacts ::                  AST.Tower -> [Module] -> [Artifact]
    }

