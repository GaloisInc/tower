module Ivory.Tower.Types.TowerPlatform
  ( TowerPlatform(..)
  ) where

import Ivory.Artifact
import Ivory.Language
import Ivory.Tower.Backend
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.SignalCode

data TowerPlatform backend a = TowerPlatform
  { platformBackend :: backend
  , platformEnv     :: a
  , addModules      :: TowerBackendOutput backend -> Dependencies -> SignalCode -> [Module]
  , addArtifacts    :: TowerBackendOutput backend -> Dependencies -> SignalCode -> [Module] -> [Artifact] -> [Artifact]
  }
