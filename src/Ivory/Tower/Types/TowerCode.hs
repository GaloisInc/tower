module Ivory.Tower.Types.TowerCode
  ( TowerCode(..)
  , emptyTowerCode
  ) where

import Ivory.Tower.ToyObjLang

data TowerCode = TowerCode
  { towercode_modules :: [Module]
  }

emptyTowerCode :: TowerCode
emptyTowerCode = TowerCode []

