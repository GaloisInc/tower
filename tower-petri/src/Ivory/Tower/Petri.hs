module Ivory.Tower.Petri
  ( module Ivory.Tower.Petri.Petri
  , module Ivory.Tower.Petri.PetriRaw
  , module Ivory.Tower.Petri.PetriLockCoarsening
  , module Ivory.Tower.Petri.Dot
  , petriTower
  ) where

import Ivory.Tower.Petri.Petri
import Ivory.Tower.Petri.PetriRaw
import Ivory.Tower.Petri.PetriLockCoarsening
import Ivory.Tower.Petri.Dot

import Ivory.Tower.AST
import Ivory.Tower.Types.Opts

petriTower :: Tower -> PetriNet
petriTower ast =
  if (LockCoarsening OptVoid `elemOpt` (tower_transformers ast))
    then 
      petriTowerLockCoarsening ast
    else
      petriTowerRaw ast