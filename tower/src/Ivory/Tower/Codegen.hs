
module Ivory.Tower.Codegen
  ( generateTowerCode
  , GeneratedCode()
  , Artifact(..)
  , artifactFromString
  , putArtifact
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.Artifact
import Ivory.Tower.Types.TowerPlatform
import Ivory.Language

generateTowerCode :: GeneratedCode -> AST.Tower -> TowerPlatform
                  -> ([Module], [Artifact])
generateTowerCode gc twr p = (ms, as)
  where
  ms = generatedcode_modules gc
    ++ threadModules p gc twr
    ++ monitorModules p gc twr
    ++ systemModules p twr
  as = systemArtifacts p twr ms
