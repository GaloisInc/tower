
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
import Ivory.Language

import Ivory.Tower.Codegen.System

generateTowerCode :: GeneratedCode -> AST.Tower -> () -> ([Module], [Artifact])
generateTowerCode gc twr _ = (ms, as)
  where
  ms = generatedcode_modules gc
    ++ threadModules gc twr
    ++ monitorModules gc twr
    ++ systemModules twr
  as = systemArtifacts twr ms
