module Ivory.Tower.Types.Dependencies where

import Data.List (nubBy)
import Data.Monoid
import Ivory.Artifact
import Ivory.Language

data Dependencies = Dependencies
  { dependencies_modules :: [Module]
  , dependencies_depends :: [Module]
  , dependencies_artifacts :: [Artifact]
  }

instance Monoid Dependencies where
  mempty = Dependencies
    { dependencies_modules = mempty
    , dependencies_depends = mempty
    , dependencies_artifacts = mempty
    }
  mappend a b = Dependencies
    { dependencies_modules = dependencies_modules a `mappend` dependencies_modules b
    , dependencies_depends = dependencies_depends a `mappend` dependencies_depends b
    , dependencies_artifacts = dependencies_artifacts a `mappend` dependencies_artifacts b
    }

dedupArtifacts :: Dependencies -> Dependencies
dedupArtifacts d = d
  { dependencies_artifacts = nubBy mightBeEqArtifact $ dependencies_artifacts d
  }
