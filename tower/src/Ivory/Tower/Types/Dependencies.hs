module Ivory.Tower.Types.Dependencies where

import Data.List (nub, nubBy)
import Data.Monoid
import Ivory.Artifact
import Ivory.Language

data Dependencies = Dependencies
  { dependencies_modules :: [Module]
  , dependencies_depends :: [Module]
  , dependencies_artifacts :: [Located Artifact]
  }

instance Monoid Dependencies where
  mempty = Dependencies
    { dependencies_modules = mempty
    , dependencies_depends = mempty
    , dependencies_artifacts = mempty
    }
  mappend a b = Dependencies
    { dependencies_modules = nub $
          dependencies_modules a `mappend` dependencies_modules b
    , dependencies_depends = nub $
          dependencies_depends a `mappend` dependencies_depends b
    , dependencies_artifacts = nubBy mightBeEqLocatedArtifact $
          dependencies_artifacts a `mappend` dependencies_artifacts b
    }

