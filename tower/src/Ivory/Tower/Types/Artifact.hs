
module Ivory.Tower.Types.Artifact
  ( Artifact(..)
  ) where

data Artifact =
  Artifact
    { artifact_filepath :: FilePath
    , artifact_contents :: String
    }

