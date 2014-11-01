
module Ivory.Tower.Types.Artifact
  ( Artifact(..)
  , artifactFromString
  , putArtifact
  ) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.FilePath

data Artifact =
  Artifact
    { artifact_filename :: FilePath
    , artifact_contents :: T.Text
    }

artifactFromString :: FilePath -> String -> Artifact
artifactFromString f s = Artifact
  { artifact_filename = f
  , artifact_contents = T.pack s
  }

putArtifact :: FilePath -> Artifact -> IO ()
putArtifact fp a = T.writeFile (fp </> artifact_filename a) (artifact_contents a)
