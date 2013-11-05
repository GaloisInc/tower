
module Ivory.Tower.Reporting.Entrypoints
  ( entrypointsDoc
  , entrypointsToFile
  ) where

import Ivory.Tower.Types

import System.IO
import Text.PrettyPrint.Leijen

-- | Write task entrypoints from a Tower 'Assembly' to a txt file
entrypointsToFile :: FilePath -> Assembly -> IO ()
entrypointsToFile f asm = withFile f WriteMode write
  where
  write h = displayIO h rendered
  w = 1000000 -- don't wrap lines
  rendered = renderPretty 1.0 w $ entrypointsDoc asm

entrypointsDoc :: Assembly -> Doc
entrypointsDoc = const empty

