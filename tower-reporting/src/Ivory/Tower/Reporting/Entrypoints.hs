--
-- (c) Galois, 2014
-- All rights reserved.
--
-- Generate file containing the procedure names of Ivory functions used to
-- implment the Tower tasks. Used in static analysis. Entry procedures are
-- FreeRTOS specific currently.
--

module Ivory.Tower.Reporting.Entrypoints
  ( entrypointsDoc
  , entrypointsToFile
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.AST.Directory (flatten)
import Ivory.Tower.AST.Task (Task(..))
import Ivory.Tower.Compile.FreeRTOS (towerEntry, mkName)

import           System.IO
import           Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------

-- | Write task entrypoints from a Tower system to a txt file
entrypointsToFile :: FilePath -> String -> AST.System p -> IO ()
entrypointsToFile f nm sysast = withFile f WriteMode write
  where
  write h = displayIO h rendered
  w = 1000000 -- don't wrap lines
  rendered = renderPretty 1.0 w $ entrypointsDoc nm sysast

-- | Write out a .mk file with Makefile variables assigned the task entry
-- points.
entrypointsDoc :: String -> AST.System p -> Doc
entrypointsDoc nm sysast =
  let go = map ((flip mkName) towerEntry . task_name . snd)
         $ flatten
         $ AST.system_tasks sysast
  in  pts "TASKS" go
  where
  mkVar kind = text nm <> char '_'
            <> text kind <+> equals <+> backslash

  pts _var []    = empty
  pts  var nodes =
    mkVar var <$$> indent 2
      (vsep (punctuate (empty <+> backslash) $ map text nodes))

--------------------------------------------------------------------------------

