
module Ivory.Tower.Compile
  ( compile
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Types.OS (OS)
import qualified Ivory.Tower.Types.OS as OS

compile :: Tower p () -> OS -> (AST.System, [Module])
compile twr os = (sysast, objs)
  where
  (sysast, systemcode) = runBase (runTower twr) os
  objs = system_mods ++ (concat taskmods)

  (taskmods, taskmdefs) = unzip (map task_cgen (systemcode_tasks systemcode))

  task_cgen (taskast, taskcode) = OS.codegen_task os sysast taskast taskcode

  system_mods = OS.codegen_sysinit os sysast systemcode allmdefs
  allmdefs = foldl (>>) (return ()) taskmdefs

