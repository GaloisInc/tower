{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Compile
  ( compile
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Monad.Tower
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Types.OS (OS)
import qualified Ivory.Tower.Types.OS as OS
import           Ivory.Tower.Types.Artifact
import           Ivory.Tower.Types.Signalable

compile :: forall p
         . (Signalable p)
        => Tower p ()
        -> OS
        -> (AST.System p, [Module], [Artifact])
compile twr os = (sysast, objs, systemcode_artifacts systemcode)
  where
  (sysast, systemcode) = runBase (runTower twr) os
  objs = system_mods ++ (concat taskmods) ++ (systemcode_modules systemcode)

  (taskmods, taskmdefs) = unzip (map (OS.codegen_task os sysast)
                                     (systemcode_tasks systemcode))

  system_mods = OS.codegen_sysinit os sysast systemcode allmdefs
  allmdefs = foldl (>>) (return ()) taskmdefs

