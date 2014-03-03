{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Compile.FreeRTOS
  ( os
  , searchDir
  ) where

import           Ivory.Language
import           Ivory.Tower
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.Types.OS as OS
import           Ivory.Tower.Types.TaskCode
import           Ivory.Tower.Types.SystemCode

import Ivory.Tower.Compile.FreeRTOS.SearchDir (searchDir)

os :: OS.OS
os = OS.OS
  { OS.gen_channel     = gen_channel
  , OS.get_emitter     = get_emitter
  , OS.get_receiver    = get_receiver
  , OS.codegen_task    = codegen_task
  , OS.codegen_sysinit = codegen_sysinit
  }

gen_channel :: (IvoryArea area, IvoryZero area)
            => AST.System
            -> AST.Chan
            -> Proxy area
            -> (Def('[]:->()), ModuleDef)
gen_channel sys chan proxy = garbage
  where
  garbage = (proc "garbage" $ body $ return (), return ())

get_emitter :: (IvoryArea area, IvoryZero area)
            => AST.System
            -> AST.Chan
            -> ConstRef s area
            -> Ivory eff ()
get_emitter sys chan = garbage
  where
  garbage ref = return ()

get_receiver :: (IvoryArea area, IvoryZero area)
             => AST.System
             -> AST.Task
             -> AST.Chan
             -> Ref s area
             -> Ivory eff IBool
get_receiver sys task chan = garbage
  where
  garbage ref = return false

codegen_task :: AST.System
             -> AST.Task
             -> TaskCode
             -> ([Module],ModuleDef)
codegen_task sys task taskcode = garbage
  where
  garbage = ([],return ())

codegen_sysinit :: AST.System
                -> SystemCode
                -> ModuleDef
                -> [Module]
codegen_sysinit sysast syscode taskmoddefs = garbage
  where
  garbage = []

