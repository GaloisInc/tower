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


time_mod :: Module
time_mod = package "tower_freertos_time" $ do
  incl time_proc

time_proc :: Def('[]:->ITime)
time_proc = proc "getTimeMicros" $ body $ ret 0 -- XXX

codegen_task :: AST.System
             -> AST.Task
             -> TaskCode
             -> ([Module],ModuleDef)
codegen_task sys task taskcode = ([loop_mod, user_mod], deps)
  where
  deps = do
    depend user_mod
    depend loop_mod

  next_due_area = area (named "time_next_due") Nothing
  next_due_ref = addrOf next_due_area

  loop_proc :: Def('[]:->())
  loop_proc = proc (named "tower_task_loop") $ body $ noReturn $ do
    inittime <- call time_proc
    store next_due_ref inittime
    taskcode_init taskcode
    forever $ noBreak $ do
      -- XXX delay until next due
      taskcode_timer taskcode next_due_ref
      taskcode_eventrxer taskcode
      taskcode_eventloop taskcode


  loop_mod = package (named "tower_task_loop") $ do
    depend user_mod
    depend time_mod
    defMemArea next_due_area
    taskcode_commprim taskcode
    incl loop_proc

  user_mod = package (named "tower_task_usercode") $ do
    depend loop_mod
    depend time_mod
    taskcode_usercode taskcode

  named n = n ++ "_" -- XXX

codegen_sysinit :: AST.System
                -> SystemCode
                -> ModuleDef
                -> [Module]
codegen_sysinit sysast syscode taskmoddefs = [time_mod, sys_mod]
  where
  sys_mod = package "tower_system" $ do
    taskmoddefs
    incl init_proc
  init_proc :: Def('[]:->())
  init_proc = proc "tower_system_init" $ body $ do
    noReturn $ systemcode_comm_initializers syscode
    -- XXX launch tasks

