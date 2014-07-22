{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS where

import Data.String (fromString)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend (compileWith)

import Ivory.GRTOS.PriorityGroup
import Ivory.GRTOS.AST
import Ivory.GRTOS.Kernel
import Ivory.GRTOS.Kernel.TaskControlBlock
import Ivory.GRTOS.SearchDir

test :: IO ()
test = compileWith Nothing (Just [searchDir])  [m, launch, kernel, taskControlBlockTypeModule]
  where
  launch = package "launch" $ do
    depend taskControlBlockTypeModule
    depend m
    defMemArea task_tcb_area
    defMemArea task_stack_area
    incl launch_proc

  task_tcb_area :: MemArea (Struct "task_control_block")
  task_tcb_area = area "sometask_tcb" Nothing
  task_tcb = addrOf task_tcb_area

  task_stack_area :: MemArea (Array 256 (Stored Uint32))
  task_stack_area = area "sometask_stack" Nothing
  task_stack = addrOf task_stack_area

  taskname :: IString
  taskname = fromString "sometask"

  launch_proc  :: Def('[]:->())
  launch_proc = proc "launch_proc" $ body $ do
    call_ kernel_task_create
              task_tcb                -- globally allocated control block
              (procPtr loop)          -- non-terminating task body
              (toCArray task_stack)   -- uint32_t *stack_start
              (256 * 4)               -- sizeof(task_stack_area)
              taskname                -- name
              1                       -- priority

  code pc = (package "pkg" $ pg_moduledef pc, pg_loop pc)

  -- XXX is there anything in the ghc 7.8's typelits that can do this
  -- dispatch for us, e.g. a magic dictionary
  (m, loop) = case ((length es) `div` 32) + 1 of
        1 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 1)
        2 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 2)
        3 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 3)
        4 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 4)
        5 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 5)
        6 -> code (priorityGroup (Priority 1 es) :: PriorityGroup 6)
        _ -> error "priorityGroup test: failed, no singleton for groups larger than 6"

  es = take 33 $ map mkevt [1..]
  mkevt n = ( Event n ("e" ++ (show n))
            , Callback (comment ("stub ivory code for event " ++ (show n))))

