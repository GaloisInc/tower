{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.String (fromString)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend (compileWith)

import Ivory.GRTOS.PriorityGroup
import Ivory.GRTOS.AST
import Ivory.GRTOS.Kernel
import Ivory.GRTOS.Kernel.TaskControlBlock
import Ivory.GRTOS.SearchDir

main :: IO ()
main = compileWith Nothing (Just [searchDir]) ms
  where
  ms = [pg_module pg, launch, kernel, taskControlBlockTypeModule]
  launch = package "launch" $ do
    depend taskControlBlockTypeModule
    depend (pg_module pg)
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
              (procPtr (pg_loop pg))  -- non-terminating task body
              (toCArray task_stack)   -- uint32_t *stack_start
              (256 * 4)               -- sizeof(task_stack_area)
              taskname                -- name
              1                       -- priority

  -- XXX is there anything in the ghc 7.8's typelits that can do this
  -- dispatch for us, e.g. a magic dictionary
  pg = case ((length es) `div` 32) + 1 of
        1 -> priorityGroup (Priority 1 es) (Proxy :: Proxy 1)
        2 -> priorityGroup (Priority 1 es) (Proxy :: Proxy 2)
        3 -> priorityGroup (Priority 1 es) (Proxy :: Proxy 3)
        4 -> priorityGroup (Priority 1 es) (Proxy :: Proxy 4)
        5 -> priorityGroup (Priority 1 es) (Proxy :: Proxy 5)
        6 -> priorityGroup (Priority 1 es) (Proxy :: Proxy 6)
        _ -> error "priorityGroup test: failed, no singleton for groups larger than 6"

  es = take 33 $ map mkevt [1..]
  mkevt n = ( Event n ("e" ++ (show n))
            , Callback (comment ("stub ivory code for event " ++ (show n))))

