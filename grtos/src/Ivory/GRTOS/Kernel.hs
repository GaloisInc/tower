{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS.Kernel where

import Ivory.Language
import Ivory.GRTOS.Kernel.TaskControlBlock

kernel_wait :: Def('[]:->())
kernel_wait = importProc "kernel_wait" kernel_header

kernel_yield :: Def('[]:->())
kernel_yield = importProc "kernelYIELD" kernel_header

kernel_begin_atomic :: Def('[]:->())
kernel_begin_atomic = importProc "kernelDISABLE_INTERRUPTS" kernel_header

kernel_end_atomic :: Def('[]:->())
kernel_end_atomic = importProc "kernelENABLE_INTERRUPTS" kernel_header

kernel_scheduler_start :: Def('[]:->())
kernel_scheduler_start = importProc "scheduler_start" kernel_header

kernel_task_create :: Def('[ Ref s0 (Struct "task_control_block")
                           , ProcPtr ('[]:->())
                           , Ref s1 (CArray (Stored Uint32))
                           , Uint32
                           , IString
                           , Uint8
                           ] :->()) -- XXX incomplete
kernel_task_create = importProc "task_create" kernel_header

kernel_header :: String
kernel_header = "grtos_kernel.h"

kernel :: Module
kernel = package "ivory_grtos_kernel" $ do
  inclHeader kernel_header
  sourceDep  kernel_header
  sourceDep  "grtos_kernel.c"
  sourceDep  "grtos_syscalls.c"
  sourceDep  "grtos_FreeRTOSConfig.h"
  sourceDep  "grtos_FreeRTOS.h"
  sourceDep  "grtos_portable.h"
  sourceDep  "grtos_projdefs.h"
  sourceDep  "grtos_tcb_type.h"
  sourceDep  "port_GCC_ARM_CM4F/grtos_portmacro.h"
  sourceDep  "port_GCC_ARM_CM4F/grtos_port.c"


