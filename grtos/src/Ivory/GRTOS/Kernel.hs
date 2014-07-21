
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS.Kernel where

import Ivory.Language

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

kernel_header :: String
kernel_header = "grtos_kernel.h"

kernel :: Module
kernel = package "ivory_grtos_kernel" $ do
  inclHeader kernel_header
  sourceDep  kernel_header


