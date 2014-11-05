module Ivory.OS.FreeRTOS.Artifacts
  ( kernel
  , wrappers
  , Config(..)
  ) where

import qualified Paths_ivory_freertos_bindings as P
import Ivory.Artifact
import Ivory.OS.FreeRTOS.Config

kernel :: Config -> [Artifact]
kernel conf = configHeader conf : kas
  where
  kas = map (artifactCabalFile P.getDataDir) kernelfiles

wrappers :: [Artifact]
wrappers = map (artifactCabalFile P.getDataDir) wrapperfiles

wrapperfiles :: [FilePath]
wrapperfiles =
  [ "ivory-freertos-wrapper/freertos_atomic_wrapper.h"
  , "ivory-freertos-wrapper/freertos_atomic_wrapper.c"
  , "ivory-freertos-wrapper/freertos_semaphore_wrapper.h"
  , "ivory-freertos-wrapper/freertos_semaphore_wrapper.c"
  , "ivory-freertos-wrapper/freertos_task_wrapper.h"
  , "ivory-freertos-wrapper/freertos_task_wrapper.c"
  , "ivory-freertos-wrapper/freertos_time_wrapper.h"
  , "ivory-freertos-wrapper/freertos_time_wrapper.c"
  ]

kernelfiles :: [FilePath]
kernelfiles =
  [ "freertos-sources/croutine.c"
  , "freertos-sources/list.c"
  , "freertos-sources/queue.c"
  , "freertos-sources/tasks.c"
  , "freertos-sources/timers.c"
  , "freertos-sources/include/FreeRTOS.h"
  , "freertos-sources/include/StackMacros.h"
  , "freertos-sources/include/croutine.h"
  , "freertos-sources/include/list.h"
  , "freertos-sources/include/mpu_wrappers.h"
  , "freertos-sources/include/portable.h"
  , "freertos-sources/include/projdefs.h"
  , "freertos-sources/include/queue.h"
  , "freertos-sources/include/semphr.h"
  , "freertos-sources/include/task.h"
  , "freertos-sources/include/timers.h"
  , "freertos-sources/portable/GCC/ARM_CM4F/port.c"
  , "freertos-sources/portable/GCC/ARM_CM4F/portmacro.h"
  , "freertos-sources/portable/MemMang/heap_1.c"
  , "freertos-sources/portable/MemMang/heap_2.c"
  , "freertos-sources/portable/MemMang/heap_3.c"
  , "freertos-sources/portable/MemMang/heap_4.c"
  ]
