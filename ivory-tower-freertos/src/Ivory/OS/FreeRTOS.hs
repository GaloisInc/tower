
module Ivory.OS.FreeRTOS
  ( modules
  ) where

import Ivory.Language

import qualified Ivory.OS.FreeRTOS.Task      as Task
import qualified Ivory.OS.FreeRTOS.Semaphore as Semaphore
import qualified Ivory.OS.FreeRTOS.Queue     as Queue

modules :: [Module]
modules = [ Task.taskModule, Semaphore.semaphoreModule, Queue.queueModule ]

