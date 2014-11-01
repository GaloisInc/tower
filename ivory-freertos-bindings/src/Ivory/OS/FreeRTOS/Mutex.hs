{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.FreeRTOS.Mutex where

import Prelude hiding (take)
import Ivory.Language

-- Dirty tricks:
-- an xSemaphoreHandle is actually a void*, but we don't have a void*
-- in Ivory. So, we keep around a uint8_t* to instansiate it as an area
-- and then, because we must pass areas by refs, end up passing uint8_t**
-- to every freertos wrapper function that needs the xSemaphoreHandle.
-- the wrapper functions will deref the argument onceand cast the remaining
-- uint8_t* to a xSemaphoreHandle to use it.

newtype Mutex =
  Mutex (Ptr Global (Stored Uint8))
  deriving (IvoryType, IvoryVar, IvoryStore)
type MutexHandle = Ref Global (Stored Mutex)

semaphoreWrapperHeader :: String
semaphoreWrapperHeader = "freertos_semaphore_wrapper.h"

moddef :: ModuleDef
moddef = do
  inclHeader semaphoreWrapperHeader
  sourceDep semaphoreWrapperHeader
  sourceDep "freertos_semaphore_wrapper.c"

create :: Def ('[ MutexHandle ] :-> ())
create =
  importProc "ivory_freertos_semaphore_create_mutex" semaphoreWrapperHeader

take :: Def ('[ MutexHandle ] :-> ())
take = importProc "ivory_freertos_semaphore_takeblocking" semaphoreWrapperHeader

give :: Def('[ MutexHandle ] :-> ())
give = importProc "ivory_freertos_semaphore_give" semaphoreWrapperHeader

