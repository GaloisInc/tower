{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.OS.FreeRTOS.Semaphore where

import Prelude hiding (take)
import Ivory.Language

-- Dirty tricks:
-- an xSemaphoreHandle is actually a void*, but we don't have a void*
-- in Ivory. So, we keep around a uint8_t* to instansiate it as an area
-- and then, because we must pass areas by refs, end up passing uint8_t**
-- to every freertos wrapper function that needs the xSemaphoreHandle.
-- the wrapper functions will deref the argument onceand cast the remaining
-- uint8_t* to a xSemaphoreHandle to use it.

type Semaphore = Stored (Ptr Global (Stored Uint8))
type SemaphoreHandle = Ref Global Semaphore

semaphoreModule :: Module
semaphoreModule = package "ivory_os_freertos_semaphore" $ do
  inclHeader "freertos_semaphore_wrapper.h"
  incl create
  incl take
  incl takeNonblocking
  incl give

create :: Def ('[ SemaphoreHandle ] :-> ())
create = externProc "ivory_freertos_semaphore_create"

take :: Def ('[ SemaphoreHandle, Uint32 ] :-> IBool)
take = externProc "ivory_freertos_semaphore_take"

takeBlocking :: Def ('[ SemaphoreHandle ] :-> ())
takeBlocking = externProc "ivory_freertos_semaphore_takeblocking"

takeNonblocking  :: Def ('[ SemaphoreHandle ] :-> IBool)
takeNonblocking = externProc "ivory_freertos_semaphore_takenonblocking"

give :: Def('[ SemaphoreHandle ] :-> ())
give = externProc "ivory_freertos_semaphore_give"


