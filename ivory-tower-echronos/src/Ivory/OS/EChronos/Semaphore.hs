{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.OS.EChronos.Semaphore where

import Prelude hiding (take)
import Ivory.Language

-- Dirty tricks:
-- an xSemaphoreHandle is actually a void*, but we don't have a void*
-- in Ivory. So, we keep around a uint8_t* to instansiate it as an area
-- and then, because we must pass areas by refs, end up passing uint8_t**
-- to every echronos wrapper function that needs the xSemaphoreHandle.
-- the wrapper functions will deref the argument onceand cast the remaining
-- uint8_t* to a xSemaphoreHandle to use it.

type Semaphore = Stored (Ptr Global (Stored Uint8))
type SemaphoreHandle = Ref Global Semaphore

semaphoreWrapperHeader :: String
semaphoreWrapperHeader = "echronos_semaphore_wrapper.h"

create :: Def ('[ SemaphoreHandle ] :-> ())
create = importProc "ivory_echronos_semaphore_create" semaphoreWrapperHeader

-- semaphore handle, maximum count, initial count
create_counting :: Def ('[ SemaphoreHandle, Uint32, Uint32 ] :-> ())
create_counting =
  importProc "ivory_echronos_semaphore_create_counting" semaphoreWrapperHeader

take :: Def ('[ SemaphoreHandle, Uint32 ] :-> IBool)
take = importProc "ivory_echronos_semaphore_take" semaphoreWrapperHeader

takeBlocking :: Def ('[ SemaphoreHandle ] :-> ())
takeBlocking =
  importProc "ivory_echronos_semaphore_takeblocking" semaphoreWrapperHeader

takeNonblocking  :: Def ('[ SemaphoreHandle ] :-> IBool)
takeNonblocking =
  importProc "ivory_echronos_semaphore_takenonblocking" semaphoreWrapperHeader

give :: Def('[ SemaphoreHandle ] :-> ())
give = importProc "ivory_echronos_semaphore_give" semaphoreWrapperHeader

give_isr :: Def('[ SemaphoreHandle ] :-> ())
give_isr = importProc "ivory_echronos_semaphore_give_isr" semaphoreWrapperHeader


