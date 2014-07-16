{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.OS.GRTOS.BinarySemaphore where

import Prelude hiding (take)
import Ivory.Language

-- Dirty tricks:
-- an xSemaphoreHandle is actually a void*, but we don't have a void*
-- in Ivory. So, we keep around a uint8_t* to instansiate it as an area
-- and then, because we must pass areas by refs, end up passing uint8_t**
-- to every grtos wrapper function that needs the xSemaphoreHandle.
-- the wrapper functions will deref the argument onceand cast the remaining
-- uint8_t* to a xSemaphoreHandle to use it.

newtype BinarySemaphore =
  BinarySemaphore (Ptr Global (Stored Uint8))
  deriving (IvoryType, IvoryVar, IvoryStore)
type BinarySemaphoreHandle = Ref Global (Stored BinarySemaphore)

semaphoreWrapperHeader :: String
semaphoreWrapperHeader = "grtos_semaphore_wrapper.h"

create :: Def ('[ BinarySemaphoreHandle ] :-> ())
create = importProc "ivory_grtos_semaphore_create_binary" semaphoreWrapperHeader

take :: Def ('[ BinarySemaphoreHandle, Uint32 ] :-> IBool)
take = importProc "ivory_grtos_semaphore_take" semaphoreWrapperHeader

give :: Def('[ BinarySemaphoreHandle ] :-> ())
give = importProc "ivory_grtos_semaphore_give" semaphoreWrapperHeader

giveFromISR :: Def('[ BinarySemaphoreHandle ] :-> ())
giveFromISR = importProc "ivory_grtos_semaphore_give_from_isr" semaphoreWrapperHeader

