{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.OS.FreeRTOS.Queue where

import Ivory.Language

-- Dirty tricks:
-- an xQueueHandle is actually a void*, but we don't have a void*
-- in Ivory. So, we keep around a uint16_t* to instansiate it as an area
-- and then, because we must pass areas by refs, end up passing uint8_t**
-- to every freertos wrapper function that needs the xQueueHandle.
-- the wrapper functions will deref the argument onceand cast the remaining
-- uint8_t* to a xQueueHandle to use it.
--
-- (we're using a uint16 as the base type for a tiny bit of type safety
-- against this same trick used with Uint8 in Ivory.OS.FreeRTOS.Semaphore)
--
--

type Queue = Stored (Ptr Global (Stored Uint16))
type QueueHandle = Ref Global Queue

queueWrapperHeader :: String
queueWrapperHeader = "freertos_queue_wrapper.h"

-- XXX this is a little sloppy?
maxWaitInt :: Integer
maxWaitInt = 4294967296
maxWait :: Uint32
maxWait = maxBound

-- Queue contents:
-- Queues can only store uint32s, because type safety is hard, you guys

create :: Def ('[ QueueHandle
                , Uint32 -- Queue Size
                ] :-> ())
create = importProc "ivory_freertos_queue_create" queueWrapperHeader

send :: Def ('[ QueueHandle
              , Uint32 -- Value
              , Uint32 -- Wait time
              ] :-> IBool) -- True if send is successful
send = importProc "ivory_freertos_queue_send" queueWrapperHeader

receive :: Def ('[ QueueHandle
                 , Ref s1 (Stored Uint32) -- Value
                 , Uint32 -- Wait time
                 ] :-> IBool) -- true if receive is successful
receive = importProc "ivory_freertos_queue_receive" queueWrapperHeader

messagesWaiting :: Def ('[ QueueHandle ] :-> Uint32)
messagesWaiting = importProc "ivory_freertos_queue_messages_waiting" queueWrapperHeader

