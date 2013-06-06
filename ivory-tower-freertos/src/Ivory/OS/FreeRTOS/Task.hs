{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.OS.FreeRTOS.Task where

import Ivory.Language

taskWrapperHeader :: String
taskWrapperHeader = "freertos_task_wrapper.h"

create :: Def ('[ ProcPtr ('[]:->()), Uint32, Uint8 ] :->())
create = importProc "ivory_freertos_task_create" taskWrapperHeader

delay :: Def ('[ Uint32 ] :->())
delay = importProc "ivory_freertos_task_delay" taskWrapperHeader

delayUntil :: Def ('[ Ref s (Stored Uint32), Uint32 ] :->())
delayUntil = importProc "ivory_freertos_task_delayuntil" taskWrapperHeader

getTimeMillis :: Def ('[] :-> Uint32 )
getTimeMillis = importProc "ivory_freertos_task_gettickcount" taskWrapperHeader
