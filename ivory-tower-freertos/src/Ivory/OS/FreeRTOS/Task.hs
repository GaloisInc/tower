{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.OS.FreeRTOS.Task where

import Ivory.Language

taskModule :: Module
taskModule = package "ivory_os_freertos_task" $ do
  inclHeader "freertos_task_wrapper.h"
  incl create
  incl delay
  incl delayUntil
  incl getTimeMillis

create :: Def ('[ ProcPtr ('[]:->()), Uint32, Uint8 ] :->())
create = externProc "ivory_freertos_task_create"

delay :: Def ('[ Uint32 ] :->())
delay = externProc "ivory_freertos_task_delay"

delayUntil :: Def ('[ Ref s (Stored Uint32), Uint32 ] :->())
delayUntil = externProc "ivory_freertos_task_delayuntil"

getTimeMillis :: Def ('[] :-> Uint32 )
getTimeMillis = externProc "ivory_freertos_task_gettickcount"
