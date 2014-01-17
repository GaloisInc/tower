{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.OS.EChronos.Task where

import Ivory.Language

taskWrapperHeader :: String
taskWrapperHeader = "echronos_task_wrapper.h"

create :: Def ('[ ProcPtr ('[]:->()), Uint32, Uint8 ] :->())
create = importProc "ivory_echronos_task_create" taskWrapperHeader

delay :: Def ('[ Uint32 ] :->())
delay = importProc "ivory_echronos_task_delay" taskWrapperHeader

delayUntil :: Def ('[ Ref s (Stored Uint32), Uint32 ] :->())
delayUntil = importProc "ivory_echronos_task_delayuntil" taskWrapperHeader

getTimeMillis :: Def ('[] :-> Uint32)
getTimeMillis =
  importProc "ivory_echronos_task_getmilliscount" taskWrapperHeader

getTimeTick :: Def ('[] :-> Uint32)
getTimeTick = importProc "ivory_echronos_task_gettickcount" taskWrapperHeader

millisToTicks :: Def ('[Uint32] :-> Uint32)
millisToTicks = importProc "ivory_echronos_task_millistoticks" taskWrapperHeader
