{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Task
  ( TaskProc
  , taskProc
  , taskWrapperHeader
  , begin
  , moddef
  ) where

import Ivory.Language
import Ivory.OS.FreeRTOS.TaskArg ()

newtype TaskProc =
  TaskProc
    { _unTaskProc :: ProcPtr ('[Ref Global (Struct "taskarg")]:->())
    } deriving (IvoryType, IvoryVar)

moddef :: ModuleDef
moddef = do
  inclHeader taskWrapperHeader
  sourceDep taskWrapperHeader
  sourceDep "freertos_task_wrapper.c"

taskWrapperHeader :: String
taskWrapperHeader = "freertos_task_wrapper.h"

--                         Stack size, Priority
begin :: Def ('[ TaskProc, Uint32, Uint8 ] :->())
begin = importProc "ivory_freertos_task_create" taskWrapperHeader

taskProc :: Def('[Ref Global (Struct "taskarg")]:->()) -> TaskProc
taskProc = TaskProc . procPtr

