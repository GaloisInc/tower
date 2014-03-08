{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Task
  ( TaskProc
  , begin
  ) where

import Ivory.Language
import Ivory.OS.FreeRTOS.TaskArg ()

newtype TaskProc =
  TaskProc
    { _unTaskProc :: ProcPtr ('[Ref Global (Struct "taskarg")]:->())
    } deriving (IvoryType)

taskWrapperHeader :: String
taskWrapperHeader = "freertos_task_wrapper.h"

begin :: Def ('[ TaskProc, Uint32, Uint8 ] :->())
begin = importProc "ivory_freertos_task_create" taskWrapperHeader

