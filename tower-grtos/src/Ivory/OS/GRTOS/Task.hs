{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.GRTOS.Task
  ( TaskProc
  , taskProc
  , taskWrapperHeader
  , begin
  , moddef
  ) where

import Ivory.Language
import Ivory.OS.GRTOS.TaskArg ()

newtype TaskProc =
  TaskProc
    { _unTaskProc :: ProcPtr ('[Ref Global (Struct "taskarg")]:->())
    } deriving (IvoryType, IvoryVar)

moddef :: ModuleDef
moddef = do
  inclHeader taskWrapperHeader
  sourceDep taskWrapperHeader
  sourceDep "grtos_task_wrapper.c"

taskWrapperHeader :: String
taskWrapperHeader = "grtos_task_wrapper.h"

--                         Stack size, Priority, Debugging Name
begin :: Def ('[ TaskProc, Uint32, Uint8, IString] :->())
begin = importProc "ivory_grtos_task_create" taskWrapperHeader

taskProc :: Def('[Ref Global (Struct "taskarg")]:->()) -> TaskProc
taskProc = TaskProc . procPtr

