{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.GRTOS.Kernel.TaskControlBlock where

import Ivory.Language

-- struct task_control_block is defined in grtos kernel, exposed by
-- grtos_kernel.h.
--
-- We need to allocate global tcbs and pass references to c functions, so we
-- make an Ivory version of the struct with only a single invalid accessor.
-- As long as the accessor is never used, generated code should be valid!
--

[ivory|
abstract struct task_control_block "grtos_kernel.h"
|]

taskControlBlockTypeModule :: Module
taskControlBlockTypeModule = package "task_control_block_type" $ do
  inclHeader "grtos_kernel.h"

