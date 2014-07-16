{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.GRTOS.TaskArg where

import Ivory.Language

-- Taskarg is a Struct used to stand in for C's void*. It should
-- only be used as an argument to a TaskProc, and never dereferenced.
-- Don't instantiate this into an Ivory module: defined in
-- grtos_task_wrapper.h (external C header)

[ivory|struct taskarg { taskarg_garbage :: Stored Uint32 }|]
