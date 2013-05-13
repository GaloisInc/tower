{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task where


import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Monad


-- Public Task Definitions -----------------------------------------------------

-- | Track Ivory dependencies used by the 'Ivory.Tower.Tower.taskLoop' created in
--   the 'Ivory.Tower.Types.Scheduled' context.
taskModuleDef :: ModuleDef -> Scheduled ()
taskModuleDef md = do
  ctx <- getTaskResult
  setTaskResult (ctx { taskres_moddef = ((taskres_moddef ctx) >> md) })

-- | Specify the stack size, in bytes, of the 'Ivory.Tower.Tower.taskLoop' created in
--   the 'Ivory.Tower.Types.Scheduled' context.
withStackSize :: Integer -> Scheduled ()
withStackSize s = do
  ctx <- getTaskResult
  case taskres_stacksize ctx of
    Nothing -> setTaskResult (ctx { taskres_stacksize = Just s })
    Just _  -> fail ("Cannot use withStackSize more than once in task named "
                  ++ taskres_name ctx)

-- | Specify an OS priority level of the 'Ivory.Tower.Tower.taskLoop' created in
--   the 'Ivory.Tower.Types.Scheduled' context. Implementation at the backend
--   defined by the 'Ivory.Tower.Types.OS' implementation.
withPriority :: Integer -> Scheduled ()
withPriority p = do
  ctx <- getTaskResult
  case taskres_priority ctx of
    Nothing -> setTaskResult (ctx { taskres_priority = Just p })
    Just _  -> fail ("Cannot use withPriority more than once in task named "
                     ++ taskres_name ctx)
-- | Add an Ivory Module to the result of this Tower compilation, from the
--   Scheduled context.
withModule :: Module -> Scheduled ()
withModule m = do
  ctx <- getTaskResult
  setTaskResult (ctx { taskres_extern_mods = m:(taskres_extern_mods ctx)})

