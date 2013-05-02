{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task where


import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Monad


-- Public Task Definitions -----------------------------------------------------

taskModuleDef :: ModuleDef -> Scheduled ()
taskModuleDef md = do
  ctx <- getTaskResult
  setTaskResult (ctx { taskres_moddef = ((taskres_moddef ctx) >> md) })

withStackSize :: Integer -> Scheduled ()
withStackSize s = do
  ctx <- getTaskResult
  case taskres_stacksize ctx of
    Nothing -> setTaskResult (ctx { taskres_stacksize = Just s })
    Just _  -> fail ("Cannot use withStackSize more than once in task named "
                  ++ taskres_name ctx)

withPriority :: Integer -> Scheduled ()
withPriority p = do
  ctx <- getTaskResult
  case taskres_priority ctx of
    Nothing -> setTaskResult (ctx { taskres_priority = Just p })
    Just _  -> fail ("Cannot use withPriority more than once in task named "
                     ++ taskres_name ctx)

withModule :: Module -> Scheduled ()
withModule m = do
  ctx <- getTaskResult
  setTaskResult (ctx { taskres_extern_mods = m:(taskres_extern_mods ctx)})

