{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Task where


import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Monad

-- Public Task Definitions -----------------------------------------------------

-- | Track Ivory dependencies used by the 'Ivory.Tower.Tower.taskBody' created
--   in the 'Ivory.Tower.Types.Task' context.
taskModuleDef :: (Schedule -> ModuleDef) -> Task ()
taskModuleDef = taskStAddModuleDef

-- | Specify the stack size, in bytes, of the 'Ivory.Tower.Tower.taskBody'
--   created in the 'Ivory.Tower.Types.Task' context.
withStackSize :: Integer -> Task ()
withStackSize stacksize = do
  s <- getTaskSt
  case taskst_stacksize s of
    Nothing -> setTaskSt $ s { taskst_stacksize = Just stacksize }
    Just _  -> fail ("Cannot use withStackSize more than once in task named "
                  ++ taskst_name s)

-- | Specify an OS priority level of the 'Ivory.Tower.Tower.taskBody' created in
--   the 'Ivory.Tower.Types.Task' context. Implementation at the backend
--   defined by the 'Ivory.Tower.Types.OS' implementation.
withPriority :: Integer -> Task ()
withPriority p = do
  s <- getTaskSt
  case taskst_priority s of
    Nothing -> setTaskSt $ s { taskst_priority = Just p }
    Just _  -> fail ("Cannot use withPriority more than once in task named "
                     ++ taskst_name s)

-- | Add an Ivory Module to the result of this Tower compilation, from the
--   Task context.
withModule :: Module -> Task ()
withModule m = do
  s <- getTaskSt
  setTaskSt $ s { taskst_extern_mods = m:(taskst_extern_mods s)}

