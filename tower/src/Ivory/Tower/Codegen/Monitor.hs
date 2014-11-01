
module Ivory.Tower.Codegen.Monitor
  ( monitorLockProcName
  , monitorUnlockProcName
  ) where

import qualified Ivory.Tower.AST as AST

monitorUnlockProcName :: AST.Monitor -> String
monitorUnlockProcName mon = "monitor_unlock_" ++ AST.monitorName mon

monitorLockProcName :: AST.Monitor -> String
monitorLockProcName mon = "monitor_lock_" ++ AST.monitorName mon

