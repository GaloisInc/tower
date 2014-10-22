
module Ivory.Tower.Codegen.Monitor
  ( generateMonitorCode
  , monitorLockProc
  , monitorUnlockProc
  ) where

import Ivory.Tower.Types.MonitorCode

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.ToyObjLang

generateMonitorCode :: MonitorCode
                    -> AST.Monitor
                    -> [Module]
generateMonitorCode mc mon =
  [ package state_pkgname (monitorcode_moddef mc mon)
  , package sync_pkgname sync_pkg
  ]
  where
  state_pkgname = "t_monitor_state_" ++ AST.monitorName mon
  sync_pkgname = "t_monitor_sync_" ++ AST.monitorName mon
  sync_pkg = do
    defProc (monitorLockProc mon)
    defProc (monitorUnlockProc mon)

monitorUnlockProc :: AST.Monitor -> Proc
monitorUnlockProc mon = proc n [] (stmt "unlock placeholder")
  where n = "monitor_unlock_" ++ AST.monitorName mon

monitorLockProc :: AST.Monitor -> Proc
monitorLockProc mon = proc n [] (stmt "lock placeholder")
  where n = "monitor_lock_" ++ AST.monitorName mon
