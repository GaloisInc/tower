
module Ivory.Tower.Codegen.System
  ( generatedCodeModules
  ) where

import qualified Data.Map as Map

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Codegen.Monitor

import qualified Ivory.Tower.AST as AST

import Ivory.Language

generatedCodeModules :: GeneratedCode -> AST.Tower -> [Module]
generatedCodeModules gc _twr
   = generatedcode_modules gc
  ++ map threadUserModule ts
  ++ map threadGenModule ts
  ++ concatMap monitorModules ms
  where
  ms = Map.toList (generatedcode_monitors gc)
  ts = Map.elems (generatedcode_threads gc)

  monitorModules (ast, code) = generateMonitorCode code ast

  threadUserModule t = package ("t_u_" ++ AST.threadName (threadcode_thread t))
                                (threadcode_user t)
  threadGenModule t = package ("t_g_" ++ AST.threadName (threadcode_thread t))
                               (threadcode_gen t)

