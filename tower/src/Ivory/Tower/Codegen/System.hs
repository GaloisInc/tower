{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Codegen.System
  ( generatedCodeModules
  ) where

import qualified Data.Map as Map

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Time
import Ivory.Tower.Codegen.Monitor
import Ivory.Tower.Codegen.Handler

import qualified Ivory.Tower.AST as AST

import Ivory.Language

generatedCodeModules :: GeneratedCode -> AST.Tower -> [Module]
generatedCodeModules gc twr
   = generatedcode_modules gc
  ++ map threadUserModule ts
  ++ map threadGenModule ts
  ++ concatMap monitorModules ms
  where
  ms = Map.toList (generatedcode_monitors gc)
  ts = Map.elems (generatedcode_threads gc)

  monitorModules (ast, code) = generateMonitorCode code ast

  threadUserModule t =
    package (threadUserCodeModName t) $ do
      depend (threadGenModule t)
      threadcode_user t
  threadGenModule t =
    package (threadGenCodeModName t) $ do
      depend (threadUserModule t)
      mapM_ depend (threadGenDeps (threadcode_thread t))
      threadLoopModdef twr (threadcode_thread t)
      threadcode_gen t


  threadGenDeps :: AST.Thread -> [Module]
  threadGenDeps t = [ package (monitorGenModName m) (return ())
                    | (m,_h) <- AST.threadHandlers (AST.messageGraph twr) t ]

threadUserCodeModName :: ThreadCode -> String
threadUserCodeModName tc = "tower_user_"
  ++ AST.threadName (threadcode_thread tc)

threadGenCodeModName :: ThreadCode -> String
threadGenCodeModName tc = "tower_gen_"
  ++ AST.threadName (threadcode_thread tc)

threadLoopProcName :: AST.Thread -> String
threadLoopProcName t = "loop_" ++ AST.threadName t

threadLoopProc :: AST.Tower -> AST.Thread -> Def('[]:->())
threadLoopProc twr thr = proc (threadLoopProcName thr) $ body $ do
  -- XXX guard on init here
  forever $ do
    -- XXX guard on event here
    t <- local (ival 666) -- XXX
    sequence_ [ call_ (hproc h) (constRef t)
              | (_m,h) <- AST.towerChanHandlers twr (AST.threadChan thr) ]

  where
  hproc :: AST.Handler -> Def('[ConstRef s (Stored ITime)]:->())
  hproc h = proc (handlerProcName h thr) (const (body (return ())))


threadLoopModdef :: AST.Tower -> AST.Thread -> ModuleDef
threadLoopModdef twr thr = do
  incl (threadLoopProc twr thr)

