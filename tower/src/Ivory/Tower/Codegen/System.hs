{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Codegen.System
  ( generatedCodeModules
  ) where

import qualified Data.Map as Map
import Data.String (fromString)

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Time
import Ivory.Tower.Codegen.Monitor
import Ivory.Tower.Codegen.Handler

import qualified Ivory.Tower.AST as AST

import Ivory.Language

import qualified Ivory.OS.FreeRTOS.Task as Task

generatedCodeModules :: GeneratedCode -> AST.Tower -> [Module]
generatedCodeModules gc twr
   = generatedcode_modules gc
  ++ map threadUserModule ts
  ++ map threadGenModule ts
  ++ concatMap monitorModules ms
  ++ [ initModule twr ]
  where
  ms = Map.toList (generatedcode_monitors gc)
  ts = Map.elems (generatedcode_threads gc)

  monitorModules (ast, code) = generateMonitorCode code ast

  threadUserModule tc =
    let t = threadcode_thread tc in
    package (threadUserCodeModName t) $ do
      depend (threadGenModule tc)
      threadcode_user tc
  threadGenModule tc =
    let t = threadcode_thread tc in
    package (threadGenCodeModName t) $ do
      depend (threadUserModule tc)
      mapM_ depend (threadGenDeps t)
      threadLoopModdef twr t
      threadcode_gen tc


  threadGenDeps :: AST.Thread -> [Module]
  threadGenDeps t = [ package (monitorGenModName m) (return ())
                    | (m,_h) <- AST.threadHandlers (AST.messageGraph twr) t ]

threadUserCodeModName :: AST.Thread -> String
threadUserCodeModName t = "tower_user_" ++ AST.threadName t

threadGenCodeModName :: AST.Thread -> String
threadGenCodeModName t = "tower_gen_" ++ AST.threadName t

threadLoopProcName :: AST.Thread -> String
threadLoopProcName t = "loop_" ++ AST.threadName t


threadLoopModdef :: AST.Tower -> AST.Thread -> ModuleDef
threadLoopModdef twr thr = do
  Task.moddef
  incl (threadLoopProc twr thr)


threadLoopProc :: AST.Tower -> AST.Thread
               -> Def('[Ref Global (Struct "taskarg")]:->())
threadLoopProc twr thr = proc (threadLoopProcName thr) $ const $ body $ do
  -- XXX guard on init here
  forever $ do
    -- XXX guard on event here
    t <- local (ival 666) -- XXX
    sequence_ [ call_ (hproc h) (constRef t)
              | (_m,h) <- AST.towerChanHandlers twr (AST.threadChan thr) ]

  where
  hproc :: AST.Handler -> Def('[ConstRef s (Stored ITime)]:->())
  hproc h = proc (handlerProcName h thr) (const (body (return ())))

initModule :: AST.Tower -> Module
initModule twr = package "tower_init" $ do
  Task.moddef
  sequence_ [ depend (package (threadGenCodeModName t) (return ()))
            | t <- AST.towerThreads twr ]
  incl initProc
  where
  initProc :: Def('[]:->())
  initProc = proc "tower_init" $ body $ do
    -- XXX init all sync primitives.
    sequence_ [ threadBegin twr thr | thr <- AST.towerThreads twr ]

threadBegin :: AST.Tower -> AST.Thread -> Ivory eff ()
threadBegin twr thr = do
  call_ Task.begin (Task.taskProc (threadLoopProc twr thr))
                stacksize priority debugname
  where
  stacksize :: Uint32
  stacksize = 1024 -- XXX need some story for computing this

  priority :: Uint8
  priority = 3 -- XXX should be possible to compute this properly

  debugname :: IString
  debugname = fromString (AST.threadName thr)


