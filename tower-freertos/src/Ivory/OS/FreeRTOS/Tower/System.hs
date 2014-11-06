{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.OS.FreeRTOS.Tower.System
  ( threadModules
  , monitorModules
  , systemModules
  , systemArtifacts
  ) where

import Control.Monad (forM_, when)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.List (sort, elemIndex)

import Text.Show.Pretty

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Time
import Ivory.Tower.Codegen.Handler
import qualified Ivory.Tower.AST.Graph as G
import qualified Ivory.Tower.AST as AST

import Ivory.Language
import Ivory.Artifact

import Ivory.OS.FreeRTOS.Tower.Init
import Ivory.OS.FreeRTOS.Tower.Signal
import Ivory.OS.FreeRTOS.Tower.Monitor

import qualified Ivory.OS.FreeRTOS.Task as Task
import qualified Ivory.OS.FreeRTOS.Time as Time

systemArtifacts :: AST.Tower -> [Module] -> [Artifact]
systemArtifacts twr ms =
  [ artifactString "debug_mods.txt" dbg
  , artifactString "debug_ast.txt" (ppShow twr)
  , artifactString "out.dot" (G.graphviz (G.messageGraph twr))
  ]
  where
  dbg = (show mods)
  mods = map moduleName ms

monitorModules :: GeneratedCode -> AST.Tower -> [Module]
monitorModules gc _twr = concatMap permon ms
  where
  ms = Map.toList (generatedcode_monitors gc)
  permon (ast, code) = generateMonitorCode gc code ast

threadModules :: GeneratedCode -> AST.Tower-> [Module]
threadModules gc twr = concatMap pertask (Map.elems (generatedcode_threads gc))
  where
  pertask tc = [threadUserModule tc, threadGenModule tc]
  threadUserModule tc =
    let t = threadcode_thread tc in
    package (threadUserCodeModName t) $ do
      dependencies
      threadMonitorDeps t monitorStateModName
      depend (threadGenModule tc)
      threadcode_user tc
  threadGenModule tc =
    let t = threadcode_thread tc in
    package (threadGenCodeModName t) $ do
      dependencies
      depend (threadUserModule tc)
      threadMonitorDeps t monitorGenModName
      threadLoopModdef gc twr t
      threadcode_gen tc

  dependencies = mapM_ depend (generatedcode_depends gc)

  threadMonitorDeps :: AST.Thread -> (AST.Monitor -> String) -> ModuleDef
  threadMonitorDeps t mname = sequence_
    [ depend $ package (mname m) $ return ()
    | (m,_) <- AST.threadHandlers (AST.messageGraph twr) t ]

threadUserCodeModName :: AST.Thread -> String
threadUserCodeModName t = "tower_user_" ++ AST.threadName t

threadGenCodeModName :: AST.Thread -> String
threadGenCodeModName t = "tower_gen_" ++ AST.threadName t

threadLoopProcName :: AST.Thread -> String
threadLoopProcName t = "loop_" ++ AST.threadName t


threadLoopRunHandlers :: AST.Tower -> AST.Thread
                      -> Ref s (Stored ITime) -> Ivory eff ()
threadLoopRunHandlers twr thr t = sequence_
  [ call_ (hproc h) (constRef t)
  | (_m,h) <- AST.towerChanHandlers twr (AST.threadChan thr) ]
  where
  hproc :: AST.Handler -> Def('[ConstRef s (Stored ITime)]:->())
  hproc h = proc (handlerProcName h thr) (const (body (return ())))

threadLoopModdef :: GeneratedCode -> AST.Tower -> AST.Thread -> ModuleDef
threadLoopModdef _gc twr thr@(AST.PeriodThread p) = do
  Task.moddef
  Time.moddef
  incl tloopProc
  codegeninit_moddef cgi

  where
  period_ms :: Uint32
  period_ms = fromIntegral (toMilliseconds (AST.period_dt p))

  cgi = codegenInit thr
  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (threadLoopProcName thr) $ const $ body $ noReturn $ do
    codegeninit_block cgi

    t_init <- call Time.getTickCount
    t_last_wake <- local (ival (t_init))

    t_rate <- call Time.getTickRateMilliseconds
    let toITime :: Uint32 -> ITime
        toITime t = fromIMilliseconds (t `iDiv` t_rate)

    forever $ noBreak $ do
      call_ Time.delayUntil t_last_wake (t_rate * period_ms)
      now <- deref t_last_wake
      t <- local (ival (toITime now))
      threadLoopRunHandlers twr thr t

threadLoopModdef gc twr thr@(AST.SignalThread s) = do
  Task.moddef
  Time.moddef
  incl tloopProc
  codegensignal_moddef cgs
  codegeninit_moddef cgi

  unGeneratedSignal (generatedCodeForSignal s gc) $ codegensignal_ready cgs
  where
  cgs = codegenSignal thr
  cgi = codegenInit thr
  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (threadLoopProcName thr) $ const $ body $ noReturn $ do
    t_rate <- call Time.getTickRateMilliseconds
    let toITime :: Uint32 -> ITime
        toITime t = fromIMilliseconds (t `iDiv` t_rate)
    codegeninit_block cgi
    forever $ noBreak $ do
      codegensignal_wait cgs
      now <- call Time.getTickCount
      t <- local (ival (toITime now))
      threadLoopRunHandlers twr thr t

threadLoopModdef _gc twr thr@(AST.InitThread _) = do
  Task.moddef
  Time.moddef
  incl tloopProc
  forM_ (AST.towerThreads twr) $ \t -> when (t /= thr) $
    depend (package (threadGenCodeModName t) (return ()))

  where
  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (threadLoopProcName thr) $ const $ body $ noReturn $ do
    t <- local (ival 0)
    threadLoopRunHandlers twr thr t
    forM_ (AST.towerThreads twr) (codegeninit_unblock . codegenInit)
    forever $ noBreak $ return ()

systemModules :: AST.Tower -> [Module]
systemModules twr = [initModule]
  where
  initModule = package "tower_init" $ do
    Task.moddef
    sequence_ [ depend (package (monitorGenModName m) (return ()))
              | m <- AST.tower_monitors twr ]
    sequence_ [ depend (package (threadGenCodeModName t) (return ()))
              | t <- AST.towerThreads twr ]
    incl entryProc
    where
    entryProc :: Def('[]:->())
    entryProc = proc "tower_entry" $ body $ do
      forM_ (AST.tower_monitors twr) $ \m -> do
        call_ (monitorInitProc m)
      forM_ (AST.towerThreads twr) $ \thr -> do
        codegensignal_init (codegenSignal thr)
        codegeninit_init (codegenInit thr)
      forM_ (AST.towerThreads twr) $ \thr -> do
        threadBegin thr

  threadBegin :: AST.Thread -> Ivory eff ()
  threadBegin thr = do
    call_ Task.begin (Task.taskProc threadLoopProcStub)
                  stacksize priority debugname
    where
    threadLoopProcStub :: Def('[Ref s (Struct "taskarg")]:->())
    threadLoopProcStub = proc (threadLoopProcName thr)
                          (const (body (return ())))
    stacksize :: Uint32
    stacksize = 1024 -- XXX need some story for computing this

    debugname :: IString
    debugname = fromString (AST.threadName thr)

    priority :: Uint8
    priority = fromIntegral (idx + 1)
      where
      Just idx = elemIndex thr priorityordering
      priorityordering = reverse (sort (AST.towerThreads twr))

