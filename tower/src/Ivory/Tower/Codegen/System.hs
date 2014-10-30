{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Codegen.System
  ( generatedCodeModules
  ) where

import qualified Data.Map as Map
import Data.String (fromString)
import Data.List (sort, elemIndex)

import Ivory.Tower.Types.GeneratedCode
import Ivory.Tower.Types.ThreadCode
import Ivory.Tower.Types.Time
import Ivory.Tower.Codegen.Monitor
import Ivory.Tower.Codegen.Handler

import qualified Ivory.Tower.AST as AST

import Ivory.Language

import qualified Ivory.OS.FreeRTOS.Task as Task
import qualified Ivory.OS.FreeRTOS.Time as Time
import qualified Ivory.OS.FreeRTOS.BinarySemaphore as Semaphore

generatedCodeModules :: GeneratedCode -> AST.Tower -> [Module]
generatedCodeModules gc twr
   = generatedcode_modules gc
  ++ map threadUserModule ts
  ++ map threadGenModule  ts
  ++ concatMap monitorModules ms
  ++ [ initModule twr ]
  where
  ms = Map.toList (generatedcode_monitors gc)
  ts = Map.elems (generatedcode_threads gc)

  dependencies = mapM_ depend (generatedcode_depends gc)

  monitorModules (ast, code) = generateMonitorCode code ast dependencies

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
  where
  period_ms :: Uint32
  period_ms = fromIntegral (toMilliseconds (AST.period_dt p))

  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (threadLoopProcName thr) $ const $ body $ noReturn $ do
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
  Semaphore.moddef
  incl tloopProc
  defMemArea sem
  unGeneratedSignal (generatedCodeForSignal s gc) $ do
    call_ Semaphore.giveFromISR (addrOf sem)
  where
  sem :: MemArea (Stored Semaphore.BinarySemaphore)
  sem = area ("signal_semaphore_" ++ AST.threadName thr) Nothing
  tloopProc :: Def('[Ref Global (Struct "taskarg")]:->())
  tloopProc = proc (threadLoopProcName thr) $ const $ body $ noReturn $ do
    t_rate <- call Time.getTickRateMilliseconds
    let toITime :: Uint32 -> ITime
        toITime t = fromIMilliseconds (t `iDiv` t_rate)
    call_ Semaphore.create (addrOf sem)
    forever $ noBreak $ do
      call_ Semaphore.take (addrOf sem)
      now <- call Time.getTickCount
      t <- local (ival (toITime now))
      threadLoopRunHandlers twr thr t


initModule :: AST.Tower -> Module
initModule twr = package "tower_init" $ do
  Task.moddef
  sequence_ [ depend (package (monitorGenModName m) (return ()))
            | m <- AST.tower_monitors twr ]
  sequence_ [ depend (package (threadGenCodeModName t) (return ()))
            | t <- AST.towerThreads twr ]
  incl entryProc
  where
  entryProc :: Def('[]:->())
  entryProc = proc "tower_entry" $ body $ do
    sequence_ [ call_ (monitorInitProc m) | m <- AST.tower_monitors twr ]
    sequence_ [ threadBegin twr thr | thr <- AST.towerThreads twr ]

threadBegin :: AST.Tower -> AST.Thread -> Ivory eff ()
threadBegin twr thr = do
  call_ Task.begin (Task.taskProc threadLoopProcStub)
                stacksize priority debugname
  where
  threadLoopProcStub :: Def('[Ref s (Struct "taskarg")]:->())
  threadLoopProcStub = proc (threadLoopProcName thr)
                        (const (body (return ())))
  stacksize :: Uint32
  stacksize = 1024 -- XXX need some story for computing this

  priority :: Uint8
  priority = fromIntegral (threadPriority twr thr)

  debugname :: IString
  debugname = fromString (AST.threadName thr)

threadPriority :: AST.Tower -> AST.Thread -> Int
threadPriority twr thr = idx + 1
  where
  Just idx = elemIndex thr priorityordering
  priorityordering = reverse (sort (AST.towerThreads twr))

