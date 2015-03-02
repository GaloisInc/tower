--
-- Code generation of handler state machines, etc. for AADL targets.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.CodeGen where

import qualified Ivory.Language  as I
import qualified Ivory.Compile.C as C
import qualified Ivory.Compile.C.CmdlineFrontend as C

import           Ivory.Tower.AST.Monitor
import           Ivory.Tower.AST.Thread

import           Ivory.Tower.Types.MonitorCode
import           Ivory.Tower.Types.Signalable
import           Ivory.Tower.Types.ThreadCode
import           Ivory.Tower.Types.GeneratedCode as G

import           Text.PrettyPrint.Leijen
import qualified Data.Map as M

--------------------------------------------------------------------------------

genIvoryCode :: C.Opts -> G.GeneratedCode -> IO ()
genIvoryCode opts
  G.GeneratedCode
  { G.generatedcode_modules   = mods
  , G.generatedcode_depends   = depends
  , G.generatedcode_threads   = threads
  , G.generatedcode_monitors  = monitors
  , G.generatedcode_signals   = signals
  , G.generatedcode_init      = init
  , G.generatedcode_artifacts = artifacts
  } = C.runCompiler modules artifacts opts
  where
  modules = mods
         ++ depends
         ++ go mkThreadCode  threads
         ++ go mkMonitorCode monitors
         ++ go mkSignalCode  signals
  go c cs = M.elems $ M.mapWithKey c cs

-- Note: handler code gets put into the thread by Tower front-end.
mkThreadCode :: Thread -> ThreadCode -> I.Module
mkThreadCode th
  ThreadCode { threadcode_gen = gen }
  = I.package (threadName th ++ "gen") gen

mkMonitorCode :: Monitor -> MonitorCode -> I.Module
mkMonitorCode m
  MonitorCode { monitorcode_moddef = code }
  = I.package (monitorName m) code

mkSignalCode :: String -> G.GeneratedSignal -> I.Module
mkSignalCode signalName
  G.GeneratedSignal { G.unGeneratedSignal = s }
  -- XXX assuming for now that we don't have unsafe signals. Pass the platform
  -- signal continuation here for eChronos.
  = I.package signalName (s (return ()))
