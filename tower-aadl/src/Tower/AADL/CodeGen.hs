--
-- Code generation of handler state machines, etc. for AADL targets.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.CodeGen where

import qualified Ivory.Language  as I
import qualified Ivory.Compile.C.CmdlineFrontend as C

import           Ivory.Tower.AST.Monitor
import           Ivory.Tower.AST.Thread
import           Ivory.Tower.Backend.Compat
import           Ivory.Tower.Types.Dependencies
import           Ivory.Tower.Types.SignalCode
import           Ivory.Tower.Types.ThreadCode

import qualified Data.Map as M

--------------------------------------------------------------------------------

genIvoryCode :: C.Opts -> TowerBackendOutput CompatBackend -> Dependencies -> SignalCode -> IO ()
genIvoryCode opts
  CompatOutput
  { compatoutput_threads = threads
  , compatoutput_monitors = monitors
  }
  Dependencies
  { dependencies_modules = mods
  , dependencies_depends = depends
  , dependencies_artifacts = artifacts
  }
  SignalCode
  { signalcode_signals = signals
  --, signalcode_init = gcinit
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
  ThreadCode { threadcode_user = usr }
  = I.package (threadName th) usr

mkMonitorCode :: Monitor -> I.ModuleDef -> I.Module
mkMonitorCode m code
  = I.package (monitorName m) code

mkSignalCode :: String -> GeneratedSignal -> I.Module
mkSignalCode sigNm
  GeneratedSignal { unGeneratedSignal = s }
  -- XXX assuming for now that we don't have unsafe signals. Pass the platform
  -- signal continuation here for eChronos.
  = I.package sigNm (s (return ()))

