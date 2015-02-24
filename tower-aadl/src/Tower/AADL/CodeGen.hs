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
  -- , G.generatedcode_signals   = signals
--  , G.generatedcode_init      = init
  , G.generatedcode_artifacts = artifacts
  } = C.runCompiler modules artifacts opts
  where
  modules = mods
         ++ depends
         ++ go mkThreadCode  threads
         ++ go mkMonitorCode monitors
--         ++ go mkSignalCode  signals
  go c cs = M.elems $ M.mapWithKey c cs

-- Note: handler code "lives" in here.
mkThreadCode :: Thread -> ThreadCode -> I.Module
mkThreadCode th
  ThreadCode
  { --threadcode_thread = threadAST
--    threadcode_user   = user
    threadcode_gen    = gen
  } = -- map (I.package (threadName th)) [user, gen]
--     [ I.package (threadName th ++ "user") user
     I.package (threadName th ++ "gen") gen
--     ]

mkMonitorCode :: Monitor -> MonitorCode -> I.Module
mkMonitorCode m
  MonitorCode { monitorcode_moddef = code }
  = I.package (monitorName m) code

-- mkSignalCode :: String -> SignalCode -> [I.Module]
-- mkSignalCode s
--   SignalCode { signalcode_moddef = code }
--   = map (I.package (signalName s)) [code]
