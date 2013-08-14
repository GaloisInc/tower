{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Tower.Frontend
  ( BuildConf(..)
  , defaultBuildConf
  , searchPathConf
  , compile

  , Twr(..)
  , compilePlatforms
  ) where

import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Control.Monad (when)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath

import Ivory.Language
import Ivory.Tower
import qualified Ivory.Opts.CFG as CFG
import qualified Ivory.Stdlib.SearchDir as Stdlib

import qualified Ivory.Compile.C.CmdlineFrontend as C
import qualified Ivory.Compile.C.CmdlineFrontend.Options as C
import qualified Ivory.Compile.AADL as A

import qualified Ivory.Tower.Graphviz         as T
import qualified Ivory.Tower.Frontend.Options as T
import qualified Ivory.Tower.Compile.FreeRTOS as FreeRTOS
import qualified Ivory.Tower.Compile.AADL     as AADL

data BuildConf =
  BuildConf
    { bc_sizemap :: Maybe CFG.SizeMap
    , bc_searchpath :: [IO FilePath]
    }

data Twr = forall p . Twr (Tower p ())

defaultBuildConf :: BuildConf
defaultBuildConf = BuildConf
  { bc_sizemap    = Nothing
  , bc_searchpath = [Stdlib.searchDir]
  }

searchPathConf :: [IO FilePath] -> BuildConf
searchPathConf p = defaultBuildConf { bc_searchpath = p }

compile :: BuildConf -> Tower p () -> IO ()
compile bc t = do
  (c_opts, t_opts) <- parseOptions =<< getArgs
  towerCompile (ivoryCompile bc c_opts) t_opts t

compilePlatforms :: BuildConf -> [(String, Twr)] -> IO ()
compilePlatforms bc table =  do
  (c_opts, t_opts) <- parseOptions =<< getArgs
  case lookup (T.conf_platform t_opts) table of
    Just (Twr t) -> towerCompile (ivoryCompile bc c_opts) t_opts t
    Nothing -> die msg
  where
  msg = "unsupported platform. the following platforms are supported:":(map fst table)

ivoryCompile :: BuildConf -> C.Opts -> [Module] -> [IO FilePath] -> IO ()
ivoryCompile bc copts ms platformspecific_sp = 
  C.runCompilerWith szmap spath ms copts
  where
  szmap = bc_sizemap bc
  spath = Just $ bc_searchpath bc ++ platformspecific_sp

towerCompile :: ([Module] -> [IO FilePath] -> IO ())
             -> T.Config
             -> Tower p ()
             -> IO ()
towerCompile compiler conf t = do
  case T.conf_os conf of
    "freertos" -> compileFreeRTOS compiler conf t
    "aadl"     -> compileAADL     compiler conf t
    o -> die [ "unsupported operating system " ++ o
             , "tower frontend supports: freertos, aadl"]

compileFreeRTOS :: ([Module] -> [IO FilePath] -> IO ())
                -> T.Config
                -> Tower p ()
                -> IO ()
compileFreeRTOS compiler conf t = do
  let (asm, objs) = FreeRTOS.compile t
  compiler objs [FreeRTOS.searchDir]
  compileDot conf asm

compileAADL :: ([Module] -> [IO FilePath] -> IO ())
            -> T.Config
            -> Tower p ()
            -> IO ()
compileAADL compiler conf t = do
  let (asm, objs) = AADL.compile t
  compiler objs searchpath
  compileDot            conf      asm
  compileAADLStructDefs conf objs
  compileAADLAssembly   conf objs asm
  where
  searchpath = []

compileDot :: T.Config -> Assembly -> IO ()
compileDot conf asm =
  when (T.conf_mkdot conf) $ T.graphvizToFile f asm
  where f = (T.conf_outdir conf) </> (T.conf_name conf) <.> "dot"

compileAADLStructDefs :: T.Config -> [Module] -> IO ()
compileAADLStructDefs conf mods =
  when (T.conf_mkmeta conf) $ mapM_ out aadlDocs
  where
  aadlDocs = catMaybes $ map compileWithCtx mods
  compileWithCtx = A.compileModule mods
  out d = A.documentToFile (fname d) d
  fname d = (T.conf_outdir conf) </> (A.doc_name d) <.> "aadl"

compileAADLAssembly :: T.Config -> [Module] -> Assembly -> IO ()
compileAADLAssembly conf mods asm = 
  when (T.conf_mkmeta conf) $ A.documentToFile fname d
  where
  packagename = "somename"
  d = AADL.assemblyDoc packagename mods asm
  fname = (T.conf_outdir conf) </> (A.doc_name d) <.> "aadl"

parseOptions :: [String] -> IO (C.Opts, T.Config)
parseOptions s = case (e1, e2) of
  ([],[]) -> case tfunmatched ++ cfunmatched ++ cfunrecog of
    [] -> case (mconcat cfs, mconcat tfs) of
      (C.Success cf, C.Success tf) -> do
        let c_opts = cf C.initialOpts
            t_opts = tf T.initialOpts
        toconf <- T.optsToConfig t_opts c_opts
        case toconf of
          Right t_conf -> return (c_opts, t_conf)
          Left e -> die [e]
      _ -> die ["impossible failure concatinating parsed options"]
    unmatched -> die ("Unknown options:":unmatched)
  _ -> die (e1 ++ e2)
  where
  (tfs,tfunmatched,tfunrecog,e1) = getOpt' Permute T.options s
  (cfs,cfunmatched,cfunrecog,e2) = getOpt' Permute C.options tfunrecog

die :: [String] -> IO a
die errs = do
  prog <- getProgName
  let banner = unlines (errs ++ ["", "Usage: " ++ prog ++ " [OPTIONS]"])
  putStrLn (usageInfo banner T.options)
  putStrLn (usageInfo "" C.options)
  exitFailure

