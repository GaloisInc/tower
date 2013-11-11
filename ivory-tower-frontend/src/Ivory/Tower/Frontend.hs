{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Tower.Frontend
  ( BuildConf(..)
  , defaultBuildConf
  , searchPathConf
  , compile
  , Twr(..)
  , compilePlatforms
  , compilePlatforms' -- ^ No getArgs call.
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

import qualified Ivory.Tower.Reporting.Graphviz    as T
import qualified Ivory.Tower.Reporting.Entrypoints as T
import qualified Ivory.Tower.Frontend.Options      as T
import qualified Ivory.Tower.Compile.FreeRTOS      as FreeRTOS
import qualified Ivory.Tower.Compile.AADL          as AADL

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
compilePlatforms bc table = compilePlatforms' bc table =<< getArgs

compilePlatforms' :: BuildConf
                  -> [(String, Twr)]
                  -> [String]
                  -> IO ()
compilePlatforms' bc table opts =  do
  (c_opts, t_opts) <- parseOptions opts
  case lookup (T.conf_platform t_opts) table of
    Just (Twr t) -> towerCompile (ivoryCompile bc c_opts) t_opts t
    Nothing -> die (msg (T.conf_platform t_opts))
  where
  msg p = ("unsupported platform \"" ++ p ++ "\"."):
    (" the following platforms are supported:"):(map fst table)

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
  compileEntrypointList conf asm

compileAADL :: ([Module] -> [IO FilePath] -> IO ())
            -> T.Config
            -> Tower p ()
            -> IO ()
compileAADL compiler conf t = do
  let (asm, objs) = AADL.compile t
  compiler objs [AADL.searchDir]
  compileDot            conf      asm
  compileAADLDocuments  conf objs asm

compileDot :: T.Config -> Assembly -> IO ()
compileDot conf asm =
  when (T.conf_mkdot conf) $ T.graphvizToFile f asm
  where f = (T.conf_outdir conf) </> (T.conf_name conf) <.> "dot"

compileEntrypointList :: T.Config -> Assembly -> IO ()
compileEntrypointList conf asm =
  when (T.conf_mkdot conf) $ T.entrypointsToFile f asm
  where
  f = (T.conf_outdir conf) </> ((T.conf_name conf) ++ "_entrypoints") <.> "txt"

compileAADLDocuments :: T.Config -> [Module] -> Assembly -> IO ()
compileAADLDocuments conf mods asm =
  when (T.conf_mkmeta conf) $ mapM_ (writeAADLDoc conf) (typedoc : docs)
  where
  compile = A.compileModule mods
  ((docs, ws),typedoc) = A.compileTypeCtx $ do
    (moddocs, modwss) <- (unzip . catMaybes) `fmap` (mapM compile mods)
    (asmdoc, asmws) <- AADL.assemblyDoc (T.conf_name conf) mods asm
    return (asmdoc:moddocs, asmws:modwss)

writeAADLDoc :: T.Config -> A.Document -> IO ()
writeAADLDoc conf d = A.documentToFile fname d
  where fname = (T.conf_outdir conf) </> (A.doc_name d) <.> "aadl"

parseOptions :: [String] -> IO (C.Opts, T.Config)
parseOptions s =
  case err1 ++ err2 of
    -- No errors
    []   -> case (mconcat cfs, mconcat tfs) of
              (C.Success cf, C.Success tf) -> do
                let c_opts = cf C.initialOpts
                let t_opts = tf T.initialOpts
                toconf <- T.optsToConfig t_opts c_opts
                case toconf of
                  Right t_conf -> return (c_opts, t_conf)
                  Left e       -> die [e]
              _ -> die [ "Tower-compile: impossible failure concatinating "
                      ++ "parsed options" ]
    errs -> die errs
  where
  (tfs, _, tfunrecog,  err1) = getOpt' Permute T.options s
  (cfs, _, pcfunrecog, err2) = getOpt' Permute C.options tfunrecog

die :: [String] -> IO a
die errs = do
  prog <- getProgName
  let banner = unlines (errs ++ [ "", "Tower compile usage: " ++ prog
                               ++ " [OPTIONS]" ])
  putStrLn (usageInfo banner T.options)
  putStrLn (usageInfo "" C.options)
  exitFailure

