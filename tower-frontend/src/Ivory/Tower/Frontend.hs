{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Tower.Frontend
  ( BuildConf(..)
  , defaultBuildConf
  , searchPathConf
  , compile
  , compile'
  , Twr(..)
  , compilePlatforms
  , compilePlatforms' -- ^ No getArgs call.
  ) where

import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Control.Monad (when, void)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Map as Map

import           Ivory.Language
import qualified Ivory.Opts.CFG as CFG
import qualified Ivory.Stdlib.SearchDir as Stdlib

import qualified Ivory.Compile.C.CmdlineFrontend as C
import qualified Ivory.Compile.C.CmdlineFrontend.Options as C
import qualified Ivory.Compile.AADL as A

import           Ivory.Tower
import           Ivory.Tower.Types.OS (OS)
import qualified Ivory.Tower.AST                   as AST
import qualified Ivory.Tower.Compile               as Tower

import qualified Ivory.Tower.Compile.FreeRTOS      as FreeRTOS
import qualified Ivory.Tower.Compile.AADL          as AADL

import qualified Ivory.Tower.Reporting.Graphviz    as T
import qualified Ivory.Tower.Reporting.Entrypoints as T

import qualified Ivory.Tower.Frontend.Options      as T

data BuildConf =
  BuildConf
    { bc_sizemap :: Maybe CFG.SizeMap
    , bc_searchpath :: [IO FilePath]
    }

data Twr = forall p . (Signalable p) => Twr (Tower p ())

defaultBuildConf :: BuildConf
defaultBuildConf = BuildConf
  { bc_sizemap    = Nothing
  , bc_searchpath = [Stdlib.searchDir]
  }

searchPathConf :: [IO FilePath] -> BuildConf
searchPathConf p = defaultBuildConf { bc_searchpath = p }

compile :: (Signalable p) => BuildConf -> Tower p () -> IO ()
compile bc t = do
  (c_opts, t_opts) <- parseOptions =<< getArgs
  compile' bc t_opts c_opts t

compile' :: Signalable p => BuildConf -> T.Config -> C.Opts -> Tower p () -> IO ()
compile' bc t_opts c_opts t =
  towerCompile bc c_opts t_opts t

compilePlatforms :: BuildConf -> [(String, Twr)] -> IO ()
compilePlatforms bc table = compilePlatforms' bc table =<< getArgs

compilePlatforms' :: BuildConf
                  -> [(String, Twr)]
                  -> [String]
                  -> IO ()
compilePlatforms' bc table opts =  do
  (c_opts, t_opts) <- parseOptions opts
  case lookup (T.conf_platform t_opts) table of
    Just (Twr t) -> towerCompile bc c_opts t_opts t
    Nothing -> die (msg (T.conf_platform t_opts))
  where
  msg p = ("unsupported platform \"" ++ p ++ "\"."):
    (" the following platforms are supported:"):(map fst table)

ivoryCompile :: BuildConf -> C.Opts -> [Module] -> [IO FilePath] -> IO C.ModuleFiles
ivoryCompile bc copts ms platformspecific_sp =
  void $ C.runCompilerWith szmap spath ms copts
  where
  szmap = bc_sizemap bc
  spath = Just $ bc_searchpath bc ++ platformspecific_sp

towerCompile :: Signalable p
             => BuildConf
             -> C.Opts
             -> T.Config
             -> Tower p ()
             -> IO ()
towerCompile bc c_opts conf t = do
  os <- selectos
  let (sysast, objs, artifacts) = Tower.compile t os
  mfs <- ivoryCompile bc c_opts objs [FreeRTOS.searchDir]

  C.compileDepFile c_opts (artifactDeps conf artifacts (C.standaloneDepFile mfs))

  writeArtifacts conf artifacts
  compileDot conf sysast
  compileEntrypointList conf sysast
  when (T.conf_os conf == "aadl") $
    compileAADLDocuments conf objs sysast
  where
  selectos :: IO OS
  selectos = case T.conf_os conf of
    "freertos" -> return FreeRTOS.os
    "aadl"     -> return AADL.os
    o -> die [ "unsupported operating system " ++ o
             , "tower frontend supports: freertos, aadl"]


artifactDeps :: T.Config -> [Artifact] -> [(String,[String])] -> [(String,[String])]
artifactDeps conf as ds = Map.toList $ foldl aux (Map.fromList ds) as
  where
  aux d a = case artifact_tag a of
    "" -> addtoartifacts d
    t  -> addkv t path $ addtoartifacts d
    where path = (T.conf_outdir conf) </> (artifact_filepath a)
          addtoartifacts = addkv "ARTIFACTS" path
  addkv k v m = Map.alter addv k m
    where addv Nothing   = Just [v]
          addv (Just vs) = Just (v:vs)

writeArtifacts :: T.Config -> [Artifact] -> IO ()
writeArtifacts conf as =
  when (T.conf_mkdot conf) $ mapM_ wf as
  where
  wf a = writeFile path (artifact_contents a)
    where
    path = (T.conf_outdir conf) </> (artifact_filepath a)

compileDot :: T.Config -> AST.System p -> IO ()
compileDot conf ast =
  when (T.conf_mkdot conf) $ T.graphvizToFile f ast
  where f = (T.conf_outdir conf) </> (T.conf_name conf) <.> "dot"

compileEntrypointList :: T.Config -> AST.System p -> IO ()
compileEntrypointList conf ast = T.entrypointsToFile f nm ast
  where
  f = T.conf_outdir conf </> (nm ++ "_entrypoints") <.> "mk"
  nm = T.conf_name conf

compileAADLDocuments :: T.Config -> [Module] -> AST.System p -> IO ()
compileAADLDocuments conf mods sysast =
  when (T.conf_mkmeta conf) $ do
    mapM_ (writeAADLDoc conf) (typedoc : docs)
    A.warningsToFile warningfname (concat ws)
  where
  compilectx = A.compileModule mods
  ((docs, ws, dname),typedoc) = A.compileTypeCtx $ do
    (moddocs, modwss) <- (unzip . catMaybes) `fmap` (mapM compilectx mods)
    (sysdoc, sysws) <- AADL.systemDoc (T.conf_name conf) mods sysast
    return (sysdoc:moddocs, sysws:modwss, A.doc_name sysdoc)
  warningfname = (T.conf_outdir conf) </> dname ++ "_warnings" <.> "txt"

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
  (tfs, _,  tfunrecog,  err1) = getOpt' Permute T.options s
  (cfs, _, _pcfunrecog, err2) = getOpt' Permute C.options tfunrecog

die :: [String] -> IO a
die errs = do
  prog <- getProgName
  let banner = unlines (errs ++ [ "", "Tower compile usage: " ++ prog
                               ++ " [OPTIONS]" ])
  putStrLn (usageInfo banner T.options)
  putStrLn (usageInfo "" C.options)
  exitFailure

