{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
--
-- Top-level driver for AADL generation.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL
  ( compileTowerAADL
  , compileTowerAADLForPlatform
  -- configuration
  , AADLConfig(..)
  , defaultAADLConfig
  , aadlConfigParser
  , HW(..)
  , OS(..)
  ) where

import           Data.Maybe
import           Data.List
import           Data.Char
import           Control.Monad hiding (forever)

import           System.FilePath (addExtension, takeFileName, (<.>))
import           System.Exit (exitFailure)

import           Text.PrettyPrint.Leijen hiding ((</>))

import qualified Ivory.Compile.C.CmdlineFrontend as O
import qualified Ivory.Compile.C.Types as O

import           Ivory.Tower
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.AST.Graph (graphviz, messageGraph)
import           Ivory.Tower.Options
import           Ivory.Tower.Types.Dependencies

import qualified Ivory.Language.Syntax     as I
import           Ivory.Artifact

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST        as A
import qualified Tower.AADL.AST.Common as A
import           Tower.AADL.Build.Common ( aadlFilesMk
                                         , OSSpecific(..) )
import qualified Tower.AADL.Build.SeL4     as SeL4 ( defaultCAmkESOS )
import           Tower.AADL.CodeGen
import           Tower.AADL.Compile
import           Tower.AADL.Config
import           Tower.AADL.Render
import           Tower.AADL.Render.Types
import           Tower.AADL.Platform

import qualified Ivory.Tower.AST as A

--------------------------------------------------------------------------------

graphvizArtifact :: String -> AST.Tower -> Located Artifact
graphvizArtifact appname ast = Root $
  artifactString (appname <.> "dot") (graphviz $ messageGraph ast)

-- | Only use this version if you're compiling for SeL4/CAmkES. If you're
-- building for a different platform use the more general
-- `compileTowerAADLForPlatform`
compileTowerAADL :: (e -> AADLConfig) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerAADL fromEnv = compileTowerAADLForPlatform ((,SeL4.defaultCAmkESOS) . fromEnv)

compileTowerAADLForPlatform :: (e -> (AADLConfig, OSSpecific a e))
                            -> (TOpts -> IO e)
                            -> Tower e ()
                            -> IO ()
compileTowerAADLForPlatform fromEnv mkEnv twr' = do
  (copts, topts)              <- towerGetOpts
  env                         <- mkEnv topts
  let (cfg',osspecific)       =  fromEnv env
  cfg                         <- parseAADLOpts' cfg' topts
  let twr                     =  twr' >> osSpecificTower osspecific
  (ast, code, deps, sigs) <-  runTower AADLBackend twr env []
  let missingCallbacks = handlersMissingCallbacks ast
  when (not (null missingCallbacks)) $ do
    putStrLn "Error: The following handlers are missing callbacks:"
    mapM_ putStrLn (map (showUnique . A.handler_name) missingCallbacks)
    exitFailure
  let aadl_sys                =  fromTower cfg ast
  let aadl_docs               =  buildAADL deps aadl_sys
  let doc_as                  =  renderCompiledDocs aadl_docs
  let deps_a                  =  aadlDepsArtifact $ aadlDocNames aadl_docs
                                                 ++ [ configSystemName cfg ]
  let (pkgs, mods, genAs)     = genIvoryCode code deps sigs

  let libAs                   = map (osSpecificSrcDir osspecific cfg) genAs

  let appname                 = takeFileName (fromMaybe "tower" (O.outDir copts))

  let as :: OSSpecific a e -> [Located Artifact]
      as os = doc_as
        ++ libAs
        ++ [ Root deps_a
           , graphvizArtifact appname ast
           ]
        ++ osSpecificArtifacts os appname cfg

  unless (validCIdent appname) $ error $ "appname must be valid c identifier; '"
                                        ++ appname ++ "' is not"
  cmodules <- O.compileUnits mods copts
  let (appMods, libMods) =
        partition (\m -> O.unitName m `elem` pkgs) cmodules
  O.outputCompiler appMods (as osspecific) (osSpecificOptsApps osspecific cfg copts)
  O.outputCompiler libMods []              (osSpecificOptsLibs osspecific cfg copts)
  where

  -- | AADL assumes that our handlers will always have a callback define. So we
  -- search the Tower AST looking for handlers that missing callbacks.
  handlersMissingCallbacks :: A.Tower -> [A.Handler]
  handlersMissingCallbacks = concatMap monitorHasEmptyHandler . A.tower_monitors
    where
    monitorHasEmptyHandler :: A.Monitor -> [A.Handler]
    monitorHasEmptyHandler = catMaybes . map handlerIsEmpty . A.monitor_handlers
    handlerIsEmpty :: A.Handler -> Maybe A.Handler
    handlerIsEmpty h = if (null (A.handler_callbacks h))
                          then Just h
                          else Nothing

-- | parseAADLOpts' is a wrapper around parseAADLOpts that
-- checks for errors and if '--help' was requested.
-- It calls exitFailure in the case of errors or help.
parseAADLOpts' :: AADLConfig -> TOpts -> IO AADLConfig
parseAADLOpts' cfg topts =
  case parseAADLOpts cfg topts of
  (c, [],        _) -> do
    case topts_help topts of
      True  -> topts_error topts "Usage:"
      False -> return ()
    return c
  (_, _, _) -> finalizeOpts topts >> exitFailure

validCIdent :: String -> Bool
validCIdent appname =
  case appname of
    [] -> False
    (a:as) -> (isAlpha a || isUnderscore a)
           && all (\c -> isAlphaNum c || isUnderscore c) as
  where
  isUnderscore a = a == '_'

-- | Compile the types, threads, and system separately without building packages.
buildAADL :: Dependencies -> A.System -> CompiledDocs
buildAADL deps sys = (renderSystem sys) { tyDoc = typesDoc }
  where
  types     = A.extractTypes sys
  tydefs    = defineTypes types istructs
  istructs  = concatMap strHdrs (dependencies_modules deps)
  strHdrs m = map (\s -> (I.modName m `addExtension` "h", s))
                  (I.public $ I.modStructs m)
  typesDoc  =
    if any defType types
      then Just (compiledTypesDoc tydefs)
      else Nothing

aadlDocNames :: CompiledDocs -> [String]
aadlDocNames docs = map docName $
  maybeToList (tyDoc docs) ++ thdDocs docs

aadlDepsArtifact :: [String] -> Artifact
aadlDepsArtifact names = artifactString aadlFilesMk $ displayS pp ""
  where
  pp = renderPretty 0.4 100 doc
  doc = text "AADL_LIST" <> equals <> dquotes (hcat (punctuate comma files))
  files = map (\nm -> text $ addExtension nm "aadl") names

