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
  , compileTowerAADLForPlatformWithOpts
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
import           Ivory.Tower.Backend
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
                                         , OSSpecific(..)
                                         , aadlDocNames )
import qualified Tower.AADL.Build.SeL4     as SeL4 ( defaultCAmkESOS )
import           Tower.AADL.CodeGen
import           Tower.AADL.Compile
import           Tower.AADL.Config
import           Tower.AADL.Render
import           Tower.AADL.Render.Types
import           Tower.AADL.Platform

--------------------------------------------------------------------------------

graphvizArtifact :: String -> AST.Tower -> Located Artifact
graphvizArtifact appname ast = Root $
  artifactString (appname <.> "dot") (graphviz $ messageGraph ast)

-- | Only use this version if you're compiling for SeL4/CAmkES. If you're
-- building for a different platform use the more general
-- `compileTowerAADLForPlatform`
compileTowerAADL :: (e -> AADLConfig) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerAADL fromEnv = compileTowerAADLForPlatform ((,SeL4.defaultCAmkESOS) . fromEnv)

compileTowerAADLForPlatform :: (e -> (AADLConfig, OSSpecific a))
                            -> (TOpts -> IO e)
                            -> Tower e ()
                            -> IO ()
compileTowerAADLForPlatform fromEnv mkEnv twr' = compileTowerAADLForPlatformWithOpts fromEnv mkEnv twr' []

compileTowerAADLForPlatformWithOpts :: (e -> (AADLConfig, OSSpecific a))
                            -> (TOpts -> IO e)
                            -> Tower e ()
                            -> [AST.Tower -> IO AST.Tower]
                            -> IO ()
compileTowerAADLForPlatformWithOpts fromEnv mkEnv twr' optslist = do
  (copts, topts)              <- towerGetOpts
  env                         <- mkEnv topts
  let (cfg',osspecific)       =  fromEnv env
  cfg                         <- parseAADLOpts' cfg' topts
  let twr                     =  twr' >> osSpecificTower osspecific
  (ast, _monitors, deps, sigs) <-  runTower AADLBackend twr env optslist
  let code = towerImpl AADLBackend ast (map (monitorImplTD ast) $ AST.tower_monitors ast)
  let aadl_sys                =  fromTower cfg ast
  let aadl_docs               =  buildAADL deps aadl_sys
  let doc_as                  =  renderCompiledDocs aadl_docs
  let deps_a                  =  aadlDepsArtifact $ aadlDocNames aadl_docs
                                                 ++ [ configSystemName cfg ]
  let (pkgs, mods, genAs)     = genIvoryCode code deps sigs

  let libAs                   = map (osSpecificSrcDir osspecific cfg) genAs

  let appname                 = takeFileName (fromMaybe "tower" (O.outDir copts))

  let as :: OSSpecific a -> [Located Artifact]
      as os = doc_as
        ++ libAs
        ++ [ Root deps_a
           , graphvizArtifact appname ast
           ]
        ++ osSpecificArtifacts os appname cfg (aadlDocNames aadl_docs)

  unless (validCIdent appname) $ error $ "appname must be valid c identifier; '"
                                        ++ appname ++ "' is not"
  cmodules <- O.compileUnits mods copts
  let (appMods, libMods) =
        partition (\m -> O.unitName m `elem` pkgs) cmodules
  O.outputCompiler appMods (as osspecific) (osSpecificOptsApps osspecific cfg copts)
  O.outputCompiler libMods []              (osSpecificOptsLibs osspecific cfg copts)


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

aadlDepsArtifact :: [String] -> Artifact
aadlDepsArtifact names = artifactString aadlFilesMk $ displayS pp ""
  where
  pp = renderPretty 0.4 100 doc
  doc = text "AADL_LIST" <> equals <> dquotes (hcat (punctuate comma files))
  files = map (\nm -> text $ addExtension nm "aadl") names

