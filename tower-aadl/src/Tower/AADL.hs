--
-- Top-level driver for AADL generation.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL
  ( compileTowerAADL
  -- configuration
  , AADLConfig(..)
  , defaultAADLConfig
  , aadlConfigParser
  , HW(..)
  , OS(..)
  ) where

import           Data.Maybe
import           Data.Char
import           Control.Monad

import           System.FilePath (takeFileName, addExtension, (</>))

import           Text.PrettyPrint.Leijen hiding ((</>))

import qualified Ivory.Compile.C.CmdlineFrontend as O

import           Ivory.Tower
import           Ivory.Tower.Options
import           Ivory.Tower.Types.Dependencies

import qualified Ivory.Language.Syntax     as I
import           Ivory.Artifact

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST        as A
import qualified Tower.AADL.AST.Common as A
import           Tower.AADL.Build
import           Tower.AADL.CodeGen
import           Tower.AADL.Compile
import           Tower.AADL.Config
import           Tower.AADL.Render
import           Tower.AADL.Render.Types

--------------------------------------------------------------------------------

compileTowerAADL :: (e -> AADLConfig) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerAADL fromEnv mkEnv twr = do
  (copts, topts) <- towerGetOpts
  env <- mkEnv topts
  let cfg = fromEnv env
  let (ast, code, deps, sigs) = runTower AADLBackend twr env
  let aadl_sys  = fromTower cfg ast
  let aadl_docs = buildAADL deps aadl_sys
  let doc_as    = renderCompiledDocs aadl_docs
  let deps_a    = aadlDepsArtifact $ aadlDocNames aadl_docs
                                  ++ [ configSystemName cfg ]
  let (mods, modDeps, genAs) = genIvoryCode code deps sigs

  let libAs = map go genAs
        where
        go l = case l of
          Src a  -> Root (artifactPath (libSrcDir cfg) a)
          Incl a -> Root (artifactPath (libHdrDir cfg) a)
          _     -> l

  let appname = takeFileName $ fromMaybe "tower" $ O.outDir copts

  let as :: [Located Artifact]
      as = doc_as
        ++ libAs
        ++ map Root
           [ deps_a
           , artifactString ramsesMakefileName (ramsesMakefile cfg)
           , artifactString kbuildName         (kbuild appname)
           , artifactString kconfigName        (kconfig appname appname)
           , artifactString makefileName       (makefile appname)
           ]

  unless (validCIdent appname) $ error $ "appname must be valid c identifier; '"
                                        ++ appname ++ "' is not"
  O.runCompiler mods    as (ivoryOpts True  cfg copts)
  O.runCompiler modDeps [] (ivoryOpts False cfg copts)
  where

  libSrcDir cfg = configLibDir cfg </> "src"
  libHdrDir cfg = configLibDir cfg </> "include"

  ivoryOpts b cfg copts =
    copts { O.outDir    = Just (dir </> if b then configSrcsDir cfg
                                          else libSrcDir cfg)
          , O.outHdrDir = Just (dir </> if b then configHdrDir  cfg
                                          else libHdrDir cfg)
          , O.outArtDir = Just dir
          }
    where
    dir = fromMaybe "." (O.outDir copts)

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
  types    = A.extractTypes sys
  tydefs   = defineTypes types istructs
  istructs = concatMap (I.public . I.modStructs) (dependencies_modules deps)
  typesDoc = if any defType types
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

