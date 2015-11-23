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
import           Data.List
import           Data.Char
import           Control.Monad

import           System.FilePath (takeFileName, addExtension, (</>), (<.>))

import           Text.PrettyPrint.Leijen hiding ((</>))

import qualified Ivory.Compile.C.CmdlineFrontend as O
import qualified Ivory.Compile.C.Types as O

import           Ivory.Tower
import           Ivory.Tower.AST.Graph (graphviz, messageGraph)
import           Ivory.Tower.Options
import           Ivory.Tower.Types.Dependencies

import qualified Ivory.Language.Syntax     as I
import           Ivory.Artifact

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST        as A
import qualified Tower.AADL.AST.Common as A
import           Tower.AADL.Build.Common ( ramsesMakefileName, makefileName
                                         , componentLibsName, mkLib, aadlFilesMk
                                         , renderMkStmts )
import qualified Tower.AADL.Build.SeL4     as SeL4 ( ramsesMakefile, makefileApp
                                                   , kbuildName, kbuildLib
                                                   , kbuildApp, kconfigApp
                                                   , kconfigName, kconfigLib
                                                   , makefileLib)
import qualified Tower.AADL.Build.EChronos as EChronos
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
  let cfg' = fromEnv env
  let cfg  = parseAADLOpts cfg' topts
  let (ast, code, deps, sigs) = runTower AADLBackend twr env
  let os' = configSystemOS cfg
  let aadl_sys  = if os' == EChronos
                    then lowerCaseThreadNames (fromTower cfg ast)
                    else fromTower cfg ast
  let aadl_docs = buildAADL deps aadl_sys
  let doc_as    = renderCompiledDocs aadl_docs
  let deps_a    = aadlDepsArtifact $ aadlDocNames aadl_docs
                                  ++ [ configSystemName cfg ]
  let (pkgs, mods, genAs) = genIvoryCode code deps sigs

  let libAs = map go genAs
        where
        go l = case l of
          Src a  -> Root (artifactPath (libSrcDir cfg) a)
          Incl a -> Root (artifactPath (libHdrDir cfg) a)
          _      -> l

  let appname = takeFileName $ fromMaybe "tower" $ O.outDir copts

  let as :: OS -> [Located Artifact]
      as os = doc_as
        ++ libAs
        ++ map Root ls
        where
        ls :: [Artifact]
        ls =
           [ deps_a
           , artifactString ramsesMakefileName
                            (renderMkStmts (ramsesMakefile os))
           ] ++ osSpecific
           where
           osSpecific = case os of
             CAmkES ->
               (if configCustomKConfig cfg
                  then []
                  else [ artifactString SeL4.kbuildName
                          (renderMkStmts (SeL4.kbuildApp   l appname))
                       , artifactString SeL4.kconfigName
                          (SeL4.kconfigApp  appname appname)
                       ]) ++
               -- apps
               [ artifactString makefileName
                   (renderMkStmts (SeL4.makefileApp appname))
               , artifactString componentLibsName
                   (mkLib cfg (aadlDocNames aadl_docs))
               , artifactString (appname <.> "dot")
                   (graphviz $ messageGraph ast)
               ] ++
               -- libs
               map (artifactPath l)
                 [ artifactString SeL4.kbuildName
                     (renderMkStmts (SeL4.kbuildLib   l))
                 , artifactString SeL4.kconfigName
                     (SeL4.kconfigLib  appname l)
                 , artifactString makefileName
                     (renderMkStmts (SeL4.makefileLib cfg))
                 ]
             _ ->
               [ artifactString makefileName
                   (renderMkStmts EChronos.makefile) ]
           ramsesMakefile EChronos = EChronos.ramsesMakefile cfg
           ramsesMakefile CAmkES   = SeL4.ramsesMakefile     cfg
           l = lib cfg

  unless (validCIdent appname) $ error $ "appname must be valid c identifier; '"
                                        ++ appname ++ "' is not"
  cmodules <- O.compileUnits mods copts
  let (appMods, libMods) =
        partition (\m -> O.unitName m `elem` pkgs) cmodules
  O.outputCompiler appMods (as os') (ivoryOpts True  cfg copts)
  O.outputCompiler libMods []       (ivoryOpts False cfg copts)
  where

  libSrcDir cfg = lib cfg </> "src"
  libHdrDir cfg = lib cfg </> "include"

  -- True: app code, False: lib code
  ivoryOpts b cfg copts =
    copts { O.outDir    = Just (dir </> if b then configSrcsDir cfg
                                          else libSrcDir cfg)
          , O.outHdrDir = Just (dir </> if b then configHdrDir  cfg
                                          else libHdrDir cfg)
          , O.outArtDir = Just dir
          }
    where
    dir = fromMaybe "." (O.outDir copts)

-- The eChronos description (.prx) requires that the thread names be
-- lower cased. The only complication here is the tripple nesting of the
-- structure, we simply take apart the fields we need until we get to the
-- underlying threadName and then lowercase that.
lowerCaseThreadNames :: A.System -> A.System
lowerCaseThreadNames system = system { A.systemComponents = sc }
  where
  sc = map lowerProcessName (A.systemComponents system)

  -- Continue into the Process record
  lowerProcessName :: A.Process -> A.Process
  lowerProcessName process = process { A.processComponents = pc }
    where
    pc = map lowerThreadName (A.processComponents process)

    -- Finally we arrive at the Thread itself
    lowerThreadName :: A.Thread -> A.Thread
    lowerThreadName thread = thread { A.threadName = tn }
      where
      tn = map toLower (A.threadName thread)

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

