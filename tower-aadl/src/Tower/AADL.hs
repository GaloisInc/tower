--
-- Top-level driver for AADL generation.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL
  ( compileAADL
  , runCompileAADL
  -- command line options
  , Opts(..)
  , initialOpts
  , parseOpts
  -- configuration
  , Config(..)
  , initialConfig
  , appendArtifacts
  , HW(..)
  , OS(..)
  ) where

import           Data.Maybe

import           System.IO (openFile, IOMode(..), hClose)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (addExtension,(</>))
import           System.Environment (getArgs)

import           Text.PrettyPrint.Leijen ( (<$$>), putDoc, hPutDoc, Doc, equals
                                         , linebreak, text, (<>)
                                         , punctuate, dquotes, hcat, comma
                                         )

import qualified Ivory.Compile.C.CmdlineFrontend as O

import           Ivory.Tower
import           Ivory.Tower.Types.Dependencies

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Artifact            as R

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST        as A
import qualified Tower.AADL.AST.Common as A
import           Tower.AADL.Build
import           Tower.AADL.CmdlineFrontend
import           Tower.AADL.CodeGen
import           Tower.AADL.Compile
import           Tower.AADL.Config
import           Tower.AADL.Render
import           Tower.AADL.Render.Common (typesPkg)
import           Tower.AADL.Render.Types

--------------------------------------------------------------------------------

compileAADL :: Tower () () -> IO ()
compileAADL t = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL opts t

-- | Compile full AADL packages.
runCompileAADL :: Opts -> Tower () () -> IO ()
runCompileAADL opts' t = do
  case genDirOpts opts of
    Nothing
      -> mapM_ (putDoc . (<$$> linebreak) . docImpl) docLst
    Just dir
      -> do createDirectoryIfMissing True dir
            mapM_ go docLst
            outputAADLDeps (dir </> aadlFilesMk)
                           (tyPkg ++ thdNames ++ [configSystemName c])
            genIvoryCode (ivoryOpts dir) code deps sigs
            wrFile ramsesMakefileName
                   (ramsesMakefile (configOpts opts'))
            wrFile kbuildName   (kbuild dir)
            wrFile kconfigName  (kconfig dir dir)
            wrFile makefileName (makefile dir)
            putArtifacts dir (configArtifacts c)
      where
      wrFile fName = writeFile (dir </> fName)
      go d = outputAADL dir (docName d) r
        where r = renderDocPkg (aTypesPkg docs) thdNames d

  where
  opts = validFPOpts opts'
  ivoryOpts dir =
    (configIvoryOpts c) { O.outDir    = Just (dir </> configSrcsDir c)
                        , O.outHdrDir = Just (dir </> configHdrDir  c)
                        -- XXX assuming that the only artifacts are headers.
                        , O.outArtDir = Just (dir </> configHdrDir  c)
                        , O.scErrors  = False }
  (ast, code, deps, sigs) = runTower AADLBackend t ()
  c             = configOpts opts
  sys           = fromTower c ast deps
  docs          = buildAADL anyTys strs sys
  docLst        = concatDocs docs
  -- Invariant: this list gives the dependency ordering for the files as well.
  thdNames      = map docName (thdDocs docs)
  -- types
  types         = A.extractTypes sys
  strs          = concatMap (I.public . I.modStructs) (dependencies_modules deps)
  anyTys        = any defType types
  tyPkg         = if anyTys then [typesPkg] else []

-- | Compile the types, threads, and system separately without building packages.
buildAADL :: Bool -> [I.Struct] -> A.System -> CompiledDocs
buildAADL anyTypes tys sys =
  (renderSystem sys) { tyDoc = typesDoc }
  where
  doc   = defineTypes types tys
  types = A.extractTypes sys
  typesDoc =
    if anyTypes
      then Just (compiledTypesDoc doc)
      else Nothing

outputAADL :: FilePath -> String -> Doc -> IO ()
outputAADL dir nm contents = do
  h <- openFile fname WriteMode
  hPutDoc h contents
  hClose h
  where
  fname = addExtension (dir </> nm) aadlExt

-- | Output a variable declaration with the AADL files generated, in order.
outputAADLDeps :: FilePath -> [String] -> IO ()
outputAADLDeps fp nms = do
  h <- openFile fp WriteMode
  hPutDoc h $ text "AADL_LIST" <> equals <> dquotes (hcat (punctuate comma files))
  hClose h
  where
  files = map (\nm -> text $ addExtension nm aadlExt) nms

aadlExt :: String
aadlExt = "aadl"

putArtifacts :: FilePath -> [R.Artifact] -> IO ()
putArtifacts fp as = do
  merrs <- mapM (R.putArtifact fp) as
  mapM_ putStrLn (catMaybes merrs)

--------------------------------------------------------------------------------
