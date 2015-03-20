--
-- Top-level driver for AADL generation.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL
  ( compileAADL
  , runCompileAADL
  , Opts(..)
  , initialOpts
  , Config(..)
  , initialConfig
  ) where

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
import           Ivory.Tower.Backend.Compat
import           Ivory.Tower.Types.Dependencies

import qualified Ivory.Language.Syntax.AST       as I

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST        as A
import qualified Tower.AADL.AST.Common as A
import           Tower.AADL.CmdlineFrontend
import           Tower.AADL.CodeGen
import           Tower.AADL.Compile
import           Tower.AADL.Config
import           Tower.AADL.Render
import           Tower.AADL.Render.Types

--------------------------------------------------------------------------------

compileAADL :: Tower () () -> IO ()
compileAADL t = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL opts t

-- | Compile full AADL packages.
runCompileAADL :: Opts -> Tower () () -> IO ()
runCompileAADL opts t = do
  case genDirOpts opts of
    Nothing
      -> mapM_ (putDoc . (<$$> linebreak) . docImpl) docLst
    Just dir
      -> do createDirectoryIfMissing True dir
            mapM_ go docLst
            outputAADLDeps (dir </> "AADL_FILES")
                           (configSystemName c : thdNames)
            genIvoryCode (ivoryOpts dir) code deps sigs
      where
      go d = outputAADL dir (docName d) (renderDocPkg (aTypesPkg docs) thdNames d)

  where
  ivoryOpts dir =
    (configIvoryOpts c) { O.outDir    = Just (dir </> configSrcsDir c)
                        , O.outHdrDir = Just (dir </> configHdrDir  c)
                        -- XXX assuming that the only artifacts are headers.
                        , O.outArtDir = Just (dir </> configHdrDir  c)
                        , O.scErrors  = False }
  (ast, code, deps, sigs) = runTower CompatBackend t ()
  c             = configOpts opts
  sys           = fromTower c ast
  docs          = buildAADL sys deps
  docLst        = concatDocs docs
  -- Invariant: this list gives the dependency ordering for the files as well.
  thdNames      = map docName (thdDocs docs)

-- | Compile the types, threads, and system separately without building packages.
buildAADL :: A.System -> Dependencies  -> CompiledDocs
buildAADL sys deps = cds { tyDoc = typesDoc sys types }
  where
  cds   = renderSystem sys
  types = concatMap (I.public . I.modStructs) (dependencies_modules deps)

-- | Compile user-defined types if there are any.
typesDoc :: A.System -> [I.Struct] -> Maybe CompiledDoc
typesDoc sys strs =
  if any defType types
    then Just (compiledTypesDoc doc)
    else Nothing
  where
  doc   = defineTypes types strs
  types = A.extractTypes sys

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
  files = map (\nm -> text $ addExtension nm aadlExt) (reverse nms)

aadlExt :: String
aadlExt = "aadl"

--------------------------------------------------------------------------------
