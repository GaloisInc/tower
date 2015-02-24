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

import           Text.PrettyPrint.Leijen ((<$$>), putDoc, hPutDoc, Doc, linebreak)

import qualified Ivory.Compile.C.CmdlineFrontend as O

import           Ivory.Tower
import qualified Ivory.Tower.Types.GeneratedCode as C

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
      where
      go d = outputAADL dir (docName d) (renderDocPkg (aTypesPkg docs) thdNames d)

  genIvoryCode ivoryOpts code

  where
  ivoryOpts = O.initialOpts { O.outDir = genDirOpts opts }
  docLst    = concatDocs docs
  thdNames  = map docName (thdDocs docs)
  docs      = buildAADL opts sys code
  (ast, code) = runTower t ()
  sys = fromTower (configOpts opts) ast

-- | Compile the types, threads, and system separately without building packages.
buildAADL :: Opts -> A.System -> GeneratedCode  -> CompiledDocs
buildAADL opts sys code = cds { tyDoc = typesDoc sys types }
  where
  cds = renderSystem sys
  -- Full system and code-gen structure implementations
  -- (ast, code) = runTower t ()
  types       =
    concatMap (I.public . I.modStructs) (C.generatedcode_modules code)

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
  fname = addExtension (dir </> nm) ".aadl"

--------------------------------------------------------------------------------
