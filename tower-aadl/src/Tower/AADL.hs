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

import           Control.Arrow
import           System.IO (openFile, IOMode(..), hClose)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (addExtension,(</>))
import           System.Environment (getArgs)

import           Text.PrettyPrint.Leijen ((<$$>), putDoc, hPutDoc, Doc, linebreak)

import           Ivory.Tower
import qualified Ivory.Tower.Types.GeneratedCode as C
import qualified Ivory.Language.Syntax.AST as I

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST as A
import           Tower.AADL.CmdlineFrontend
import           Tower.AADL.Render
import           Tower.AADL.Render.Types
import           Tower.AADL.Config

--------------------------------------------------------------------------------

compileAADL :: Tower () () -> IO ()
compileAADL t = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL opts t

-- | Compile full AADL packages.
runCompileAADL :: Opts -> Tower () () -> IO ()
runCompileAADL opts t =
  case genDirOpts opts of
    Nothing
      -> mapM_ (putDoc . (<$$> linebreak) . docImpl) docLst
    Just dir
      -> do createDirectoryIfMissing True dir
            mapM_ go docLst
      where
      go d = outputAADL dir (docName d) (renderDocPkg thdNames d)
  where
  docLst   = concatDocs docs
  thdNames = map docName (thdDocs docs)
  docs     = buildAADL opts t

-- |Compile the types, threads, and system separately without building packages.
buildAADL :: Opts -> Tower () () -> CompiledDocs
buildAADL opts t = cds { tyDoc = typesDoc sys strs }
  where
  cds = renderSystem sys
  -- Full system and code-gen structure implementations
  (sys, strs) = mkSystem opts t

  toCompDoc :: (String, Doc) -> CompiledDoc
  toCompDoc = uncurry (compiledDoc ThreadDoc)

-- | Compile user-defined types if there are any.
typesDoc :: A.System -> [I.Struct] -> Maybe CompiledDoc
typesDoc sys strs =
  if null types
    then Nothing
    else Just (compiledTypesDoc doc)
  where
  doc   = defineTypes (types, strs)
  types = A.extractTypes sys

mkSystem :: Opts -> Tower () () -> (A.System, [I.Struct])
mkSystem opts t =
  ( fromTower (configOpts opts) ast
  , concatMap (I.public . I.modStructs) (C.generatedcode_modules code)
  )
  where
  (ast,code) = runTower t ()

outputAADL :: FilePath -> String -> Doc -> IO ()
outputAADL dir nm contents = do
  h <- openFile fname WriteMode
  hPutDoc h contents
  hClose h
  where
  fname = addExtension (dir </> nm) ".aadl"

--------------------------------------------------------------------------------
