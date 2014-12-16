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

import           Text.PrettyPrint.Leijen ((<$$>), empty, putDoc, hPutDoc, Doc)

import           Ivory.Tower

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST as A
import           Tower.AADL.CmdlineFrontend
import           Tower.AADL.Render
import           Tower.AADL.Config

--------------------------------------------------------------------------------

compileAADL :: Tower () () -> IO ()
compileAADL t = do
  args <- getArgs
  opts <- parseOpts args
  runCompileAADL opts t

runCompileAADL :: Opts -> Tower () () -> IO ()
runCompileAADL opts t = do
  let towerAST = fst (runTower t ())
  let ast = fromTower (configOpts opts) towerAST
  let doc = renderSystem ast
  case genDirOpts opts of
    Nothing  -> putDoc (header <$$> doc <$$> empty)
    Just dir -> do createDirectoryIfMissing True dir
                   outputAADL dir (A.systemName ast) doc

outputAADL :: FilePath -> String -> Doc -> IO ()
outputAADL dir sys contents = do
  h <- openFile fname WriteMode
  hPutDoc h contents
  hClose h
  where
  fname = addExtension (dir </> sys) ".aadl"

header :: Doc
header = renderStringComment "File generated from Tower-AADL compiler" <$$> empty
