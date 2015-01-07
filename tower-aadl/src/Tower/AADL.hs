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
runCompileAADL opts t =
  case genDirOpts opts of
    Nothing
      -> putDoc (header <$$> renderSystem fullSys <$$> empty)
    Just dir
      -> do createDirectoryIfMissing True dir
            if multiFile opts
              then do
                let (sys,thds) = A.decomposeThreads fullSys
                let thdDocs :: [(String, Doc)]
                    thdDocs = map (A.threadName &&& renderThread) thds
                mapM_ (uncurry (outputAADL dir)) thdDocs
                outSys sys
              else outSys fullSys
      where
      outSys sys = outputAADL dir (A.systemName sys) (renderSystem sys)
  where
  fullSys = mkSystem opts t

mkSystem :: Opts -> Tower () () -> A.System
mkSystem opts t = fromTower (configOpts opts) towerAST
  where
  towerAST = fst (runTower t ())

outputAADL :: FilePath -> String -> Doc -> IO ()
outputAADL dir nm contents = do
  h <- openFile fname WriteMode
  hPutDoc h contents
  hClose h
  where
  fname = addExtension (dir </> nm) ".aadl"

header :: Doc
header = renderStringComment "File generated from Tower-AADL compiler" <$$> empty
