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
import           Control.Monad
import           System.IO (openFile, IOMode(..), hClose)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (addExtension,(</>))
import           System.Environment (getArgs)

import           Text.PrettyPrint.Leijen ((<$$>), empty, putDoc, hPutDoc, Doc)

import           Ivory.Tower
import qualified Ivory.Tower.Types.GeneratedCode as C
import qualified Ivory.Language.Syntax.AST as I

import           Tower.AADL.FromTower
import qualified Tower.AADL.AST as A
import           Tower.AADL.CmdlineFrontend
import           Tower.AADL.Render
import           Tower.AADL.Render.Types
import           Tower.AADL.Render.Common (typesPkg)
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
      -> do putDoc $ mkTowerDoc $ renderSystem fullSys
            mkTypes $ putDoc $ mkTowerDoc renderTys
    Just dir
      -> do createDirectoryIfMissing True dir
            mkTypesPkg
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
      mkTypesPkg = mkTypes (outputAADL dir typesPkg renderTys)
  where
  (fullSys, strs) = mkSystem opts t
  types           = A.extractTypes fullSys
  mkTypes action  = unless (null types) action
  renderTys       = defineTypes (types, strs)

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

header :: Doc
header = renderStringComment "File generated from Tower-AADL compiler" <$$> empty

mkTowerDoc :: Doc -> Doc
mkTowerDoc doc = header <$$> doc <$$> empty
