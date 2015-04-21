{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Config where

import Data.Char
import System.FilePath (isPathSeparator)

import qualified Ivory.Compile.C.CmdlineFrontend as C

--------------------------------------------------------------------------------

data Config = Config
  { configSrcsDir     :: FilePath
  -- ^ Location of/to put C sources relative to genDirOpts.
  , configHdrDir      :: FilePath
  -- ^ Location of/to put C headers relative to genDirOpts.
  , configSystemName  :: String
  -- ^ System name.
  , configSystemOS    :: String
  -- ^ Operating system name.
  , configSystemHW    :: String
  -- ^ HW name.
  , configIvoryOpts   :: C.Opts
  -- ^ Ivory options
  } deriving Show

initialConfig :: Config
initialConfig = Config
  { configSrcsDir     = "user_code"
  , configHdrDir      = "include"
  , configSystemName  = "sys"
  , configSystemOS    = "CAmkES"
  , configSystemHW    = "QEMU"
  , configIvoryOpts   = C.initialOpts
  }

-- | Camkes needs filepaths, modulo '/', to be valid C identifiers.
validDirName :: FilePath -> Either String FilePath
validDirName fp =
  if | null fp
      -> Left "Empty filepath in AADL config."
     | and $ isAlpha (head fp) : fmap go (tail fp)
      -> Right fp
     | otherwise
      -> Left $ "Filepath " ++ fp
             ++ " must contain only valid C identifiers for Camkes."
  where
  -- A character is a C identifier char or a path separator.
  go c = isAlphaNum c
      || (c == '_')
      || isPathSeparator c
