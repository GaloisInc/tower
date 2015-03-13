{-# LANGUAGE RecordWildCards #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Config where

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
