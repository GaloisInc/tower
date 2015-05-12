{-# LANGUAGE RecordWildCards #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Config where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import qualified Ivory.Artifact as A

--------------------------------------------------------------------------------

data HW =
    QEMU
  | ODROID
  deriving (Show, Read, Eq)

data OS =
  CAmkES
  deriving (Show, Read, Eq)

data AADLConfig = AADLConfig
  { configSrcsDir     :: FilePath
  -- ^ Location of/to put C sources relative to genDirOpts.
  , configHdrDir      :: FilePath
  -- ^ Location of/to put C headers relative to genDirOpts.
  , configSystemName  :: String
  -- ^ System name.
  , configSystemOS    :: OS
  -- ^ Operating system name.
  , configSystemHW    :: HW
  -- ^ HW name.
  }

initialAADLConfig :: AADLConfig
initialAADLConfig = AADLConfig
  { configSrcsDir     = "user_code"
  , configHdrDir      = "include"
  , configSystemName  = "sys"
  , configSystemOS    = CAmkES
  , configSystemHW    = QEMU
  }
