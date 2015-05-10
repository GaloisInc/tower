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

data Config = Config
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
  , configIvoryOpts   :: C.Opts
  -- ^ Ivory options
  , configArtifacts   :: [A.Artifact]
  -- ^ Artifacts to generate
  }

-- | Overwrite Ivory options.
setConfigIvoryOpts :: C.Opts -> Config -> Config
setConfigIvoryOpts opts c =
  c { configIvoryOpts = opts }

-- | Appends AADL artifacts.
addAadlArtifacts :: [A.Artifact] ->Config -> Config
addAadlArtifacts artifacts c =
  c { configArtifacts = configArtifacts c ++ artifacts }

initialConfig :: Config
initialConfig = Config
  { configSrcsDir     = "user_code"
  , configHdrDir      = "include"
  , configSystemName  = "sys"
  , configSystemOS    = CAmkES
  , configSystemHW    = QEMU
  , configIvoryOpts   = C.initialOpts
  , configArtifacts   = []
  }
