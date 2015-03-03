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
  -- ^ Location of/to put C sources.
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
  { configSrcsDir     = ""
  , configSystemName  = "system"
  , configSystemOS    = "OS"
  , configSystemHW    = "HW"
  , configIvoryOpts   = C.initialOpts
  }

