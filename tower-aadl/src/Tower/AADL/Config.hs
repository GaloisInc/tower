{-# LANGUAGE RecordWildCards #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Config where

--------------------------------------------------------------------------------

data Config = Config
  { configSrcsDir     :: FilePath
  -- ^ Directory for output files (or standard out).
  , configSystemName  :: String
  -- ^ System name.
  , configSystemOS    :: String
  -- ^ Operating system name.
  , configSystemHW    :: String
  -- ^ HW name.
  } deriving (Show, Read, Eq)

initialConfig :: Config
initialConfig = Config
  { configSrcsDir     = ""
  , configSystemName  = "system"
  , configSystemOS    = "OS"
  , configSystemHW    = "HW"
  }

