{-# LANGUAGE RecordWildCards #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Config where

import Data.Char (toUpper)
import Ivory.Tower.Config

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

defaultAADLConfig :: AADLConfig
defaultAADLConfig = AADLConfig
  { configSrcsDir     = "user_code"
  , configHdrDir      = "include"
  , configSystemName  = "sys"
  , configSystemOS    = CAmkES
  , configSystemHW    = QEMU
  }

aadlConfigParser :: AADLConfig -> ConfigParser AADLConfig
aadlConfigParser dflt = subsection "aadl" p `withDefault` dflt
  where
  p = do
    os <- (subsection "os" osParser) `withDefault` (configSystemOS dflt)
    hw <- (subsection "os" hwParser) `withDefault` (configSystemHW dflt)
    return dflt { configSystemOS = os, configSystemHW = hw }
  osParser = string >>= \s ->
    case map toUpper s of
      "CAMKES" -> return CAmkES
      _ -> fail ("expected AADL OS, got " ++ s)
  hwParser = string >>= \s ->
    case map toUpper s of
      "QEMU"   -> return QEMU
      "ODROID" -> return ODROID
      _ -> fail ("expected AADL HW Platform, got " ++ s)

