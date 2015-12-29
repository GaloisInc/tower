{-# LANGUAGE RecordWildCards #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Config where

import           Data.List
import           Data.Char (toUpper)
import           Ivory.Tower.Options (TOpts(..))
import           Ivory.Tower.Config
import qualified System.Console.GetOpt as O

import           Tower.AADL.Priorities (PriorityMap, emptyPriorityMap)

----------------------------------------

data HW =
    QEMU
  | ODROID
  | PIXHAWK
  deriving (Show, Read, Eq)

data OS =
    CAmkES
  | EChronos
  deriving (Show, Read, Eq)

data AADLConfig = AADLConfig
  { configSrcsDir        :: FilePath
  -- ^ Location of/to put C sources relative to genDirOpts.
  , configHdrDir         :: FilePath
  -- ^ Location of/to put C headers relative to genDirOpts.
  , configLibDir         :: FilePath
  -- ^ Location of/to put C lib sources relative to genDirOpts.
  , configSystemName     :: String
  -- ^ System name.
  , configSystemOS       :: OS
  -- ^ Operating system name.
  , configSystemHW       :: HW
  -- ^ HW name.
  , configPriorities     :: PriorityMap
  -- ^ Priorities for threads.
  , configCustomMakefile :: Bool
  -- ^ If True, user provides custom Makefile.
  , configCustomKConfig  :: Bool
  -- ^ If True, user provides custom Kconfig, Kbuild.
  }
  deriving (Show)

defaultAADLConfig :: AADLConfig
defaultAADLConfig = AADLConfig
  { configSrcsDir         = "user_code"
  , configHdrDir          = "include"
  , configLibDir          = "smaccmpilot"
  , configSystemName      = "sys"
  , configSystemOS        = CAmkES
  , configSystemHW        = QEMU
  , configPriorities      = emptyPriorityMap
  , configCustomMakefile  = False
  , configCustomKConfig   = False
  }

lib :: AADLConfig -> String
lib c = "lib" ++ configLibDir c

aadlConfigParser :: AADLConfig -> ConfigParser AADLConfig
aadlConfigParser dflt = subsection "aadl" p `withDefault` dflt
  where
  p = do
    os <- (subsection "os" osParser) `withDefault` (configSystemOS dflt)
    hw <- (subsection "os" hwParser) `withDefault` (configSystemHW dflt)
    return dflt { configSystemOS = os, configSystemHW = hw }
  osParser = string >>= \s ->
    case map toUpper s of
      "CAMKES"   -> return CAmkES
      "ECHRONOS" -> return EChronos
      _ -> fail ("expected AADL OS, got " ++ s)
  hwParser = string >>= \s ->
    case map toUpper s of
      "QEMU"    -> return QEMU
      "ODROID"  -> return ODROID
      "PIXHAWK" -> return PIXHAWK
      _ -> fail ("expected AADL HW Platform, got " ++ s)

----------------------------------------
-- Additional command line opts

data Flag = LibDir String
  deriving (Show, Read, Eq)

options :: [O.OptDescr Flag]
options =
  [ O.Option "" ["lib-dir"] (O.ReqArg LibDir "DIR") "library directory name" ]

parseAADLOpts :: AADLConfig -> TOpts -> (AADLConfig, [String], [String])
parseAADLOpts c topts = (foldl' go c flags, nonOpts, errs)
  where
  (flags, nonOpts, errs) = O.getOpt O.Permute options (topts_args topts)
  go c' (LibDir dir) = c' { configLibDir = dir }
