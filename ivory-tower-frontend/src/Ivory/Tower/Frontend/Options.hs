
module Ivory.Tower.Frontend.Options where

import qualified Ivory.Compile.C.CmdlineFrontend.Options as C

import Ivory.Compile.C.CmdlineFrontend.Options (OptParser(..), success)
import System.Console.GetOpt

-- XXX utility - are these in base? sorry you guys

newtype Id a = Id { unId :: a }

-- Two pass evaulation for getting arguments input, then processing them
-- for required / default cases
-- I might have completely screwed up this design, I ran up against this
-- problem and coded the quick and dirty solution. Sorry if this causes
-- distaste later. Pat

data OptsR a = OptsR
  { or_name     :: a String
  , or_platform :: a String
  , or_os       :: a String
  , or_mkdot    :: a Bool
  , or_outdir   :: a FilePath
  }

-- First we parse and fill in with the options we got
type Opts = OptsR Maybe
initialOpts :: Opts
initialOpts = OptsR
  { or_name      = Nothing
  , or_platform  = Nothing
  , or_os        = Nothing
  , or_mkdot     = Nothing
  , or_outdir    = Nothing
  }

-- Then we produce a completed record which has each value
type Config = OptsR Id

-- public accessors for Config:
conf_name :: Config -> String
conf_name = unId . or_name

conf_platform :: Config -> String
conf_platform = unId . or_platform

conf_os :: Config -> String
conf_os= unId . or_os

conf_mkdot :: Config -> Bool
conf_mkdot = unId . or_mkdot

conf_outdir :: Config -> FilePath
conf_outdir = unId . or_outdir

-- Options table:

options :: [OptDescr (OptParser Opts)]
options =
  [ Option "n" ["project"]          (ReqArg setName "STRING")
      "tower project name"
  , Option "p" ["platform"]         (ReqArg setPlatform "STRING")
      "tower platform name"
  , Option "s" ["operating-system"] (ReqArg setOS "STRING")
      "tower operating system name"
  , Option "d" ["dot"]              (NoArg (setMkDot True))
      "enable dot file output (default)"
  , Option "D" ["no-dot"]           (NoArg (setMkDot False))
      "disable dot file output"
  , Option ""  ["tower-outdir"]     (ReqArg setOutdir "STRING")
      "tower metadata output directory"
  ]
  where
  -- writers for the OptsR Maybe fields:
  setName :: String -> OptParser Opts
  setName s = success (\opts -> opts { or_name = Just s })
  setOS :: String -> OptParser Opts
  setOS s = success (\opts -> opts { or_os = Just s })
  setPlatform :: String -> OptParser Opts
  setPlatform s = success (\opts -> opts { or_platform = Just s })
  setMkDot :: Bool -> OptParser Opts
  setMkDot b = success (\opts -> opts { or_mkdot = Just b })
  setOutdir :: String -> OptParser Opts
  setOutdir s = success (\opts -> opts { or_outdir = Just s })

-- Process the incomplete Opts record into a complete Config record,
-- using the C backend options to fill in defaults OR throwing an error.
optsToConfig :: Opts -> C.Opts -> Either String Config
optsToConfig topts copts = do
  n <- required or_name "missing required option 'project'"
  p <- required or_platform "missing required option 'platform'"
  s <- required or_os "missing required option 'operating-system'"
  d <- case or_mkdot topts of
    Just v -> return v
    Nothing -> return True
  o <- case or_outdir topts of
    Just o -> return o
    Nothing -> return $ C.srcDir copts
  return $ OptsR
    { or_name = Id n
    , or_platform = Id p
    , or_os = Id s
    , or_mkdot = Id d
    , or_outdir = Id o
    }
 where
 required :: (Opts -> Maybe a) -> String -> Either String a
 required accessor errmsg =
   case accessor topts of
     Nothing -> fail errmsg
     Just a -> return a

