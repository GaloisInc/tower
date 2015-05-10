{-# LANGUAGE MultiWayIf #-}

--
-- Command line for AADL generator.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.CmdlineFrontend where

import Data.Monoid
import Data.Char

import System.Console.GetOpt
import System.Exit (exitFailure,exitSuccess)
import System.Environment (getProgName)
import System.FilePath (isPathSeparator)

--------------------------------------------------------------------------------
-- Option Parsing

data OptParser opt = OptParser [String] (opt -> opt)

instance Monoid (OptParser opt) where
  mempty = OptParser [] id
  -- left-to-right composition makes the last option parsed take precedence
  OptParser as f `mappend` OptParser bs g = OptParser (as ++ bs) (f . g)

-- | Option parser succeeded, use this function to transform the default
-- options.
success :: (opt -> opt) -> OptParser opt
success  = OptParser []

-- | Yield either a list of errors, or a function to produce an options
-- structure, given a set of default options.  Discard any non-options.
parseOptions :: [OptDescr (OptParser opt)] -> [String]
             -> (Either [String] (opt -> opt))
parseOptions opts args = case getOpt Permute opts args of
  (fs,[],[]) -> case mconcat fs of
    OptParser [] f -> Right f
    OptParser es _ -> Left es
  (_,_,es)  -> Left es

--------------------------------------------------------------------------------

data Opts = Opts
  { genDirOpts :: Maybe FilePath
  -- ^ Location to generate files (or use standard out).
  , helpOpts   :: Bool
  -- ^ Help.
  }

initialOpts :: Opts
initialOpts = Opts
  { genDirOpts = Just "tower_aadl_out"
  , helpOpts   = False
  }

--------------------------------------------------------------------------------

setGenDir :: FilePath -> OptParser Opts
setGenDir s = success (\opts -> opts { genDirOpts = Just s })

setStdOut :: OptParser Opts
setStdOut = success (\opts -> opts { genDirOpts = Nothing })

setHelp :: OptParser Opts
setHelp = success (\opts -> opts { helpOpts = True })

--------------------------------------------------------------------------------

mkOptNoArg :: String -> a -> String -> OptDescr a
mkOptNoArg o setter help = Option "" [o] (NoArg setter) help

mkOptArg :: String -> (String -> a) -> String -> String -> OptDescr a
mkOptArg o setter arg help = Option "" [o] (ReqArg setter arg) help

options :: [OptDescr (OptParser Opts)]
options =
  [ mkOptNoArg "std-out" setStdOut
      "print AADL to standard out"
  , mkOptArg "out-dir" setGenDir "PATH"
      "path to save AADL files"
  , mkOptNoArg "help" setHelp
      "display this message"
  ]

-- | Parse an @Opts@ structure from a list of strings.
parseOpts :: [String] -> IO Opts
parseOpts args = case parseOptions options args of
  Right f   ->
    let opts = f initialOpts
     in if helpOpts opts
           then printUsage [] >> exitSuccess
           else return opts
  Left errs -> printUsage errs >> exitFailure

printUsage :: [String] -> IO ()
printUsage errs = do
  prog <- getProgName
  let banner = unlines
        (errs ++ ["", "Usage: " ++ prog ++ " [OPTIONS]"])
  putStrLn (usageInfo banner options)

-- | Checks that filepaths conform to Camkes requirements (a C identifier).
validFPOpts :: Opts -> Opts
validFPOpts opts =
  case genDirOpts opts of
    Nothing  -> opts
    Just fp  -> opts { genDirOpts = Just (validDirName fp) }

-- | Camkes needs filepaths, modulo '/', to be valid C identifiers.
validDirName :: FilePath -> FilePath
validDirName fp =
  if | null fp
      -> error "Empty out-dir in AADL options."
     | and $ isAlpha (head fp) : fmap go (tail fp)
      -> fp
     | otherwise
      -> error $ "out-dir " ++ fp
             ++ " must contain only valid C identifiers for Camkes."
  where
  -- A character is a C identifier char or a path separator.
  go c = isAlphaNum c
      || (c == '_')
      || isPathSeparator c
