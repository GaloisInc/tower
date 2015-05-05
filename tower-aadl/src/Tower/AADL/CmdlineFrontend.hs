--
-- Command line for AADL generator.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.CmdlineFrontend where

import Data.Monoid
import Data.Maybe
import Data.Either

import System.Console.GetOpt
import System.Exit (exitFailure,exitSuccess)
import System.Environment (getProgName)

import           Tower.AADL.Config

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
  { configOpts :: Config
  -- ^ Config options.
  , genDirOpts :: Maybe FilePath
  -- ^ Location to generate AADL files (or use standard out).
  , helpOpts   :: Bool
  -- ^ Help.
  }

initialOpts :: Opts
initialOpts = Opts
  { configOpts = initialConfig
  , genDirOpts = Just ""
  , helpOpts   = False
  }

--------------------------------------------------------------------------------

setSrcsDir :: FilePath -> OptParser Opts
setSrcsDir s = success (\opts -> opts { configOpts = (configOpts opts) { configSrcsDir = s }})

setSystemName :: String ->  OptParser Opts
setSystemName s = success (\opts -> opts { configOpts = (configOpts opts) { configSystemName = s }})

setSystemOS :: String -> OptParser Opts
setSystemOS s = success (\opts -> opts { configOpts = (configOpts opts) { configSystemOS = read s }})

setSystemHW :: String -> OptParser Opts
setSystemHW s = success (\opts -> opts { configOpts = (configOpts opts) { configSystemHW = read s }})

setStdOut :: OptParser Opts
setStdOut = success (\opts -> opts { genDirOpts = Nothing })

setGenDir :: FilePath -> OptParser Opts
setGenDir s = success (\opts -> opts { genDirOpts = Just s })

setHelp :: OptParser Opts
setHelp = success (\opts -> opts { helpOpts = True })

--------------------------------------------------------------------------------

mkOptNoArg :: String -> a -> String -> OptDescr a
mkOptNoArg o setter help = Option "" [o] (NoArg setter) help

mkOptArg :: String -> (String -> a) -> String -> String -> OptDescr a
mkOptArg o setter arg help = Option "" [o] (ReqArg setter arg) help

options :: [OptDescr (OptParser Opts)]
options =
  [ mkOptArg "srcs-dir" setSrcsDir "PATH"
      "path to C sources"
  , mkOptArg "system" setSystemName "NAME"
      "system name"
  , mkOptArg "system" setSystemOS "NAME"
      "OS name"
  , mkOptArg "system" setSystemHW "NAME"
      "hardware name"
  , mkOptNoArg "std-out" setStdOut
      "print AADL to standard out"
  , mkOptArg "aadl-dir" setGenDir "PATH"
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

-- | Checks that filepaths conform to Camkes requirements. Either Fails or
-- Returns a no-op.
validFPOpts :: Opts -> Opts
validFPOpts opts =
  let ls = lefts res in
  if null ls
    then opts
    else error (unlines ls)
  where
  res :: [Either String FilePath]
  res =
       maybeToList (fmap validDirName (genDirOpts opts))
    ++ fmap validDirName [ configSrcsDir c, configHdrDir c ]
  c = configOpts opts
