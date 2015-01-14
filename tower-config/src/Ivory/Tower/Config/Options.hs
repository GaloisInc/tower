
module Tower.Config.Options
  ( CfgOpts(..)
  , getCfgOpts
  ) where

import Ivory.Tower.Compile.Options
import System.Console.GetOpt
  (OptDescr(..), ArgDescr(..), usageInfo)
import qualified Ivory.Compile.C.CmdlineFrontend.Options as C

data CfgOpts = CfgOpts
  { cfgopts_configfile :: FilePath
  , cfgopts_configpath :: [FilePath]
  , cfgopts_debug      :: Bool
  } deriving (Show)

initialCfgOpts :: CfgOpts
initialCfgOpts  = CfgOpts
  { cfgopts_configfile = "default.conf"
  , cfgopts_configpath = ["."]
  , cfgopts_debug      = False
  }

setConfigFile :: FilePath -> C.OptParser CfgOpts
setConfigFile f = C.success (\t -> t { cfgopts_configfile = f })

clearConfigPath :: C.OptParser CfgOpts
clearConfigPath = C.success (\t -> t { cfgopts_configpath = [] })

setConfigPath :: FilePath -> C.OptParser CfgOpts
setConfigPath p = C.success (\t -> t { cfgopts_configpath = cfgopts_configpath t ++ [p] })

setConfigDebug :: C.OptParser CfgOpts
setConfigDebug = C.success (\t -> t { cfgopts_debug = True })

cfgOptions :: [OptDescr (C.OptParser CfgOpts)]
cfgOptions = [ Option "" ["conf-file"] (ReqArg setConfigFile "PATH")
                "path to tower application config file. default: default.conf"
             , Option "" ["conf-path"] (ReqArg setConfigPath "PATH")
                "extend include path for tower application config file. default: ."
             , Option "" ["clear-conf-path"] (NoArg clearConfigPath)
                 "clear config file path"
             , Option "" ["debug-conf"] (NoArg setConfigDebug)
                 "print debugging info for config"
             ]

getCfgOpts :: TOpts -> IO (CfgOpts, TOpts)
getCfgOpts topts =
  let (unused, mkCfgOpts)  = parseOpts cfgOptions (topts_args topts)
      topts' = TOpts
        { topts_args = unused
        , topts_error = \s -> do
            putStrLn ("Errors in tower-config options:\n" ++ s)
            putStrLn $ usageInfo "" cfgOptions
            topts_error topts "(none)"
        }
  in case mkCfgOpts of
    Right mkcfg -> return (mkcfg initialCfgOpts, topts')
    Left es -> topts_error topts' (unlines es)

