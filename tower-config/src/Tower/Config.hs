
module Tower.Config
  ( module Tower.Config.Parser
  , getConfig'
  , getConfig
  ) where

import Control.Monad (when)
import Ivory.Tower.Compile.Options hiding (parseOpts)
import Tower.Config.Parser
import Tower.Config.Options
import Tower.Config.Document
import Tower.Config.Extend
import Tower.Config.TOML

-- XXX consider putting (Show a) constraint and debugging
-- complete 'a'.
getConfig' :: TOpts -> ConfigParser a -> IO (a, TOpts)
getConfig' topts p = do
  (cfgopts, t') <- getCfgOpts topts
  let dbg s = when (cfgopts_debug cfgopts) (putStrLn ("DBG: " ++ s))
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)
    Right toml -> do
      dbg ("base toml document:" ++ (debugtoml (Left toml)))
      let extra_opts :: [String]
          extra_opts = topts_args t'
          ext_toml :: Value
          ext_toml = Left (extendConfig toml extra_opts)
      dbg ("cfg extra args: " ++ (show extra_opts))
      dbg ("parsed extra args to: " ++ (show (parseOpts extra_opts)))
      dbg ("extended toml document:" ++ (debugtoml ext_toml))
      case runConfigParser p ext_toml of
        Right c -> return (c, t')
        Left  e -> topts_error t' ("Error parsing config file: " ++ e)

  where
  debugtoml :: Value -> String
  debugtoml v = sep ++ ppValue v ++ sep
  sep = "\n==================\n"

getConfig :: TOpts -> ConfigParser a -> IO a
getConfig topts p = fmap fst (getConfig' topts p)
