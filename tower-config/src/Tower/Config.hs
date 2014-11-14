
module Tower.Config
  ( module Tower.Config.Parser
  , getConfig
  ) where

import Ivory.Tower.Compile.Options
import Tower.Config.Parser
import Tower.Config.Options
import Tower.Config.Document
import Tower.Config.Extend

getConfig :: TOpts -> ConfigParser a -> IO (a, TOpts)
getConfig topts p = do
  (cfgopts, t') <- getCfgOpts topts
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)
    Right toml ->
      let extendedV = Left (extendConfig toml (topts_args topts)) in
      case runConfigParser p extendedV of
        Right c -> return (c, t')
        Left  e -> topts_error t' ("Error parsing config file: " ++ e)


