
module Tower.Config
  ( module Tower.Config.Parser
  , getConfig
  ) where

import Ivory.Tower.Compile.Options
import Tower.Config.Parser
import Tower.Config.Options
import Tower.Config.Document

getConfig :: TOpts -> ConfigParser a -> IO (a, TOpts)
getConfig topts p = do
  (cfgopts, t') <- getCfgOpts topts
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)
    Right v ->
      case runConfigParser p v of
        Right c -> return (c, t')
        Left  e -> topts_error t' ("Error parsing config file: " ++ e)


