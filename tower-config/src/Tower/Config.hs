
module Tower.Config
  ( module Tower.Config.Parser
  , getConfig'
  , getConfig
  ) where

import Control.Monad (when)
import Ivory.Tower.Compile.Options
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
  let dbg s = when (cfgopts_debug cfgopts) (putStrLn s)
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)
    Right toml -> do
      dbg ("base document: " ++ (show toml))
      let extendedV :: Value
          extendedV = Left (extendConfig toml (topts_args topts))
      dbg ("document with cmdargs: " ++ (show extendedV))
      case runConfigParser p extendedV of
        Right c -> return (c, t')
        Left  e -> topts_error t' ("Error parsing config file: " ++ e)

getConfig :: TOpts -> ConfigParser a -> IO a
getConfig topts p = fmap fst (getConfig' topts p)
