
module Tower.Config
  ( module Tower.Config.Types
  , getConfig
  ) where

import Ivory.Tower.Compile.Options
import Tower.Config.Types
import Tower.Config.Options
import Tower.Config.Document

getConfig :: TOpts -> IO (Value, TOpts)
getConfig topts = do
  (cfgopts, t') <- getCfgOpts topts
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Right v -> return (v, t')
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)


