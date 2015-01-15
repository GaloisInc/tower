
module Ivory.Tower.Config
  ( module Ivory.Tower.Config.Parser
  , getConfig'
  , getConfig
  ) where

import Control.Monad (when)
import Ivory.Artifact
import Ivory.Tower.Compile.Options hiding (parseOpts)
import Ivory.Tower.Config.Parser
import Ivory.Tower.Config.Options
import Ivory.Tower.Config.Document
import Ivory.Tower.Config.Extend
import Ivory.Tower.Config.TOML

getConfig' :: TOpts -> ConfigParser a -> IO (a, TOpts)
getConfig' topts p = do
  (cfgopts, t') <- getCfgOpts topts
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)
    Right toml -> do
      let extra_opts :: [String]
          extra_opts = topts_args t'
          ext_toml :: Value
          ext_toml = Left (extendConfig toml extra_opts)
          conf_artifact = artifactString "build.conf" (ppValue ext_toml)
      case runConfigParser p ext_toml of
        Left  e -> topts_error t' ("Error parsing config file: " ++ e)
        Right c -> do
          case (topts_outdir t', cfgopts_debug cfgopts) of
            (Just d, False) -> putArtifact_ d conf_artifact
            (Just d, True)  -> putArtifact_ d conf_artifact
                            >> printArtifact conf_artifact
            (Nothing, _)    -> printArtifact conf_artifact
          return (c, t')

getConfig :: TOpts -> ConfigParser a -> IO a
getConfig topts p = fmap fst (getConfig' topts p)
