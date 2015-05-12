
module Ivory.Tower.Config
  ( module Ivory.Tower.Config.Parser
  , getConfig'
  , getConfig
  ) where

import Control.Monad (when)
import Ivory.Artifact
import Ivory.Tower.Options hiding (parseOpts)
import Ivory.Tower.Config.Parser
import Ivory.Tower.Config.Options
import Ivory.Tower.Config.Document
import Ivory.Tower.Config.TOML

getConfig' :: TOpts -> ConfigParser a -> IO (a, TOpts)
getConfig' topts p = do
  (cfgopts, t') <- getCfgOpts topts
  d <- getDocument (cfgopts_configfile cfgopts)
                   (cfgopts_configpath cfgopts)
  case d of
    Left e -> topts_error t' ("Error in tower getConfig: " ++ e)
    Right toml -> do
      let conf_artifact = artifactString "build.conf" (ppValue (Left toml))
      case runConfigParser p (Left toml) of
        Left  e -> topts_error t' ("Error parsing config file: " ++ e)
        Right c -> do
          case topts_outdir t' of
            Just d -> putArtifact_ d conf_artifact
            Nothing -> return ()
          case cfgopts_debug cfgopts of
            True -> printArtifact conf_artifact
            False -> return ()
          return (c, t')

getConfig :: TOpts -> ConfigParser a -> IO a
getConfig topts p = do
  (c, t') <- getConfig' topts p
  finalizeOpts t'
  return c
