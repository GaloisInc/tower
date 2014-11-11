
module Ivory.Tower.Compile.Options
  ( TOpts(..)
  , getOpts
  ) where

import Data.Monoid (mconcat)
import System.Console.GetOpt
  (ArgOrder(Permute), OptDescr(..), ArgDescr(..), getOpt, usageInfo)
import System.Exit (exitFailure)
import qualified Ivory.Compile.C.CmdlineFrontend.Options as C

data TOpts = TOpts
  { topts_configfile :: FilePath
  , topts_configpath :: [FilePath]
  } deriving (Show)

initialTOpts :: TOpts
initialTOpts  = TOpts
  { topts_configfile = "default.conf"
  , topts_configpath = ["."]
  }

setConfigFile :: FilePath -> C.OptParser TOpts
setConfigFile f = C.success (\t -> t { topts_configfile = f })

clearConfigPath :: C.OptParser TOpts
clearConfigPath = C.success (\t -> t { topts_configpath = [] })

setConfigPath :: FilePath -> C.OptParser TOpts
setConfigPath p = C.success (\t -> t { topts_configpath = topts_configpath t ++ [p] })

toptions :: [OptDescr (C.OptParser TOpts)]
toptions = [ Option "" ["conf-file"] (ReqArg setConfigFile "PATH")
              "path to tower application config file. default: default.conf"
           , Option "" ["conf-path"] (ReqArg setConfigPath "PATH")
              "extend include path for tower application config file. default: ."
           , Option "" ["clear-conf-path"] (NoArg clearConfigPath)
              "clear config file path"
           ]


parseOpts :: [OptDescr (C.OptParser opt)] -> [String]
          -> ([String], (Either [String] (opt -> opt)))
parseOpts opts args = case getOpt Permute opts args of
  (fs,us,es) -> case mconcat fs of
    C.Success f -> case es of
       [] -> (us, Right f)
       _  -> (us, Left es)
    C.Error es' -> (us, Left (es ++ es'))

getOpts :: [String] -> IO (C.Opts, TOpts)
getOpts args =
  let (us, mkCOpts)  = parseOpts C.options args
      (us', mkTOpts) = parseOpts toptions  us
  in case (mkCOpts, mkTOpts, us) of
    (Right mkc, Right mkt, []) -> return (mkc C.initialOpts, mkt initialTOpts)
    _ -> do
      let banner = getErrors mkCOpts
                ++ getErrors mkTOpts
                ++ unusedErrors us'
      putStrLn (usageInfo banner C.options)
      putStrLn (usageInfo "" toptions)
      exitFailure

getErrors :: Either [String] as -> String
getErrors (Left es) = unlines es
getErrors (Right _) = ""

unusedErrors :: [String] -> String
unusedErrors [] = ""
unusedErrors us = "The following options were not recognized: \n"
               ++ unlines us
