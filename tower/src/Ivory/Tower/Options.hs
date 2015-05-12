{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Options
  ( TOpts(..)
  , towerGetOpts
  , parseOpts
  , getOpts
  , finalizeOpts
  ) where

import Data.Monoid (Monoid(..),mconcat)
import System.Console.GetOpt
  (ArgOrder(Permute), OptDescr(..), getOpt', usageInfo)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Ivory.Compile.C.CmdlineFrontend.Options as C

data TOpts = TOpts
  { topts_outdir  :: Maybe FilePath
  , topts_help    :: Bool
  , topts_args    :: [String]
  , topts_error   :: forall a . String -> IO a
  }

towerGetOpts :: IO (C.Opts, TOpts)
towerGetOpts = getArgs >>= getOpts

finalizeOpts :: TOpts -> IO ()
finalizeOpts topts = case topts_args topts of
  [] -> case topts_help topts of
    True -> topts_error topts "Usage:"
    False -> return ()
  as -> topts_error topts ("Unrecognized arguments:\n" ++ unlines as)

parseOpts :: [OptDescr (C.OptParser opt)] -> [String]
          -> ([String], (Either [String] (opt -> opt)))
parseOpts opts args =
  let (fs, ns, us, es) = getOpt' Permute opts args
      (C.OptParser errs f) = mconcat fs
      unused = ns ++ us
  in case errs ++ es of
    [] -> (unused, Right f)
    e' -> (unused, Left e')

getOpts :: [String] -> IO (C.Opts, TOpts)
getOpts args = case mkCOpts of
  Left es -> err (unlines es)
  Right mkc -> do
    let copts = mkc C.initialOpts
    return (copts, TOpts
              { topts_outdir = C.outDir copts
              , topts_help   = C.help copts
              , topts_args   = unusedArgs
              , topts_error  = err
              })
  where
  (unusedArgs, mkCOpts) = parseOpts C.options args
  err s = do
      putStrLn s
      putStrLn ""
      putStrLn "ivory-backend-c options:"
      putStrLn $ usageInfo "" C.options
      exitFailure
