{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Compile.Options
  ( TOpts(..)
  , parseOpts
  , getOpts
  ) where

import Data.Monoid (Monoid(..),mconcat)
import System.Console.GetOpt
  (ArgOrder(Permute), OptDescr(..), getOpt', usageInfo)
import System.Exit (exitFailure)
import qualified Ivory.Compile.C.CmdlineFrontend.Options as C

data TOpts = TOpts
  { topts_args :: [String]
  , topts_error   :: forall a . String -> IO a
  }

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
getOpts args =
  let (unused, mkCOpts) = parseOpts C.options args
      topts = TOpts
        { topts_args = unused
        , topts_error = \s -> do
            putStrLn ("Errors in ivory-backend-c options:\n" ++ s)
            putStrLn $ usageInfo "" C.options
            exitFailure
        }
  in case mkCOpts of
    Right mkc -> return (mkc C.initialOpts, topts)
    Left es -> topts_error topts (unlines es)
