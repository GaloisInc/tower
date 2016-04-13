{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Opts.LockCoarsening.LockOptimize
      ( lockOptimizeMonitor
      ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Opts
import Data.List

getLocked :: [String] -> AST.Monitor -> [AST.Handler]
getLocked lock mon = 
  let han = AST.monitor_handlers mon in
  filter (\x -> let (Just (LockCoarsening (OptHandler y))) = getOpt (LockCoarsening OptVoid) (AST.handler_transformers x) in (not $ null $ intersect lock y)) han

getBestToMerge :: AST.Monitor -> [String] -> [[String]] -> [[String]] -> ([[String]],[[String]])
getBestToMerge mon elem acc [] = (reverse acc, [])
getBestToMerge mon elem acc (test:liste) =
  if (null $ elem \\ test)
    then (reverse acc, test:liste)
    else getBestToMerge mon elem (test:acc) liste


optimizeLocks :: AST.Monitor -> [[String]] -> [[String]] -> [[String]]
optimizeLocks mon [] (a:b) = optimizeLocks mon [a] b
optimizeLocks mon l [] = l
optimizeLocks mon l (a:b) =
  let (deb,liste) = getBestToMerge mon a [] l in
  case liste of
    [] -> optimizeLocks mon (deb++[a]) b
    this:fin -> optimizeLocks mon (deb++[nub $ this ++ a]++fin) b


lockOptimizeMonitor :: AST.Monitor -> IO AST.Monitor
lockOptimizeMonitor mon = do
  let (Just (LockCoarsening (OptMonitor locks))) = getOpt (LockCoarsening OptVoid) (AST.monitor_transformers mon)
  let sortedLocks = sortBy (\x y-> compare (length $ getLocked x mon) (length $ getLocked y mon)) locks
  -- Opt concept : take two locks and merge them if one is included in the other
  let bestLocks = optimizeLocks mon [] sortedLocks
  return mon {AST.monitor_transformers = replaceOpt (LockCoarsening $ OptMonitor bestLocks) (AST.monitor_transformers mon)}


