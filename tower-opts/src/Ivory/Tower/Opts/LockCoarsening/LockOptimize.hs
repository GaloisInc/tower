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
import Data.Ord

getLocked :: [String] -> AST.Monitor -> [AST.HandlerFast]
getLocked lock mon = 
  let han = AST.monitor_handlers mon in
  let list = filter (\x -> let (Just (LockCoarsening (OptHandler y))) = getOpt (LockCoarsening OptVoid) (AST.handler_transformers x) in (not $ null $ intersect (sort lock) (sort y))) han in
  map AST.HandlerFast list

getBestToMerge :: AST.Monitor -> [String] -> [[String]] -> [[String]] -> ([[String]],[[String]])
getBestToMerge _ _ acc [] = (reverse acc, [])
getBestToMerge mon element acc (test:liste) =
  let big = sort $ getLocked test mon in
  let small = sort $ getLocked element mon in 
  if ((intersect small big) == small)
    then (reverse acc, test:liste)
    else getBestToMerge mon element (test:acc) liste


optimizeLocks :: AST.Monitor -> [[String]] -> [[String]] -> [[String]]
optimizeLocks mon [] (a:b) = optimizeLocks mon [a] b
optimizeLocks _ l [] = l
optimizeLocks mon l (a:b) =
  let (deb,liste) = getBestToMerge mon a [] l in
  case liste of
    [] -> optimizeLocks mon (deb++[a]) b
    this:fin -> optimizeLocks mon (deb++[nub $ this ++ a]++fin) b


lockOptimizeMonitor :: AST.Monitor -> IO (AST.Monitor,Int)
lockOptimizeMonitor mon = do
  let (Just (LockCoarsening (OptMonitor locks))) = getOpt (LockCoarsening OptVoid) (AST.monitor_transformers mon)
  let sortedLocks = sortBy (\x y-> compare (Down $ length $ getLocked x mon) (Down $ length $ getLocked y mon)) locks
  -- Opt concept : take two locks and merge them if one is included in the other
  let bestLocks = optimizeLocks mon [] sortedLocks
  return (mon {AST.monitor_transformers = replaceOpt (LockCoarsening $ OptMonitor bestLocks) (AST.monitor_transformers mon)},length bestLocks)


