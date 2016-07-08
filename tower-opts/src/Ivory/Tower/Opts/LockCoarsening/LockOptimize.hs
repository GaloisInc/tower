module Ivory.Tower.Opts.LockCoarsening.LockOptimize
      ( lockOptimizeMonitor
      ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Opts
import Data.List
import Data.Ord

getLocked :: [String] -> AST.Monitor -> [AST.HandlerFast]
getLocked lock mon = 
  let han = AST.monitor_handlers mon
      predicate = \x ->
        let y = case getOpt (LockCoarsening OptVoid) (AST.handler_transformers x) of
              Nothing -> err "Trying to optimize locks on a handler that has no lock coarsening annotation."
              (Just (LockCoarsening (OptHandler a))) -> a
              _ -> err "While getting AST annotation : obtained something else that what asked (OptHandler)."
        in
        (not $ null $ intersect (sort lock) (sort y))
      list = filter predicate han in
  map AST.HandlerFast list

-- | Auxiliary function for optimizeLocks
getBestToMerge :: AST.Monitor              -- ^ The monitor in which the lock coarsening is done
               -> [String]                 -- ^ A lock L1
               -> [[String]]               -- ^ An accumulator
               -> [[String]]               -- ^ The list of locks to look into
               -> ([[String]],[[String]])  -- ^ The previous list of locks with the lock L2 to merge with in the head of the second list
getBestToMerge _ _ acc [] = (reverse acc, [])
getBestToMerge mon element acc (test:liste) =
  let big = sort $ getLocked test mon
      small = sort $ getLocked element mon in
  if ((intersect small big) == small)
    then (reverse acc, test:liste)
    else getBestToMerge mon element (test:acc) liste

-- | Given a monitor, and a list of locks, this function will return a new list of locks in which some monitors have been merged together.
--   For each pair of locks (L1 , L2), we define Si the set of handlers that acquire the lock Li.
--   If (S1 included in S2) or (S2 included in S1), we merge L1 and L2
optimizeLocks :: AST.Monitor -- ^ The monitor in which the lock coarsening is done
              -> [[String]]  -- ^ An accumulator
              -> [[String]]  -- ^ A list of locks to optimize (a lock is a list of ressources)
              -> [[String]]  -- ^ The list of locks after optimization
optimizeLocks mon [] (a:b) = optimizeLocks mon [a] b
optimizeLocks _ l [] = l
optimizeLocks mon l (a:b) =
  let (deb,liste) = getBestToMerge mon a [] l in
  case liste of
    [] -> optimizeLocks mon (deb++[a]) b
    this:fin -> optimizeLocks mon (deb++[nub $ this ++ a]++fin) b


lockOptimizeMonitor :: AST.Monitor -> IO (AST.Monitor,Int)
lockOptimizeMonitor mon = do
  let locks = case getOpt (LockCoarsening OptVoid) (AST.monitor_transformers mon) of
        Nothing -> err "Trying to optimize locks on a handler that has no lock coarsening annotation."
        (Just (LockCoarsening (OptMonitor a))) -> a
        _ -> err "While getting AST annotation : obtained something else that what asked (OptMonitor)."
      sortedLocks = sortBy (\x y-> compare (Down $ length $ getLocked x mon) (Down $ length $ getLocked y mon)) locks
  -- Opt concept : take two locks and merge them if one is included in the other
  let bestLocks = optimizeLocks mon [] sortedLocks
  return (mon {AST.monitor_transformers = replaceOpt (LockCoarsening $ OptMonitor bestLocks) (AST.monitor_transformers mon)},length bestLocks)


err :: String -> a
err msg = error ("Ivory.Tower.Opts.LockCoarsening.LockOptimize: " ++ msg)
