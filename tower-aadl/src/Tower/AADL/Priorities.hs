--
-- Setting priorities for AADL threads.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Priorities where

import           Data.List
import qualified Data.Map as M

import           Tower.AADL.Threads
import qualified Ivory.Tower.AST as A

----------------------------------------

data Priority = P Int
  deriving (Show, Read, Eq, Ord)

instance Num Priority where
  P a + P b = P (a+b)
  P a * P b = P (a*b)
  P a - P b = P (a-b)
  negate (P a) = P (negate a)
  abs (P a) = P (abs a)
  signum (P a) = P (signum a)
  fromInteger a = P (fromIntegral a)

-- Bounds for seL4 on ODROID
instance Bounded Priority where
  minBound = 120
  maxBound = 140

----------------------------------------

minPer :: Priority
minPer = minBound + fromInteger 1

perPriorities = iterate (+1) minPer

-- | Map from monitor names to priorities
type PriorityMap = M.Map String Priority

emptyPriorityMap :: PriorityMap
emptyPriorityMap = M.empty

getPriority :: String -> PriorityMap -> Priority
getPriority nm mp =
  case M.lookup nm mp of
    Nothing -> error $ "Internal error: lookup of monitor "
                     ++ nm ++ " in priority map."
    Just p  -> p

-- Initialization threads have the lowest priorties.
-- External threads have maximum bound.
-- Periodic are rate monotonic starting from minimum priority.
mkPriorities :: ActiveThreads -> PriorityMap
mkPriorities thds =
  M.unions [i, p, s, e, fp, fe]
  where
  go f t = M.fromList (map f t)

  i  = case atThreadsInit thds of
         NoInit  -> M.empty
         HasInit -> M.fromList [(A.threadName (A.InitThread A.Init), minBound)]

  p  = go (\(t,pri) -> (A.threadName (A.PeriodThread t), pri))
          (zip orderedPeriodic perPriorities)

  s  = go (\t -> (A.signal_name t, maxBound)) (atThreadsSignal thds)

  e  = go (\t -> (A.monitorName t,  maxBound)) (atThreadsExternal thds)

  fp = go (\t -> (A.monitorName t, minBound+1)) (atThreadsFromPeriodic thds)

  fe = go (\(t,_) -> (A.monitorName t, maxBound)) (atThreadsFromExternal thds)

  orderedPeriodic = reverse (sort pts)

  pts = atThreadsPeriodic thds

-- All periodic threads have priorities lower than topPer.
  topPer =
    let m = minPer + fromIntegral (length pts) in
    if m == maxBound
      then error "Unscheduable: not enough priority slots."
      else m
