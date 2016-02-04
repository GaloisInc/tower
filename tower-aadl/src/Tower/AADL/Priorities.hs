--
-- Setting priorities for AADL threads.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Priorities where

import           Data.List
import qualified Data.Map as M

import           Tower.AADL.Threads

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

-- | Map from monitor names to priorities
type PriorityMap = M.Map String Priority

emptyPriorityMap :: PriorityMap
emptyPriorityMap = M.empty

getPriority :: String -> PriorityMap -> Priority
getPriority nm mp = 0 -- XXX
  -- case M.lookup nm mp of
  --   Nothing -> error $ "Internal error: lookup of monitor "
  --                    ++ nm ++ " in priority map."
  --   Just p  -> p

-- Initialization threads have the lowest priorties.
mkPriorities :: ActiveThreads -> PriorityMap
mkPriorities thds = M.empty -- XXX


  -- M.unions [extPris, perPris, initPris, extPerPris]
  -- where
  -- extPris  = M.fromList
  --          $ map (\t -> (threadName t, maxBound)) (threadsFromExternal thds)
  -- perPris  = M.fromList
  --          $ zip (map threadName orderedPeriodic) perPriorities
  -- initPris = M.fromList
  --          $ map (\t -> (threadName t, minBound)) (threadsInit thds)

  -- extPerPris = M.fromList
  --            $ map (\(th,t) -> (threadName t, pri th)) (threadsFromExtPer thds)
  --   where
  --   pri th = getPriority (threadName th) perPris

  -- orderedPeriodic = reverse $ sort (threadsPeriodic thds)

  -- minPer :: Priority
  -- minPer = minBound + fromInteger 1

  -- perPriorities = iterate (+1) minPer

  -- -- All periodic threads have priorities lower than topPer.
  -- topPer =
  --   let m = minPer + fromIntegral (length (threadsPeriodic thds)) in
  --   if m == maxBound
  --     then error "Unscheduable: not enough priority slots."
  --     else m
