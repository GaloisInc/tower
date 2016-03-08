--
-- Setting priorities for AADL threads.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Priorities where

import           Data.List
import qualified Data.Map as M

import           Tower.AADL.Threads
import           Tower.AADL.Platform (OS(..))
import qualified Ivory.Tower.AST as A

----------------------------------------

data Priority
  = MinPriority
  | P Int -- ^ Positive means the priority relative to min, negative is priority relative to max,
          --   zero means MinPriority
  | MaxPriority
  deriving (Read, Show, Eq, Ord)

toEchronosPriority :: Priority -> Int
toEchronosPriority MinPriority = 140
toEchronosPriority (P n) | abs n > 20 = error ("toEchronosPriority: relative priority ("
                                     ++ show n ++ ")" ++ " out of range [0..20]")
                         | n >= 0     = 140 - n
                         | otherwise  = 120 - n
toEchronosPriority MaxPriority = 120

toSeL4Priority :: Priority -> Int
toSeL4Priority MinPriority = 120
toSeL4Priority (P n) | abs n > 20  = error ("toSeL4Priority: relative priority ("
                                  ++ show n ++ ")" ++ " out of range [0..20]")
                     | n >= 0      = 120 + n
                     | otherwise   = 140 + n
toSeL4Priority MaxPriority = 140

minPriority :: Priority
minPriority = MinPriority

maxPriority :: Priority
maxPriority = MaxPriority

incPriority :: Priority -> Priority
incPriority MinPriority = P 1
incPriority (P n) | n >= 0     = P (n + 1)
                  | otherwise  = P (n - 1)
incPriority MaxPriority = MaxPriority

decPriority :: Priority -> Priority
decPriority MinPriority = MinPriority
decPriority (P n) | n >= 0    = P (n - 1)
                  | otherwise = P (n + 1)
decPriority MaxPriority = P (-1)

----------------------------------------

minPer :: Priority
minPer = incPriority minPriority

perPriorities :: [Priority]
perPriorities = iterate incPriority minPer

-- | Map from monitor names to priorities
type PriorityMap = M.Map String Priority

emptyPriorityMap :: PriorityMap
emptyPriorityMap = M.empty

getPriority :: OS -> String -> PriorityMap -> Int
getPriority os nm mp = transform $
  case M.lookup nm mp of
    Nothing -> error $ "Internal error: lookup of monitor "
                     ++ nm ++ " in priority map."
    Just p  -> p
  where
  transform = case os of
    EChronos -> toEchronosPriority
    CAmkES   -> toSeL4Priority

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
         HasInit -> M.fromList [(A.threadName (A.InitThread A.Init), minPriority)]

  p  = go (\(t,pri) -> (A.threadName (A.PeriodThread t), pri))
          (zip orderedPeriodic perPriorities)

  s  = go (\t -> (A.threadName (A.SignalThread t), maxPriority)) (atThreadsSignal thds)

  e  = go (\t -> (A.monitorName t,  maxPriority)) (atThreadsExternal thds)

  fp = go (\t -> (A.monitorName t, incPriority minPriority)) (atThreadsFromPeriodic thds)

  fe = go (\(t,_) -> (A.monitorName t, maxPriority)) (atThreadsFromExternal thds)

  orderedPeriodic = reverse (sort pts)

  pts = atThreadsPeriodic thds
