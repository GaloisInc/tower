{-# LANGUAGE ConstraintKinds #-}
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

type Priority a = (Enum a, Bounded a, Ord a)

newtype SeL4Priority = SP Int
  deriving (Read, Show, Eq, Ord)

instance Bounded SeL4Priority where
  minBound = SP 120
  maxBound = SP 140

mkSeL4Priority :: Int -> SeL4Priority
mkSeL4Priority n =
  let SP maxP = maxBound
      SP minP = minBound
  in SP (min (max minP n) maxP)

instance Enum SeL4Priority where
  toEnum          = mkSeL4Priority
  fromEnum (SP n) = n
  succ     (SP n) = mkSeL4Priority (n + 1)
  pred     (SP n) = mkSeL4Priority (n - 1)

newtype EChronosPriority = EP Int
  deriving (Read, Show, Eq, Ord)

instance Bounded EChronosPriority where
  -- EChronos internally rounds priorities to nearest multiple of 16 we do that
  -- as well just to be conservative.  We accomplish it by storing values from 0
  -- to 15 and then muliplying by 16 in fromEnum.
  minBound = EP 0
  maxBound = EP 15

mkEChronosPriority :: Int -> EChronosPriority
mkEChronosPriority n =
  let EP maxP = maxBound
      EP minP = minBound
  in EP (min (max minP n) maxP)

instance Enum EChronosPriority where
  toEnum          = mkEChronosPriority
  -- See the note in the Bounded instance
  fromEnum (EP n) = n*16
  succ     (EP n) = mkEChronosPriority (n + 1)
  pred     (EP n) = mkEChronosPriority (n - 1)

minPriority :: Bounded a => a
minPriority = minBound

maxPriority :: Bounded a => a
maxPriority = maxBound

incPriority :: Enum a => a -> a
incPriority = succ

decPriority :: Enum a => a -> a
decPriority = pred

----------------------------------------

minPer :: Priority a => a
minPer = incPriority minPriority

perPriorities :: Priority a => [a]
perPriorities = iterate incPriority minPer

-- | Map from monitor names to priorities
type AbstractPriorityMap a = M.Map String a
type PriorityMap           = AbstractPriorityMap Int

emptyPriorityMap :: PriorityMap
emptyPriorityMap = M.empty

getPriority :: String -> PriorityMap -> Int
getPriority nm mp =
  M.findWithDefault (error $ "Internal error: lookup of monitor "
                           ++ nm ++ " in priority map.")
                    nm mp

mkSeL4Priorities :: ActiveThreads -> PriorityMap
mkSeL4Priorities thds =
  fromEnum <$> (mkPriorities thds :: AbstractPriorityMap SeL4Priority)

mkEChronosPriorities :: ActiveThreads -> PriorityMap
mkEChronosPriorities thds =
  fromEnum <$> (mkPriorities thds :: AbstractPriorityMap EChronosPriority)

-- Initialization threads have the lowest priorties.
-- External threads have maximum bound.
-- Periodic are rate monotonic starting from minimum priority.
mkPriorities :: Priority a => ActiveThreads -> AbstractPriorityMap a
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
