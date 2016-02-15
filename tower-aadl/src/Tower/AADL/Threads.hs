{-# LANGUAGE MultiWayIf #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Threads
  ( HMap
  , ActiveThreads(..)
  , PassiveThreads(..)
  , HasInit(..)
  , emptyHMap
  , fromHMap
  , toPassiveThreads
  , toActiveThreads
  ) where


import Prelude hiding (init)
import Data.Monoid
import Data.Maybe (isJust)
import Data.List (find)

import qualified Ivory.Tower.AST as A
import qualified Ivory.Tower.Types.Unique as U

----------------------------------------

-- Handler unique names that come from either periodic or external threads.
type HMap = [U.Unique]

emptyHMap :: HMap
emptyHMap = []

fromHMap :: HMap -> A.Handler -> Bool
fromHMap hmap h =
    isJust
  $ find (\u -> u == A.handler_name h) hmap

data HasInit = NoInit | HasInit deriving (Show, Read, Eq, Ord)

instance Monoid HasInit where
  mempty                    = NoInit
  HasInit `mappend` _       = HasInit
  _       `mappend` HasInit = HasInit
  _       `mappend` _       = NoInit

-- Intermediate data types that collect Tower elements into groups that are
-- meaningful for AADL (notably, distinguishing active and passive threads).

data ActiveThreads = ActiveThreads
  { atThreadsInit         :: HasInit
  , atThreadsPeriodic     :: [A.Period]
  , atThreadsSignal       :: [A.Signal]
  , atThreadsExternal     :: [A.Monitor]
  , atThreadsFromPeriodic :: [A.Monitor]
  , atThreadsFromExternal :: [(A.Monitor, HMap)]  -- From external or periodic threads
  } deriving Show

data PassiveThreads = PassiveThreads
  { ptThreadsPassive :: [A.Monitor]
  } deriving Show

instance Monoid ActiveThreads where
  mempty = ActiveThreads mempty [] [] [] [] []
  ActiveThreads a0 b0 c0 d0 e0 f0 `mappend` ActiveThreads a1 b1 c1 d1 e1 f1 =
    ActiveThreads (a0 <> a1) (b0 <> b1) (c0 <> c1) (d0 <> d1) (e0 <> e1) (f0 <> f1)

instance Monoid PassiveThreads where
  mempty = PassiveThreads []
  PassiveThreads a0 `mappend` PassiveThreads a1 =
    PassiveThreads (a0++a1)

injectInitThread :: ActiveThreads
injectInitThread = mempty { atThreadsInit = HasInit }

injectPeriodicThread :: A.Period -> ActiveThreads
injectPeriodicThread m = mempty { atThreadsPeriodic = [m] }

injectSignalThread :: A.Signal -> ActiveThreads
injectSignalThread t = mempty { atThreadsSignal = [t] }

injectExternalThread :: A.Monitor -> ActiveThreads
injectExternalThread m = mempty { atThreadsExternal = [m] }

injectFromExternal :: (A.Monitor, HMap) -> ActiveThreads
injectFromExternal m = mempty { atThreadsFromExternal = [m] }

injectFromPeriodic :: A.Monitor -> ActiveThreads
injectFromPeriodic m = mempty { atThreadsFromPeriodic = [m] }

injectPassiveThread :: A.Monitor -> PassiveThreads
injectPassiveThread m = mempty { ptThreadsPassive = [m] }

class ThreadName a where
  threadName :: a -> String

instance ThreadName A.Monitor where
  threadName = A.monitorName

instance ThreadName A.Thread where
  threadName = A.threadName

----------------------------------------

-- All monitors except monitors that are labeled as external. For each passive
-- monitor, we also record whether any of its handlers send or receive messages
-- to an external monitor.
toPassiveThreads :: A.Tower -> PassiveThreads
toPassiveThreads t = mconcat (map injectPassiveThread pts)
  where
  ms    = A.tower_monitors t
  pts = filter (not . isExternalMonitor)
      $ filter (not . isFromExternalMon t)
      $ filter (not . isFromPerThread t)
        ms

toActiveThreads :: A.Tower -> ActiveThreads
toActiveThreads t =
     mconcat (map towerThreadToThread (A.towerThreads t))
  <> mconcat (map injectExternalThread iem)
  <> mconcat (map injectFromExternal (mkExternalActiveThreads t))
  <> mconcat (map injectFromPeriodic (mkPeriodicActiveThreads t))
  where
  towerThreadToThread thd =
    case thd of
      A.InitThread{}   -> injectInitThread
      A.PeriodThread p -> injectPeriodicThread p
      A.SignalThread s -> injectSignalThread s

  iem  = filter isExternalMonitor (A.tower_monitors t)

mkExternalActiveThreads :: A.Tower -> [(A.Monitor, HMap)]
mkExternalActiveThreads t = map go ms
  where
  ms = filter (isFromExternalMon t) (A.tower_monitors t)

  go :: A.Monitor -> (A.Monitor, HMap)
  go m = (m, mp)
    where
    mp = map A.handler_name (handlersFromExternalMon t m)

-- Note: filters out monitors that have handlers from external threads. So
-- external threads take precedence.
mkPeriodicActiveThreads :: A.Tower -> [A.Monitor]
mkPeriodicActiveThreads t =
  filter (isFromPerThread t) (A.tower_monitors t)

----------------------------------------

isFromExternalMon :: A.Tower -> A.Monitor -> Bool
isFromExternalMon t m = not $ null $ handlersFromExternalMon t m

handlersFromExternalMon :: A.Tower -> A.Monitor -> [A.Handler]
handlersFromExternalMon t m =
  filter (isFromExternalMonH t) (A.monitor_handlers m)

-- Does the handler handle a message sent by a handler in an external monitor?
isFromExternalMonH :: A.Tower -> A.Handler -> Bool
isFromExternalMonH t h =
  isJust $ find (\h' -> A.handler_name h' == A.handler_name h) fromExts
  where
  ms       = A.tower_monitors t
  extMs    = filter isExternalMonitor ms
  extHs    = concatMap A.monitor_handlers extMs
  fromExts = map snd $ concatMap (A.handlerOutboundHandlers t) extHs

-- Is this an external monitor?
isExternalMonitor :: A.Monitor -> Bool
isExternalMonitor m = A.monitor_external m == A.MonitorExternal

------------------------------------------------------------

isFromPerThread :: A.Tower -> A.Monitor -> Bool
isFromPerThread t m =
     not (null (handlersFromPerThread m))
  && not (isFromExternalMon t m)

handlersFromPerThread :: A.Monitor -> [A.Handler]
handlersFromPerThread m =
  filter isFromPerThreadH (A.monitor_handlers m)

-- Does the handler handle a message sent by a handler in periodic thread?
isFromPerThreadH :: A.Handler -> Bool
isFromPerThreadH h =
  case A.handler_chan h of
    A.ChanPeriod{} -> True
    _              -> False

