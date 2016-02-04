{-# LANGUAGE MultiWayIf #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Threads where

import Prelude hiding (init)
import Data.Monoid
import Data.Maybe (isJust)
import Data.List (find)

import Debug.Trace

import qualified Ivory.Tower.AST       as A
import qualified Ivory.Tower.AST.Graph as G

----------------------------------------

data ActiveThreads = ActiveThreads
  { atThreadsInit         :: [String]
  , atThreadsPeriodic     :: [A.Period]
  , atThreadsSignal       :: [A.Signal]
  }

data PassiveThreads = PassiveThreads
  { ptThreadsExternal     :: [A.Monitor]
  , ptThreadsFromExtPer   :: [(A.Thread, A.Monitor)]
  , ptThreadsFromExternal :: [A.Monitor]
  , ptThreadsPassive      :: [A.Monitor]
  }

instance Monoid ActiveThreads where
  mempty = ActiveThreads [] [] []
  ActiveThreads a0 b0 c0 `mappend` ActiveThreads a1 b1 c1 =
    ActiveThreads (a0++a1) (b0++b1) (c0++c1)

instance Monoid PassiveThreads where
  mempty = PassiveThreads [] [] [] []
  PassiveThreads a0 b0 c0 d0 `mappend` PassiveThreads a1 b1 c1 d1 =
    PassiveThreads (a0++a1) (b0++b1) (c0++c1) (d0++d1)

injectExternalThread :: A.Monitor -> PassiveThreads
injectExternalThread m = mempty { ptThreadsExternal = [m] }

injectFromExtPerThread :: A.Thread -> A.Monitor -> PassiveThreads
injectFromExtPerThread th m = undefined --passiveInject (mempty { threadsFromExtPer = [(th,m)] })

injectPeriodicThread :: A.Period -> ActiveThreads
injectPeriodicThread m = mempty { atThreadsPeriodic = [m] }

injectInitThread :: String -> ActiveThreads
injectInitThread t = mempty { atThreadsInit = [t] }

injectFromExternalThread :: A.Monitor -> PassiveThreads
injectFromExternalThread m = mempty { ptThreadsFromExternal = [m] }

injectPassiveThread :: A.Monitor -> PassiveThreads
injectPassiveThread m = mempty { ptThreadsPassive = [m] }

injectSignalThread :: A.Signal -> ActiveThreads
injectSignalThread t = mempty { atThreadsSignal = [t] }

class ThreadName a where
  threadName :: a -> String

instance ThreadName A.Monitor where
  threadName = A.monitorName

instance ThreadName A.Thread where
  threadName = A.threadName

----------------------------------------

-- -- Take Tower monitors and threads and turn them into AADL threads.
-- toThreads :: A.Tower -> Threads
-- toThreads t = mconcat pers `mappend` mconcat monsToThreads
--   where
--   monsToThreads :: [Threads]
--   monsToThreads = map monToThread (A.tower_monitors t)
--   monToThread m
--     | extMon  && not init    && not fromExt && isNothing fromPer
--     = injectExternalThread m
--     | init    && not extMon  && not fromExt && isNothing fromPer
--     = injectInitThread m
--     | fromExt && not extMon  && not init    && isNothing fromPer
--     = injectFromExternalThread m
--     | fromExt && not extMon  && not init    && isJust fromPer
--     = injectFromExtPerThread (fromJust fromPer) m
--     | not (extMon || init || fromExt)
--     = injectPassiveThread m
--     | otherwise
--     = error $ "Cannot handle a monitor that combines handlers for "
--            ++ "initialization, external monitor, and handling messages "
--            ++ "from external monitors for monitor: " ++ A.monitorName m
--            ++ " " ++ unwords (map show [extMon, init, fromExt, isJust fromPer])

--     where
--     handlers = A.monitor_handlers m
--     fromPer  = periodicChan t m
--     fromExt  = any (externalChan t) handlers
--     init     = any fromInit handlers
--     extMon   =
--       case A.monitor_external m of
--         A.MonitorExternal
--           -> True
--         _ -> False

--   pers = map injectPeriodicThread (periodThreads t)

--   fromInit :: A.Handler -> Bool
--   fromInit h =
--     case A.handler_chan h of
--       A.ChanInit{} -> True
--       _            -> False

-- Take Tower monitors and threads and turn them into AADL threads.

-- toThreads t =
--      monitorsToThreads (A.tower_monitors t)
--   <> towerThreadsToThreads (A.towerThreads t)

toPassiveThreads :: A.Tower -> PassiveThreads
toPassiveThreads t = mconcat (map injectPassiveThread (A.tower_monitors t))

-- where
  -- monitorToThread m = 
    -- if | A.monitor_external m == A.MonitorExternal
    --      -> injectExternalThread m

toActiveThreads :: A.Tower -> ActiveThreads
toActiveThreads t =
  mconcat (map towerThreadToThread (A.towerThreads t))
  where
  towerThreadToThread t =
    case t of
      A.SignalThread s -> injectSignalThread s
      A.PeriodThread p -> injectPeriodicThread p
      A.InitThread   i -> injectInitThread i

----------------------------------------

-- Computes whether a handler handles a message sent from an external monitor.
-- XXX expensive to recompute. Compute once?
externalChan :: A.Tower -> A.Handler -> Bool
externalChan t h =
  isJust $ find (\h' -> A.handler_name h' == A.handler_name h) fromExts
  where
  ms       = A.tower_monitors t
  extMs    = filter (\m -> A.monitor_external m == A.MonitorExternal) ms
  extHs    = concatMap A.monitor_handlers extMs
  fromExts = map snd $ concatMap (A.handlerOutboundHandlers t) extHs

-- -- Returns the thread that sends the periodic clock.
-- periodicChan :: A.Tower -> A.Monitor -> Maybe A.Thread
-- periodicChan t m =
--   case concatMap go chs of
--     []  -> Nothing
--     [t'] -> Just t'
--     _   -> error "Impossible lookup in periodicChan: tower-aadl"

--   where
--   go (t', mshs) =
--     case lookup m mshs of
--       Nothing -> []
--       Just{}  -> [t']

--   ts = periodThreads t

--   chs :: [(A.Thread, [(A.Monitor, A.Handler)])]
--   chs = zip ts (map (G.towerChanHandlers t) (map A.threadChan ts))

-- ----------------------------------------

-- periodThreads :: A.Tower -> [A.Thread]
-- periodThreads t =
--   filter (\th -> case th of
--                  A.PeriodThread{} -> True
--                  _                -> False
--          ) (A.towerThreads t)
