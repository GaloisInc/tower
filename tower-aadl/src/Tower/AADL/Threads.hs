--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.Threads where

import Prelude hiding (init)
import Data.Monoid
import Data.Maybe (isJust, fromJust, isNothing)
import Data.List (find)

import qualified Ivory.Tower.AST       as A
import qualified Ivory.Tower.AST.Graph as G

----------------------------------------

data Threads = Threads
  { threadsExternal     :: [A.Monitor]
  , threadsFromExtPer   :: [(A.Thread, A.Monitor)]
  , threadsPeriodic     :: [A.Thread]
  , threadsInit         :: [A.Monitor]
  , threadsFromExternal :: [A.Monitor]
  , threadsPassive      :: [A.Monitor]
  }

instance Monoid Threads where
  mempty = Threads [] [] [] [] [] []
  Threads a0 b0 c0 d0 e0 f0  `mappend` Threads a1 b1 c1 d1 e1 f1 =
    Threads (a0++a1) (b0++b1) (c0++c1) (d0++d1) (e0++e1) (f0++f1)

injectExternalThread :: A.Monitor -> Threads
injectExternalThread m = Threads [m] [] [] [] [] []

injectFromExtPerThread :: A.Thread -> A.Monitor -> Threads
injectFromExtPerThread th m = Threads [] [(th,m)] [] [] [] []

injectPeriodicThread :: A.Thread -> Threads
injectPeriodicThread m = Threads [] [] [m] [] [] []

injectInitThread :: A.Monitor -> Threads
injectInitThread m = Threads [] [] [] [m] [] []

injectFromExternalThread :: A.Monitor -> Threads
injectFromExternalThread m = Threads [] [] [] [] [m] []

injectPassiveThread :: A.Monitor -> Threads
injectPassiveThread m = Threads [] [] [] [] [] [m]

class ThreadName a where
  threadName :: a -> String

instance ThreadName A.Monitor where
  threadName = A.monitorName

instance ThreadName A.Thread where
  threadName = A.threadName

----------------------------------------

toThreads :: A.Tower -> Threads
toThreads t = mconcat pers `mappend` mconcat monsToThreads
  where
  monsToThreads :: [Threads]
  monsToThreads = map monToThread (A.tower_monitors t)
  monToThread m
    | extMon  && not init    && not fromExt && isNothing fromPer
    = injectExternalThread m
    | init    && not extMon  && not fromExt && isNothing fromPer
    = injectInitThread m
    | fromExt && not extMon  && not init    && isNothing fromPer
    = injectFromExternalThread m
    | fromExt && not extMon  && not init    && isJust fromPer
    = injectFromExtPerThread (fromJust fromPer) m
    | not (extMon || init || fromExt)
    = injectPassiveThread m
    | otherwise
    = error $ "Cannot handle a monitor that combines handlers for "
           ++ "initialization, external monitor, and handling messages "
           ++ "from external monitors for monitor: " ++ A.monitorName m
           ++ " " ++ unwords (map show [extMon, init, fromExt, isJust fromPer])

    where
    handlers = A.monitor_handlers m
    fromPer  = periodicChan t m
    fromExt  = any (externalChan t) handlers
    init     = any fromInit handlers
    extMon   =
      case A.monitor_external m of
        A.MonitorExternal
          -> True
        _ -> False

  pers = map injectPeriodicThread (periodThreads t)

  fromInit :: A.Handler -> Bool
  fromInit h =
    case A.handler_chan h of
      A.ChanInit{} -> True
      _            -> False

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

-- Returns the thread that sends the periodic clock.
periodicChan :: A.Tower -> A.Monitor -> Maybe A.Thread
periodicChan t m =
  case concatMap go chs of
    []  -> Nothing
    [t'] -> Just t'
    _   -> error "Impossible lookup in periodicChan: tower-aadl"

  where
  go (t', mshs) =
    case lookup m mshs of
      Nothing -> []
      Just{}  -> [t']

  ts = periodThreads t

  chs :: [(A.Thread, [(A.Monitor, A.Handler)])]
  chs = zip ts (map (G.towerChanHandlers t) (map A.threadChan ts))

----------------------------------------

periodThreads :: A.Tower -> [A.Thread]
periodThreads t =
  filter (\th -> case th of
                 A.PeriodThread{} -> True
                 _                -> False
         ) (A.towerThreads t)
