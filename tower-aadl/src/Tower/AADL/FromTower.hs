{-# LANGUAGE RecordWildCards #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.FromTower
  -- ( fromTower
  -- )
    where

import Prelude hiding (init)

import qualified Ivory.Tower.AST as A
import qualified Ivory.Tower.Types.Time as T
import Ivory.Tower.Types.Unique (showUnique)
import qualified Ivory.Tower.AST.Graph as G

import Tower.AADL.AST.AST
import Tower.AADL.AST.TypeAST

import Control.Arrow

--------------------------------------------------------------------------------

data Config = Config
  { configThreadModules :: [FilePath]
  -- ^ Ivory module names for thread code, corresponding to the list of (active)
  -- threads in the Tower AST.
  , configSystemName :: String
  , configSystemOS   :: String
  , configSystemHW   :: String
  } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

-- | Takes a name for the system, a Tower AST, and returns an AADL System AST.
fromTower :: Config -> A.Tower -> System
fromTower c t = System { .. }
  where
  systemName       = configSystemName c
  systemProperties = [ SystemOS (configSystemOS c)
                     , SystemHW (configSystemHW c) ]
  systemComponents = [mkProcess systemName t]

mkProcess :: String -> A.Tower -> Process
mkProcess systemName t = Process { .. }
  where
  processName = systemName ++ "_process"
  processComponents = activeThreads ++ passiveThreads
  activeThreads  = map (fromThread t) (skipInitThread (A.towerThreads t))
  passiveThreads = map (fromMonitor "foo") (A.tower_monitors t)
  skipInitThread = filter (("thread_init" /=) . A.threadName)

fromThread :: A.Tower -> A.Thread -> Thread
fromThread twr t = Thread { .. }
  where
  threadName       = A.threadName t
  threadFeatures   = [ChannelFeature (chanFeature twr t)]
  threadProperties =
    [ ThreadType Active
    , DispatchProtocol (dispatch (A.threadChan t))

    -- XXX made up for now
     , ExecTime 10 100
    , StackSize 100
    , Priority 1
    ]

dispatch :: A.Chan -> DispatchProtocol
dispatch c = case c of
  A.ChanSignal{}  -> Aperiodic
  A.ChanPeriod p  -> Periodic (T.toMicroseconds (A.period_dt p))
  _               -> error $ "dispatch: " ++ show c

chanFeature :: A.Tower -> A.Thread -> Channel
chanFeature twr t = case c of
  A.ChanSync{}
    -> error "Impossible: in fromThread: active thread with ChanSync."
  A.ChanSignal s
    -> error $ "chanFeature " ++ show c
  A.ChanPeriod per
    -> let chanLabel = prettyPeriod per in
       let chanType  = periodChanType per in
       Channel { .. }
  A.ChanInit init
    -> error $ "chanFeature " ++ show c
  where
  c             = A.threadChan t
  chanHandle    = Output
  chanCallbacks = Prim (map A.monitorName chanMonitors)
  chanMonitors  = map fst (G.towerChanHandlers twr c)

fromMonitor :: FilePath -> A.Monitor -> Thread
fromMonitor fp m = Thread { .. }
  where
  (fs, ps) = concatPair (map (fromHandler fp) (A.monitor_handlers m))
  threadName = A.monitorName m
  threadFeatures = fs
  threadProperties = ps ++
    [ ThreadType Passive
    , DispatchProtocol Aperiodic

    -- XXX made up for now
    , ExecTime 10 100
    , StackSize 100
    , Priority 1
    ]

fromHandler :: FilePath -> A.Handler -> ([Feature], [ThreadProperty])
fromHandler fp h = (cf:fs, ps)
  where
  callbacks = map showUnique (A.handler_callbacks h)
  cf        = fromInputChan fp callbacks (A.handler_chan h)
  (fs, ps)  = concatPair (map fromEmitter (A.handler_emitters h))

-- | Process Tower handlers which take inputs and run callbacks.
fromInputChan :: FilePath -> [String] -> A.Chan -> Feature
fromInputChan fp callbacks c = case c of
  A.ChanSync s
    -> let chanType  = syncChanType  s in
       let chanLabel = syncChanLabel s in
       ChannelFeature (Channel { .. })
  A.ChanSignal sig
    -> error $ "fromInputChan " ++ show c
  A.ChanPeriod per
    -> let chanLabel = prettyPeriod per in
       let chanType  = periodChanType per in
       ChannelFeature (Channel { .. })
  A.ChanInit{}
    -> error "Impossible ChanInit in fromInputChan."
  where
  chanHandle = Input
  chanCallbacks = User (zip (repeat fp) callbacks)

fromEmitter :: A.Emitter -> ([Feature], [ThreadProperty])
fromEmitter e = (fs, ps)
  where
  c  = A.emitter_chan e
  fs = [ChannelFeature (emitterChan (A.emitterProcName e) c)]
  ps = [SendEvents [(chanLabel, A.emitter_bound e)]]
  chanLabel = case c of
    A.ChanSync s
      -> show (A.sync_chan_label s)
    _ -> error $ "Impossible chan " ++ show c ++ " in fromEmitter."

emitterChan :: String -> A.Chan -> Channel
emitterChan proc c = case c of
  A.ChanSync s
    -> Channel { .. }
      where
      chanLabel = syncChanLabel s
      chanType  = syncChanType s
  _ -> error $ "Impossible channel " ++ show c ++ " in emitterChan."
  where
  chanHandle    = Output
  chanCallbacks = Prim [proc]

syncChanType :: A.SyncChan -> ChanType
syncChanType = renderChanType . A.sync_chan_type

syncChanLabel :: A.SyncChan -> String
syncChanLabel = ("sync_" ++) . show . A.sync_chan_label

--------------------------------------------------------------------------------
-- Helpers

concatPair :: [([a],[b])] -> ([a],[b])
concatPair = (concat *** concat) . unzip

prettyPeriod :: A.Period -> String
prettyPeriod = T.prettyTime . A.period_dt

periodChanType :: A.Period -> ChanType
periodChanType = renderChanType . A.period_ty

