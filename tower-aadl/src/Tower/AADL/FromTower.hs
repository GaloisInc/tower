{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

--
-- Map the Tower AST into the AADL AST.
--
-- (c) 2014 Galois, Inc.
--

module Tower.AADL.FromTower
  ( fromTower
  ) where

import           Prelude hiding (init)
import           System.FilePath ((</>), addExtension)
import           Data.List (partition)
import           Data.Monoid (mconcat)

import qualified Ivory.Tower.AST as A
import qualified Ivory.Tower.AST.Graph as G
import qualified Ivory.Tower.Types.Time as T
import qualified Ivory.Tower.Types.Unique as U
import           Ivory.Tower.Codegen.Handler (callbackProcName)


import           Tower.AADL.AST
import           Tower.AADL.Config

import           Control.Arrow

--------------------------------------------------------------------------------

-- | Takes a name for the system, a Tower AST, and returns an AADL System AST.
fromTower :: Config -> A.Tower -> System
fromTower c t = System { .. }
  where
  systemName       = configSystemName c
  systemProperties = [ SystemOS (configSystemOS c)
                     , SystemHW (configSystemHW c) ]
  systemComponents = [mkProcess c t]

mkProcess :: Config -> A.Tower -> Process
mkProcess c t = Process { .. }
  where
  processName       = configSystemName c ++ "_process"
  processComponents = activeThreads ++ passiveThreads
  activeThreads     = map (fromThread t) (skipInitThread (A.towerThreads t))
  passiveThreads    = map (fromMonitor c t) (A.tower_monitors t)
  skipInitThread    = filter (("thread_init" /=) . A.threadName)

fromThread :: A.Tower -> A.Thread -> Thread
fromThread twr t = Thread { .. }
  where
  threadName       = A.threadName t
  threadComments   = []
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
  A.ChanSignal _s
    -> error $ "chanFeature " ++ show c
  A.ChanPeriod per
    -> let chanLabel = prettyPeriod per in
       let chanType  = A.period_ty per in
       Channel { .. }
  A.ChanInit _init
    -> error $ "chanFeature " ++ show c
  where
  c             = A.threadChan t
  chanHandle    = Output
  chanCallbacks = Prim (map A.monitorName chanMonitors)
  chanMonitors  = map fst (G.towerChanHandlers twr c)

fromMonitor :: Config -> A.Tower -> A.Monitor -> Thread
fromMonitor c t m = Thread { .. }
  where
  hs               = A.monitor_handlers m
  threadComments   = catHandlerSrcLocs
                       (concatMap A.handler_comments hs)
  (fs, ps)         = concatPair (map (fromHandler c t) hs)
  threadName       = A.monitorName m
  threadFeatures   = fs
  threadProperties = ps ++
    [ ThreadType Passive
    , DispatchProtocol Aperiodic

    -- XXX made up for now
    , ExecTime 10 100
    , StackSize 100
    , Priority 1
    ]

type Sym = String
type Src = String

-- | Create the feature groups and thread properties from a Tower handler. A
-- handler is a collection of emitters and callbacks associated with a single
-- input channel.
fromHandler :: Config
            -> A.Tower
            -> A.Handler
            -> ([Feature], [ThreadProperty])
fromHandler c t h = (cf:fs, ps)
  where
  (fs, ps) = concatPair (map fromEmitter (A.handler_emitters h))
  cf       = fromInputChan cbs (A.handler_chan h)
  cbs      = concatMap mkCbNames (A.handler_callbacks h)

  -- Use Tower's API to create a callback function symbol based on the callback
  -- (a `Unique`) and the Tower threads that call it. Those functions are
  -- defined in .c files named after their active threads.
  mkCbNames :: U.Unique -> [(Src, Sym)]
  mkCbNames cb = map (mkSrc &&& mkSym) cbThds
    where
    mkSrc thd = (flip addExtension) "c"
              $ configSrcsDir c
            </> A.threadUserCodeModName thd ++ A.threadName thd
    mkSym     = callbackProcName cb (A.handler_name h)
    cbThds    = G.handlerThreads t h

fromInputChan :: [(Src, Sym)] -> A.Chan -> Feature
fromInputChan callbacks c = case c of
  A.ChanSync s
    -> let chanType  = A.sync_chan_type  s in
       let chanLabel = syncChanLabel s in
       ChannelFeature (Channel { .. })
  A.ChanSignal sig
    -> error $ "fromInputChan " ++ show c
  A.ChanPeriod per
    -> let chanLabel = prettyPeriod per in
       let chanType  = A.period_ty per in
       ChannelFeature (Channel { .. })
  A.ChanInit{}
    -> error "Impossible ChanInit in fromInputChan."
  where
  chanHandle    = Input
  chanCallbacks = User callbacks

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
      chanType  = A.sync_chan_type s
  _ -> error $ "Impossible channel " ++ show c ++ " in emitterChan."
  where
  chanHandle    = Output
  chanCallbacks = Prim [proc]

syncChanLabel :: A.SyncChan -> String
syncChanLabel = ("sync_" ++) . show . A.sync_chan_label

--------------------------------------------------------------------------------
-- Helpers

concatPair :: [([a],[b])] -> ([a],[b])
concatPair = (concat *** concat) . unzip

prettyPeriod :: A.Period -> String
prettyPeriod = T.prettyTime . A.period_dt

-- concatenate source locations, then return all comments.
catHandlerSrcLocs :: [A.Comment] -> [A.Comment]
catHandlerSrcLocs cs = srcloc : strs
  where
  (strs, srclocs) =
    partition (\case {A.UserComment{} -> True; A.SourcePos{} -> False}) cs
  srcloc = A.SourcePos (mconcat (map (\(A.SourcePos sl) -> sl) srclocs))
