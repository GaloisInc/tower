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

import qualified Ivory.Tower.AST           as A
import qualified Ivory.Tower.AST.Graph     as G
import qualified Ivory.Tower.Types.Time    as T
import qualified Ivory.Tower.Types.Unique  as U
import           Ivory.Tower.Codegen.Handler (callbackProcName,handlerProcName)

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
  processComponents = concatMap (fromMonitor c t) (A.tower_monitors t)

fromMonitor :: Config -> A.Tower -> A.Monitor -> [Thread]
fromMonitor c t m = map (fromHandler c t) (A.monitor_handlers m)

-- | Create the feature groups and thread properties from a Tower handler. A
-- handler is a collection of emitters and callbacks associated with a single
-- input channel.
fromHandler :: Config
            -> A.Tower
            -> A.Handler
            -> Thread
fromHandler c t h = Thread { .. }
  where
  threadName       = U.showUnique (A.handler_name h)
  threadFeatures   = cfs ++ fs
  threadComments   = A.handler_comments h
  (fs, ps)         = concatPair (map fromEmitter (A.handler_emitters h))
  cfs              = fromInputChan cbs (A.handler_chan h)
  cbs              = concatMap mkCbNames (A.handler_callbacks h)
  (threadType, dispatch) =
    case A.handler_chan h of
      A.ChanSignal sig -- XXX address is a lie
          -> (Active, Signal (A.signal_name sig) 0xdeadbeef)
      A.ChanPeriod per
          -> ( Active, Periodic (T.toMicroseconds (A.period_dt per)))
      A.ChanInit{}
          -- XXX is Aperiodic right? We're ignoring init chans for now, anyways
          -> (Active, Aperiodic)
      A.ChanSync{}
          -> (Passive, Aperiodic)
  threadPriority =
    case threadType of
      Active  -> [Priority 1] -- XXX made up for now
      Passive -> []
  stackSize =
    case threadType of
      Active  -> [StackSize 100] -- XXX made up for now
      Passive -> []
  propertySrcText  =
    case threadType of
      Active
        -> [PropertySourceText (f, s)]
           where
           s  = handlerProcName h th
           f  = mkCFile c (A.threadName th)
           th =
             case A.handler_chan h of
               A.ChanSignal sig -> A.SignalThread sig
               A.ChanPeriod per -> A.PeriodThread per
               A.ChanInit i     -> A.InitThread   i
               A.ChanSync{}     -> error "Impossible in fromHandler"
      Passive
        -> []
  threadProperties =
      ThreadType threadType
    : DispatchProtocol dispatch
    : ExecTime 10 100 -- XXX made up for now
    : propertySrcText ++ stackSize ++ threadPriority ++ ps

  -- Use Tower's API to create a callback function symbol based on the callback
  -- (a `Unique`) and the Tower threads that call it. Those functions are
  -- defined in .c files named after their active threads.
  mkCbNames :: U.Unique -> [SourcePath]
  mkCbNames cb = map (mkSrc &&& mkSym) cbThds
    where
    mkSrc thd = mkCFile c (A.threadName thd)
    mkSym     = callbackProcName cb (A.handler_name h)
    cbThds    = G.handlerThreads t h

fromInputChan :: [SourcePath] -> A.Chan -> [Feature]
fromInputChan callbacks c = case c of
  A.ChanSync s
    -> let chanType  = A.sync_chan_type  s in
       let chanLabel = syncChanLabel s in
       [ChannelFeature (Channel { .. })]
  A.ChanSignal{}
    -> error $ "fromInputChan " ++ show c
  A.ChanPeriod{}
    -> [] -- Input chans from clocks are implicit in AADL.
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
  chanCallbacks = Prim proc

syncChanLabel :: A.SyncChan -> String
syncChanLabel = show . A.sync_chan_label

--------------------------------------------------------------------------------
-- Helpers

concatPair :: [([a],[b])] -> ([a],[b])
concatPair = (concat *** concat) . unzip

-- From a name, add the '.c' extension and file path. Relative to the AADL source path.
mkCFile :: Config -> FilePath -> FilePath
mkCFile c fp =
      ".."
  </> configSrcsDir c
  </> addExtension fp "c"
