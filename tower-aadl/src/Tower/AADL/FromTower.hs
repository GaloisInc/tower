{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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
import           Control.Applicative

import qualified Ivory.Tower.AST            as A
import qualified Ivory.Tower.Types.Time     as T
import qualified Ivory.Tower.Types.Unique   as U

import           Tower.AADL.AST
import           Tower.AADL.CodeGen (callbackSym, emitterSym)
import           Tower.AADL.Config

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
  processComponents = concatMap (fromMonitor c) (A.tower_monitors t)

fromMonitor :: Config -> A.Monitor -> [Thread]
fromMonitor c m =
  map (fromHandler c (A.monitorName m)) (A.monitor_handlers m)

-- | Create the feature groups and thread properties from a Tower handler. A
-- handler is a collection of emitters and callbacks associated with a single
-- input channel.
fromHandler :: Config
            -> String
            -> A.Handler
            -> Thread
fromHandler c monitorName h = Thread { .. }
  where
  threadName       = U.showUnique (A.handler_name h)
  threadFeatures   = rxChan ++ map OutputFeature txChans
  threadComments   = A.handler_comments h
  es               = A.handler_emitters h
  (txChans, bnds)  = unzip $ map fromEmitter es
  sends            = SendEvents (zip txChans bnds)
  -- Create all the callback names for the handler.
  rxChan           = fromInputChan (mkCallbacksHandler c h monitorName)
                                   (A.handler_chan h)

  threadProperties =
      ThreadType threadType
    : DispatchProtocol dispatch
    : ExecTime 10 100 -- XXX made up for now
    : sends
    : concat (    [propertySrcText c h monitorName, stackSize, threadPriority]
              <*> pure threadType
             )
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

threadPriority :: ThreadType -> [ThreadProperty]
threadPriority threadType =
  case threadType of
    Active  -> [Priority 1] -- XXX made up for now
    Passive -> []

stackSize :: ThreadType -> [ThreadProperty]
stackSize threadType =
  case threadType of
    Active  -> [StackSize 100] -- XXX made up for now
    Passive -> []

mkCallbacksHandler :: Config -> A.Handler -> String -> [SourcePath]
mkCallbacksHandler c h fileNm =
  map (mkCFile c fileNm,) (map go (A.handler_callbacks h))
  where
  go cb = callbackSym cb (A.handler_name h)

propertySrcText :: Config -> A.Handler -> String -> ThreadType -> [ThreadProperty]
propertySrcText c h monitorName threadType =
  case threadType of
    Passive
      -> []
    Active
      -> [PropertySourceText (mkCallbacksHandler c h monitorName)]

-- Create the input callback names in the handler for a given channel.
fromInputChan :: [SourcePath] -> A.Chan -> [Feature]
fromInputChan callbacks c = case c of
  A.ChanSignal{}
    -> error $ "fromInputChan " ++ show c
  A.ChanPeriod{}
    -> [] -- Input chans from clocks are implicit in AADL.
  A.ChanInit{}
    -> error "Impossible ChanInit in fromInputChan."
  A.ChanSync s
    -> map mkInput callbacks
    where
    mkInput cb = InputFeature
               $ Input { inputLabel    = show (A.sync_chan_label s)
                       , inputType     = A.sync_chan_type s
                       , inputCallback = cb
                       }

-- | From an emitter, return its output channel and bound.
fromEmitter :: A.Emitter -> (Output, Bound)
fromEmitter e = (Output { .. }, bnd)
  where
  outputEmitter = emitterSym e
  bnd           = A.emitter_bound e
  (outputLabel, outputType) =
    case A.emitter_chan e of
      s -> (show (A.sync_chan_label s), A.sync_chan_type s)

-- From a name, add the '.c' extension and file path. Relative to the AADL source path.
mkCFile :: Config -> FilePath -> FilePath
mkCFile c fp =
      configSrcsDir c
  </> addExtension fp "c"
