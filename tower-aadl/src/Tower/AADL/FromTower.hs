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
import           Control.Monad

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
  processComponents = concatMap (fromMonitor c t) (A.tower_monitors t)

fromMonitor :: Config -> A.Tower -> A.Monitor -> [Thread]
fromMonitor c t m =
  map (fromHandler c t (A.monitorName m)) (A.monitor_handlers m)

-- | Create the feature groups and thread properties from a Tower handler. A
-- handler is a collection of emitters and callbacks associated with a single
-- input channel.
fromHandler :: Config
            -> A.Tower
            -> String
            -> A.Handler
            -> Thread
fromHandler c t monitorName h = Thread { .. }
  where
  threadName       = U.showUnique (A.handler_name h)
  threadFeatures   = rxChan ++ map OutputFeature txChans
  threadComments   = A.handler_comments h
  es               = A.handler_emitters h
  (txChans, bnds)  = unzip $ map fromEmitter es
  sends            = SendEvents (zip txChans bnds)
  -- Create each callback symbol associated with the handler.
  cbs              = mkCallbacksHandler c h monitorName
  rxChan           = fromInputChan cbs (A.handler_chan h)

  threadProperties =
      ThreadType threadType
    : DispatchProtocol dispatch
    : ExecTime 10 100 -- XXX made up for now
    : sends
    : (if isAbstract h then [External] else propertySrcText)
   ++ concat ([stackSize, threadPriority] <*> pure threadType)
  (threadType, dispatch) =
    if isAbstract h then (Active, Aperiodic)
      else
        case A.handler_chan h of
          A.ChanSignal sig -- XXX address is a lie
              -> (Active, Signal (A.signal_name sig) 0xdeadbeef)
          A.ChanPeriod per
              -> ( Active, Periodic (T.toMicroseconds (A.period_dt per)))
          A.ChanInit{}
              -- XXX is Aperiodic right? We're ignoring init chans for now, anyways
              -> (Active, Aperiodic)
          A.ChanSync{}
              -> ( -- abstract handlers affect dependents
                   if fromAbstract t h
                     then Active
                     else Passive
                 , Aperiodic
                 )
  propertySrcText :: [ThreadProperty]
  propertySrcText =
    case threadType of
      Passive
        -> []
      Active
        -> [PropertySourceText (mkCallbacksHandler c h monitorName)]

isAbstract :: A.Handler -> Bool
isAbstract h =
  case A.handler_type h of
    A.AbstractHandler
      -> True
    A.DefinedHandler
      -> False

-- For a given handler, sees if any callback emitting onto it comes from an
-- abstract handler.
--
-- XXX this may be expensive to run for each handler. May want to compute all
-- affected handlers up front.
fromAbstract :: A.Tower -> A.Handler -> Bool
fromAbstract t h = or $ do
  -- Get the channel
  let c = A.handler_chan h
  -- Get all the monitors
  m <- A.tower_monitors t
  -- Get all of m's handlers
  n <- A.monitor_handlers m
  -- Filter the abstract ones
  guard (A.handler_type n == A.AbstractHandler)
  -- For each handler, get all of the emitters
  e <- A.handler_emitters n
  -- For each emitter, get the sync chan it emits on
  let s = A.emitter_chan e
  -- See if the channels match
  return $ case c of
             A.ChanSync s' -> s == s'
             _             -> False

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
  if isAbstract h
    then map (Nothing,)          (map mkName (A.handler_callbacks h))
    else map (Just (mkCFile c fileNm),) (map mkName (A.handler_callbacks h))
  where
  mkName cb = callbackSym cb (A.handler_name h)

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
