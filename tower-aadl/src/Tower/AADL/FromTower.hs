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
import           Data.Monoid
import           Data.List (find)
import           Data.Maybe (isJust)
import           System.FilePath ((</>), addExtension)
import           Control.Applicative

import qualified Ivory.Tower.AST                as A
import qualified Ivory.Tower.Types.Unique       as U
import qualified Ivory.Tower.Types.Dependencies as D
import qualified Ivory.Tower.Types.Time         as T

import qualified Ivory.Language                 as I
import qualified Ivory.Artifact                 as R

import           Tower.AADL.AST
import           Tower.AADL.Config

--------------------------------------------------------------------------------

-- | Takes a name for the system, a Tower AST, and returns an AADL System AST.
fromTower :: Config -> A.Tower -> D.Dependencies -> System
fromTower c t d =
  System { systemName       = configSystemName c
         , systemComponents = [sc]
         , systemProperties = sps
         }
  where
  sps = [ SystemOS $ show $ configSystemOS c
        , SystemHW $ show $ configSystemHW c ]
  sc = mkProcess c t d

mkProcess :: Config
          -> A.Tower
          -> D.Dependencies
          -> Process
mkProcess c t d = Process { .. }
  where
  processName       = configSystemName c ++ "_process"
  processComponents =
    concatMap (fromMonitor c t d) (A.tower_monitors t)

fromMonitor :: Config
            -> A.Tower
            -> D.Dependencies
            -> A.Monitor
            -> [Thread]
fromMonitor c t d m =
  case A.monitor_external m of
    A.MonitorExternal
      -> [externalMonitor c t d m]
    A.MonitorDefined
      -> let nm = A.monitorName m in
         map (fromHandler c t d nm) (A.monitor_handlers m)

-- | Collapse all the handlers into a single AADL thread for AADL handlers.
externalMonitor :: Config
                -> A.Tower
                -> D.Dependencies
                -> A.Monitor
                -> Thread
externalMonitor c t d m =
  Thread
    { threadName       = nm
    , threadFeatures   = concat features
    , threadProperties = props
    , threadComments   = concatMap A.handler_comments hs
    }
  where
  nm = A.monitorName m
  features = map (fromExternalHandler c t nm) hs
  hs = A.monitor_handlers m
  props =
    [ External
    , DispatchProtocol Sporadic
    , Priority 10
    , StackSize 256
    , ThreadType Active
    , ExecTime 10 50
    , SourceText (depsSourceText c d)
    ]

-- Combine all the handlers into one AADL thread. Assume that for handlers
-- coming from defined components, their emitters go to external
-- components. Conversely, for handlers coming from external components, their
-- emitters go to defined components.
fromExternalHandler :: Config -> A.Tower -> String -> A.Handler -> [Feature]
fromExternalHandler c t monitorName h =
  if fromAbstractChan t ch
    -- Channel comes from external component. Omit the input portion and
    -- callback. Just list the emitters.
    then map OutputFeature mkOutFeatures
    -- Input portion of the channel comes from a defined component.
    else rxChan
  where
  es = map fromEmitter (A.handler_emitters h)
  mkOutFeatures = fst $ unzip es
  ch = A.handler_chan h
  rxChan =
    -- There is no source for external handlers---they're created.
    let cbs = map (\(_,b) -> ("",b)) (mkCallbacksHandler c h monitorName) in
    fromInputChan cbs ch

-- | Create the feature groups and thread properties from a Tower handler. A
-- handler is a collection of emitters and callbacks associated with a single
-- input channel.
fromHandler :: Config
            -> A.Tower
            -> D.Dependencies
            -> String
            -> A.Handler
            -> Thread
fromHandler c t d monitorName h =
  Thread
    { threadName       = A.handlerName h
    , threadFeatures   = rxChan ++ (map OutputFeature txChans)
    , threadProperties = thdProps
    , threadComments   = A.handler_comments h
    }
  where
  cbs      = mkCallbacksHandler c h monitorName
  es       = map fromEmitter (A.handler_emitters h)
  thdProps = threadProperties
  (txChans, bnds)  = unzip es
  sends            = SendEvents (zip txChans bnds)
  -- Create each callback symbol associated with the handler.
  rxChan           = fromInputChan cbs (A.handler_chan h)

  threadProperties =
        ThreadType threadType
      : DispatchProtocol dispatch
      : ExecTime 10 100 -- XXX made up for now
      : sends
      : propertySrcText
     ++ concat ([stackSize, threadPriority] <*> pure threadType)
  (threadType, dispatch) = handlerType t h

  propertySrcText :: [ThreadProperty]
  propertySrcText =
    -- Everyone gets the Tower modules
    case threadType of
      Passive
        -> [SourceText towerDeps]
      Active
        -> [ EntryPoint syms
           , SourceText $ fps `mappend` depsSourceText c d
           ]
    where
    (fps, syms) = unzip cbs
    towerDeps = depsSourceText c d

handlerType :: A.Tower -> A.Handler -> (ThreadType, DispatchProtocol)
handlerType t h
  | fromAbstractMonitor t h
  = (Active, Sporadic)
  | otherwise
  =
  case A.handler_chan h of
    A.ChanSignal sig -- XXX address is a lie
        -> (Active, Signal (A.signal_name sig) 0xdeadbeef)
    A.ChanPeriod per
        -> (Active, Periodic (T.toMicroseconds (A.period_dt per)))
    A.ChanInit{}
        -- XXX is Aperiodic right? We're ignoring init chans for now, anyways
        -> (Active, Aperiodic)
    A.ChanSync{}
        -> (Passive, Aperiodic)

-- XXX expensive to recompute. Compute once.
fromAbstractMonitor :: A.Tower -> A.Handler -> Bool
fromAbstractMonitor t h =
  isJust $ find (\h' -> A.handler_name h' == A.handler_name h) fromExts
  where
  ms = A.tower_monitors t
  extMs = filter (\m -> A.monitor_external m == A.MonitorExternal) ms
  extHs = concatMap A.monitor_handlers extMs
  fromExts = map snd $ concatMap (A.handlerOutboundHandlers t) extHs

-- For a given channel, see if it's source is abstract (i.e., a sync chan with
-- no caller).
--
-- XXX expensive to recompute.
fromAbstractChan :: A.Tower -> A.Chan -> Bool
fromAbstractChan _ A.ChanSignal{}   = False
fromAbstractChan _ A.ChanPeriod{}   = False
fromAbstractChan _ A.ChanInit{}     = False
fromAbstractChan t (A.ChanSync c)   = and $ do
  -- Get all the monitors
  m <- A.tower_monitors t
  -- Get all of m's handlers
  n <- A.monitor_handlers m
  -- For each handler, get all of the emitters
  e <- A.handler_emitters n
  -- For each emitter, get the sync chan it emits on
  let c' = A.emitter_chan e
  -- See if the channels match
  return $ c /= c'

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
  map (mkCFile c fileNm,) nms
  where
  nms = map U.showUnique (A.handler_callbacks h)

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
fromEmitter e =
    ( Output
        { outputLabel   = outputLabel
        , outputType    = outputType
        , outputEmitter = sym
        }
    , A.emitter_bound e
    )
  where
  sym = U.showUnique (A.emitter_name e)
  (outputLabel, outputType) =
    case A.emitter_chan e of
      s -> (show (A.sync_chan_label s), A.sync_chan_type s)

-- From a name, add the '.c' extension and file path. Relative to the AADL source path.
mkCFile :: Config -> FilePath -> FilePath
mkCFile c fp =
      configSrcsDir c
  </> addExtension fp "c"

depsSourceText :: Config -> D.Dependencies -> [FilePath]
depsSourceText c d =
     map (mkCFile c . I.moduleName) (D.dependencies_modules d)
  ++ map R.artifactFileName (D.dependencies_artifacts d)
