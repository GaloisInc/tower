{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import           Data.Maybe (catMaybes, isJust)
import           Data.Either
import           Data.List (find)
import           System.FilePath ((</>), addExtension)

import qualified Ivory.Tower.AST                as A
import qualified Ivory.Tower.Types.Unique       as U
import qualified Ivory.Tower.Types.Time         as T

import           Tower.AADL.AST
import           Tower.AADL.Config
import           Tower.AADL.Names


----------------------------------------
-- Magic made up numbers

execTime :: (Integer, Integer)
execTime = (10, 100)

stackSize :: Integer
stackSize = 1000

queueSize :: Integer
queueSize = 1000

priority :: Priority
priority = 11

----------------------------------------

-- | Takes a name for the system, a Tower AST, and returns an AADL System AST.
fromTower :: AADLConfig -> A.Tower -> System
fromTower c t =
  System { systemName       = configSystemName c
         , systemComponents = [sc]
         , systemProperties = sps
         }
  where
  sps = [ SystemOS $ show $ configSystemOS c
        , SystemHW $ show $ configSystemHW c ]
  sc = mkProcess c t

mkProcess :: AADLConfig
          -> A.Tower
          -> Process
mkProcess c t = Process { .. }
  where
  processName       = configSystemName c ++ "_process"
  processComponents =
       activeMonitors c t
    ++ map (fromMonitor c t) (A.tower_monitors t)

activeMonitors :: AADLConfig
               -> A.Tower
               -> [Thread]
activeMonitors c t =
  catMaybes $ map (activeMonitor c) (A.towerThreads t)

activeMonitor :: AADLConfig
              -> A.Thread
              -> Maybe Thread
activeMonitor c t =
  case t of
    A.PeriodThread p
      -> Just $ Thread
         { threadName       = A.threadName t
         , threadFeatures   = [fs p]
         , threadProperties = props p
         , threadComments   = []
         }
    _ -> Nothing
  where
  fs p = OutputFeature
       $ Output
       { outputId       = periodId p
       , outputLabel    = prettyTime p
       , outputType     = A.period_ty p
       , outputEmitter  = periodicEmitter p
       }
  props p =
    [ ThreadType Active
    , DispatchProtocol (Periodic (periodId p))
    , ExecTime execTime
    , SendEvents [(prettyTime p, 1)]
    , StackSize stackSize
    , Priority priority
    , EntryPoint [periodicCallback p]
    , SourceText [mkCFile c (periodicCallback p)]
    ]

fromMonitor :: AADLConfig
            -> A.Tower
            -> A.Monitor
            -> Thread
fromMonitor c t m =
  case A.monitor_external m of
    A.MonitorExternal
      -> externalMonitor c t m
    A.MonitorDefined
      -> fromDefinedMonitor c t m

fromDefinedMonitor :: AADLConfig
                   -> A.Tower
                   -> A.Monitor
                   -> Thread
fromDefinedMonitor c t m =
  Thread
  { threadName       = A.monitorName m
  , threadFeatures   = lefts handlerInputs ++ handlerEmitters
  , threadProperties = props
  , threadComments   = concatMap A.handler_comments handlers
  }
  where
  handlers = A.monitor_handlers m
  handlerInputs = map (fromInputChan c WithFile (activeProp props) m) handlers
  handlerEmitters = map OutputFeature
                  $ fst
                  $ unzip
                  $ allEmitters
  allEmitters = concatMap fromEmitters handlers
  props = props' ++
    [ ExecTime execTime
    , SendEvents $ zip (map outputLabel outs) bnds
    , SourceText initFps
    ]
    where
    (initFps, initSyms) = unzip
                        $ concatMap initCallback
                        $ rights handlerInputs
    activeProps =
      [ ThreadType Active
      , DispatchProtocol Sporadic
      , StackSize stackSize
      , Priority priority
      ]
    initProps = activeProps ++ map InitProperty initSyms
    props'
      | any (fromExternalMonitor t) handlers
      = activeProps
      | any fromInit handlers
      = initProps
      | otherwise
      = [ ThreadType Passive, DispatchProtocol Aperiodic ]
    (outs,bnds) = unzip allEmitters

-- | Collapse all the handlers into a single AADL thread for AADL handlers.
externalMonitor :: AADLConfig
                -> A.Tower
                -> A.Monitor
                -> Thread
externalMonitor c t m =
  Thread
    { threadName       = A.monitorName m
    , threadFeatures   = features
    , threadProperties = props
    , threadComments   = concatMap A.handler_comments handlers
    }
  where
  handlers = A.monitor_handlers m
  features = concatMap (fromExternalHandler c t m) handlers
  props =
    [ External
    , DispatchProtocol Sporadic
    , Priority priority
    , StackSize stackSize
    , ThreadType Active
    , ExecTime execTime
    , SourceText [] -- necessary, aadl2rtos crashes w/out it.
    ]

fromInit :: A.Handler -> Bool
fromInit h =
  case A.handler_chan h of
    A.ChanInit{} -> True
    _            -> False

-- Combine all the handlers into one AADL thread. Assume that for handlers
-- coming from defined components, their emitters go to external
-- components. Conversely, for handlers coming from external components, their
-- emitters go to defined components.
fromExternalHandler :: AADLConfig -> A.Tower -> A.Monitor -> A.Handler -> [Feature]
fromExternalHandler c t m h =
  if fromAbstractChan t ch
    -- Channel comes from external component. Omit the input portion and
    -- callback. Just list the emitters.
    then map OutputFeature mkOutFeatures
    -- Input portion of the channel comes from a defined component.
    else lefts [fromInputChan c NoFile False  m h]
  where
  mkOutFeatures = fst $ unzip $ fromEmitters h
  ch = A.handler_chan h

-- Computes whether a handler handles a message sent from an external monitor.
-- XXX expensive to recompute. Compute once?
fromExternalMonitor :: A.Tower -> A.Handler -> Bool
fromExternalMonitor t h =
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

data Files = WithFile | NoFile
  deriving (Show, Read, Eq)

mkCallbacksHandler :: AADLConfig -> Files -> A.Handler -> String -> [SourcePath]
mkCallbacksHandler c f h fileNm =
  case f of
    WithFile -> map (mkCFile c fileNm,) nms
    NoFile   -> map ("",) nms
  where
  nms = map U.showUnique (A.handler_callbacks h)

fromInputChan :: AADLConfig
              -> Files
              -> Bool
              -> A.Monitor
              -> A.Handler
              -> Either Feature Init
fromInputChan c f active m h =
  case A.handler_chan h of
    A.ChanSignal{}
      -> error "fromInputChan: Singal"
    A.ChanPeriod p
      -> Left
       $ InputFeature
       $ Input { inputId       = periodId p
               , inputLabel    = T.prettyTime (A.period_dt p)
               , inputType     = A.period_ty p
               , inputCallback = cbs
               , inputQueue    = Nothing
               }
    A.ChanInit{}
      -> Right
       $ Init { initCallback = cbs }
    A.ChanSync s
      -> Left
       $ InputFeature
       $ Input { inputId       = A.sync_chan_label s
               , inputLabel    = A.handlerName h
               , inputType     = A.sync_chan_type s
               , inputCallback = cbs
               , inputQueue    = if active then Just queueSize
                                   else Nothing
               }
  where
  cbs = mkCallbacksHandler c f h (threadFile m)

periodId :: A.Period -> ChanId
periodId p =
  case A.period_dt p of
    T.Microseconds t -> t

fromEmitters :: A.Handler -> [(Output, Bound)]
fromEmitters h =
  zipWith fromEmitter ids (A.handler_emitters h)
  where
  ids = map (\i -> A.handlerName h ++ '_': show i) [0::Int ..]

-- | From an emitter, return its output channel and bound.
fromEmitter :: String -> A.Emitter -> (Output, Bound)
fromEmitter label e =
    ( Output
        { outputId      = A.sync_chan_label (A.emitter_chan e)
        , outputLabel   = label
        , outputType    = outputType
        , outputEmitter = sym
        }
    , A.emitter_bound e
    )
  where
  sym = U.showUnique (A.emitter_name e)
  outputType = A.sync_chan_type $ A.emitter_chan e

-- From a name, add the '.c' extension and file path. Relative to the AADL source path.
mkCFile :: AADLConfig -> FilePath -> FilePath
mkCFile c fp =
      configSrcsDir c
  </> addExtension fp "c"

activeProp :: [ThreadProperty] -> Bool
activeProp = elem (ThreadType Active)
