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

import qualified Ivory.Tower.AST                as A
import qualified Ivory.Tower.Types.Unique       as U
import qualified Ivory.Tower.Types.Time         as T

import           Tower.AADL.AST
import           Tower.AADL.AST.Common (towerTime)
import           Tower.AADL.Config
import           Tower.AADL.Platform
import           Tower.AADL.Priorities
import           Tower.AADL.Names
import           Tower.AADL.Threads

-- XXX
--import Debug.Trace
--import Text.Show.Pretty

----------------------------------------
-- Magic made up numbers

execTime :: (Integer, Integer)
execTime = (10, 100)

stackSize :: Integer
stackSize = 1024

queueSize :: Integer
queueSize = 1000

----------------------------------------

-- | Takes a name for the system, a Tower AST, and returns an AADL System AST.
fromTower :: AADLConfig -> A.Tower -> System
fromTower c t =
  System { systemName       = configSystemName c
         , systemComponents = [sc]
         , systemProperties = sps
         }
  where
  sps = [ SystemOS   $ showOS $ configSystemOS   c
        , SystemHW   $ show   $ configSystemHW   c
        , SystemAddr          $ configSystemAddr c ]
  sc = mkProcess c t

-- The Haskell identifier for eChronos must have initial caps.
-- So we define this function make sure the case comes out correctly
showOS :: OS -> String
showOS CAmkES   = "CAmkES"
showOS EChronos = "eChronos"

mkProcess :: AADLConfig
          -> A.Tower
          -> Process
mkProcess c' t = Process { .. }
  where
  processName = configSystemName c ++ "_process"
  pt = toPassiveThreads t
  at = toActiveThreads t
  mkPs = case configSystemOS c of
         CAmkES   -> mkSeL4Priorities
         EChronos -> mkEChronosPriorities
  c  = c' { configPriorities = mkPs at }
  processComponents =
           (fromInitThread               c  ) (atThreadsInit     at    )
    ++ map (fromPeriodicThread           c  ) (atThreadsPeriodic at    )
    ++ map (fromSignalThread             c  ) (atThreadsSignal   at    )
    ++ map (fromExternalMonitor          c t) (atThreadsExternal at    )
    ++ map (fromFromExternalOrPerMonitor c  ) (map (,emptyHMap) (atThreadsFromPeriodic at))
    ++ map (fromFromExternalOrPerMonitor c  ) (atThreadsFromExternal at)
    ++ map (fromPassiveMonitor           c  ) (ptThreadsPassive  pt    )

fromInitThread :: AADLConfig
               -> HasInit
               -> [Thread]
fromInitThread _c  NoInit  = []
fromInitThread  c  HasInit =
  [ Thread
    { threadName       = nm
    , threadFeatures   = [fs]
    , threadProperties = props
    , threadComments   = []
    }
  ]
  where
  nm = A.threadName (A.InitThread A.Init)
  fs = OutputFeature
     $ Output
     { outputId       = InitChanId systemInit
     , outputLabel    = systemInit
     , outputType     = towerTime
     , outputEmitter  = initEmitter
     }
  props =
    [ ThreadType Active
    , DispatchProtocol Sporadic
    , ExecTime execTime
    , SendEvents [(systemInit, 1)]
    , StackSize stackSize
    , Priority (getPriority nm (configPriorities c))
    , EntryPoint [initCallback]
    , SourceText [mkCFile c initCallback]
    , InitProperty initCallback
    ]

fromSignalThread :: AADLConfig
                 -> A.Signal
                 -> Thread
fromSignalThread c s =
  Thread
  { threadName       = nm
  , threadFeatures   = [sf,fs]
  , threadProperties = props
  , threadComments   = []
  }
  where
  nm = A.threadName (A.SignalThread s)
  sf = SignalFeature
     $ SignalInfo { signalInfoNumber      = A.signal_number s
                  , signalInfoName        = A.signal_name   s
                  , signalInfoSendsEvents = [(A.signal_name s, 1)]
                  , signalInfoCallback    = [(mkCFile c (signalCallback s), signalCallback s)]
                  }
  fs = OutputFeature
     $ Output
     { outputId       = SignalChanId (fromIntegral (A.signal_number s))
     , outputLabel    = A.signal_name s
     , outputType     = towerTime
     , outputEmitter  = signalEmitter s
     }
  props =
    [ ThreadType Active
    , DispatchProtocol Sporadic
    , ExecTime execTime
    , StackSize stackSize
    , Priority (getPriority nm (configPriorities c))
    ]

fromPeriodicThread :: AADLConfig
                   -> A.Period
                   -> Thread
fromPeriodicThread c p =
  Thread
  { threadName       = nm
  , threadFeatures   = [fs]
  , threadProperties = props
  , threadComments   = []
  }
  where
  nm = A.threadName (A.PeriodThread p)
  fs = OutputFeature
     $ Output
     { outputId       = periodId p
     , outputLabel    = prettyTime p
     , outputType     = A.period_ty p
     , outputEmitter  = periodicEmitter p
     }
  props =
    [ ThreadType Active
    , DispatchProtocol (Periodic (getPeriod p))
    , ExecTime execTime
    , SendEvents [(prettyTime p, 1)]
    , StackSize stackSize
    , Priority (getPriority nm (configPriorities c))
    , EntryPoint [periodicCallback p]
    , SourceText [mkCFile c (periodicCallback p)]
    ]

fromPassiveMonitor :: AADLConfig
                   -> A.Monitor
                   -> Thread
fromPassiveMonitor c m =
  Thread
  { threadName       = nm
  , threadFeatures   = handlerInputs ++ handlerEmitters allEmitters
  , threadProperties = props
  , threadComments   = concatMap A.handler_comments handlers
  }
  where
  nm            = A.monitorName m
  handlers      = A.monitor_handlers m
  handlerInputs = map (fromInputChan c WithFile m emptyHMap) handlers
  allEmitters   = concatMap fromEmitters handlers

--  handlerInputs = map (fromInputChan c WithFile m) handlers
  props =
    [ ThreadType Passive
    , DispatchProtocol Aperiodic
    , ExecTime execTime
    , SourceText []
    ]

-- Handles messages from either a periodic thread or an external thread.
fromFromExternalOrPerMonitor :: AADLConfig
                             -> (A.Monitor, HMap)
                             -> Thread
fromFromExternalOrPerMonitor c (m,hmap) =
  Thread
  { threadName       = nm
  , threadFeatures   = handlerInputs ++ handlerEmitters allEmitters
  , threadProperties = props
  , threadComments   = concatMap A.handler_comments handlers
  }
  where
  nm            = A.monitorName m
  handlers      = A.monitor_handlers m
  handlerInputs = map (fromInputChan c WithFile m hmap) handlers
  allEmitters   = concatMap fromEmitters handlers

--  handlerInputs = map (fromInputChan c WithFile m) handlers
  props =
    [ ThreadType Active
    , DispatchProtocol Sporadic
    , ExecTime execTime
    , SourceText []
    , StackSize stackSize
    , Priority (getPriority nm (configPriorities c))
    ]

handlerEmitters :: [(Output, a)] -> [Feature]
handlerEmitters allEmitters = map OutputFeature
                            $ fst
                            $ unzip
                            $ allEmitters

-- fromExtHdlrMonitor :: AADLConfig
--                    -> A.Monitor
--                    -> Thread
-- fromExtHdlrMonitor c m =
--   Thread
--   { threadName       = nm
--   , threadFeatures   = handlerInputs ++ handlerEmitters allEmitters
--   , threadProperties = props
--   , threadComments   = concatMap A.handler_comments handlers
--   }
--   where
--   nm = A.monitorName m
--   handlers = A.monitor_handlers m
--   handlerInputs = map (fromInputChan c WithFile m emptyHMap) handlers
--   allEmitters = concatMap fromEmitters handlers
--   props =
--     [ ExecTime execTime
--     , SendEvents $ zip (map outputLabel outs) bnds
--     , ThreadType Active
--     , DispatchProtocol Sporadic
--     , StackSize stackSize
--     , Priority (getPriority nm (configPriorities c))
--     ]
--     where
--     (outs,bnds) = unzip allEmitters

fromExternalMonitor :: AADLConfig
                    -> A.Tower
                    -> A.Monitor
                    -> Thread
fromExternalMonitor c t m =
  Thread
    { threadName       = nm
    , threadFeatures   = features
    , threadProperties = props
    , threadComments   = concatMap A.handler_comments handlers
    }
  where
  nm       = A.monitorName m
  handlers = A.monitor_handlers m
  features = concatMap (fromExternalHandler c t m) handlers
  props    =
    [ External
    , DispatchProtocol Sporadic
    , ThreadType Active
    , ExecTime execTime
    , SourceText [] -- necessary, aadl2rtos crashes w/out it.
    ]

-- Assume that for handlers coming from defined components, their emitters go to
-- external components. Conversely, for handlers coming from external
-- components, their emitters go to defined components.
fromExternalHandler :: AADLConfig -> A.Tower -> A.Monitor -> A.Handler -> [Feature]
fromExternalHandler c t m h =
  if fromAbstractChan t ch
    -- Channel comes from external component. Omit the input portion and
    -- callback. Just list the emitters.
    then map OutputFeature mkOutFeatures
    -- Input portion of the channel comes from a defined component.
    else [fromInputChan c NoFile m emptyHMap h]
  where
  mkOutFeatures = fst $ unzip $ fromEmitters h
  ch = A.handler_chan h

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
              -> A.Monitor
              -> HMap
              -> A.Handler
              -> Feature
fromInputChan c f m hmap h = InputFeature $
  case A.handler_chan h of
    A.ChanSignal s
      -> Input { inputId          = SignalChanId (fromIntegral (A.signal_number s))
               , inputLabel       = A.handlerName h
               , inputType        = towerTime
               , inputCallback    = cbs
               , inputSendsEvents = events
               , inputQueue       = q
               }
    A.ChanPeriod p
      -> Input { inputId          = periodId p
               , inputLabel       = A.handlerName h
               , inputType        = A.period_ty p
               , inputCallback    = cbs
               , inputSendsEvents = events
               , inputQueue       = q
               }
    A.ChanSync s
      -> Input { inputId          = SynchChanId (A.sync_chan_label s)
               , inputLabel       = A.handlerName h
               , inputType        = A.sync_chan_type s
               , inputCallback    = cbs
               , inputSendsEvents = events
               , inputQueue       = q
               }
    A.ChanInit{}
      -> Input { inputId          = InitChanId systemInit
               , inputLabel       = A.handlerName h
               , inputType        = towerTime
               , inputCallback    = cbs
               , inputSendsEvents = events
               , inputQueue       = q
               }
  where
  q = mkInputQueue hmap h
  events = zip (map outputLabel outs) bnds
  (outs, bnds) = unzip (fromEmitters h)
  cbs = mkCallbacksHandler c f h (threadFile m)

mkInputQueue :: HMap -> A.Handler -> Maybe Integer
mkInputQueue hmap h
  | fromHMap hmap h
  = Just queueSize
  | otherwise
  = Nothing

getPeriod :: A.Period -> Integer
getPeriod p =
  case A.period_dt p of
    T.Microseconds t -> t

periodId :: A.Period -> ChanId
periodId = PeriodChanId . getPeriod

fromEmitters :: A.Handler -> [(Output, Bound)]
fromEmitters h =
  zipWith fromEmitter ids (A.handler_emitters h)
  where
  ids = map (\i -> A.handlerName h ++ '_': show i) [0::Int ..]

-- | From an emitter, return its output channel and bound.
fromEmitter :: String -> A.Emitter -> (Output, Bound)
fromEmitter label e =
    ( Output
        { outputId      = SynchChanId (A.sync_chan_label (A.emitter_chan e))
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

