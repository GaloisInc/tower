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
import           Data.Either (lefts, rights)
import           Data.Maybe (catMaybes)
import           System.FilePath ((</>), addExtension)

import qualified Ivory.Tower.AST                as A
import qualified Ivory.Tower.Types.Unique       as U
import qualified Ivory.Tower.Types.Time         as T

import qualified Ivory.Language.Syntax.Type as I

import           Tower.AADL.AST
import           Tower.AADL.Config
import           Tower.AADL.Priorities
import           Tower.AADL.Names
import           Tower.AADL.Threads

import Text.Show.Pretty
import Debug.Trace

----------------------------------------
-- Magic made up numbers

execTime :: (Integer, Integer)
execTime = (10, 100)

stackSize :: Integer
stackSize = 1000

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
  sps = [ SystemOS $ showOS $ configSystemOS c
        , SystemHW $ show   $ configSystemHW c ]
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
  c           = c' { configPriorities = mkPriorities at }
  processComponents = -- trace (show (threadsPeriodic thds)) $
       map (fromExternalMonitor c t    ) (ptThreadsExternal     pt)
    ++ map (fromExtHdlrMonitor  c      ) (ptThreadsFromExternal pt)
    ++ map (fromExtHdlrMonitor  c . snd) (ptThreadsFromExtPer   pt)
    ++ map (fromPassiveMonitor  c      ) (ptThreadsPassive      pt)
    -- Tower threads below
    ++ map (periodicThread      c      ) (atThreadsPeriodic    at)
    ++ map (fromInitThread      c      ) (atThreadsInit        at)
    ++ map (fromSignalThread    c      ) (atThreadsSignal      at)

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
     , outputType     = I.TyInt I.Int64 -- XXX generalize, correct?
     , outputEmitter  = signalEmitter s
     }
  props =
    [ ThreadType Active
    , DispatchProtocol Sporadic
    , ExecTime execTime
    , StackSize stackSize
    , Priority (getPriority nm (configPriorities c))
    ]

  -- events = zip (map outputLabel outs) bnds
  -- (outs, bnds) = unzip (fromEmitters h)
  -- cbs = mkCallbacksHandler c f h (threadFile m)


periodicThread :: AADLConfig
               -> A.Period
               -> Thread
periodicThread c p =
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

fromGenericMonitor :: AADLConfig
                   -> A.Monitor
                   -> [ThreadProperty]
                   -> Thread
fromGenericMonitor c m props =
  Thread
  { threadName       = nm
  , threadFeatures   = catMaybes (lefts handlerInputs) ++ handlerEmitters allEmitters
  , threadProperties = props
  , threadComments   = concatMap A.handler_comments handlers
  }
  where
  nm            = A.monitorName m
  handlers      = A.monitor_handlers m
  handlerInputs = map (fromInputChan c WithFile False m) handlers
  allEmitters   = concatMap fromEmitters handlers

fromInitThread :: AADLConfig
                -> String
                -> Thread
fromInitThread c i = undefined

-- fromInitMonitor :: AADLConfig
--                 -> A.Monitor
--                 -> Thread
-- fromInitMonitor c m = fromGenericMonitor c m props
--   where
--   nm = A.monitorName m
--   handlers = A.monitor_handlers m
--   handlerInputs = map (fromInputChan c WithFile True m) handlers
--   allEmitters = concatMap fromEmitters handlers
--   props =
--     [ ExecTime execTime
--     , SourceText initFps
--     , ThreadType Active
--     , DispatchProtocol Sporadic
--     , StackSize stackSize
--     , Priority (getPriority nm (configPriorities c))
--     , SendEvents $ zip (map outputLabel outs) bnds
--     ] ++ map InitProperty initSyms

--   inits = rights handlerInputs
--   (initFps, initSyms) = unzip
--                       $ concatMap initCallback
--                       $ inits
--   (outs,bnds) = unzip allEmitters

fromPassiveMonitor :: AADLConfig
                   -> A.Monitor
                   -> Thread
fromPassiveMonitor c m = fromGenericMonitor c m props
  where
  handlers = A.monitor_handlers m
  handlerInputs = map (fromInputChan c WithFile False m) handlers
  props =
    [ ThreadType Passive
    , DispatchProtocol Aperiodic
    , ExecTime execTime
    , SourceText initFps
    ]
    where
    (initFps, _) = unzip
                 $ concatMap initCallback
                 $ rights handlerInputs

handlerEmitters :: [(Output, a)] -> [Feature]
handlerEmitters allEmitters = map OutputFeature
                            $ fst
                            $ unzip
                            $ allEmitters

fromExtHdlrMonitor :: AADLConfig
                   -> A.Monitor
                   -> Thread
fromExtHdlrMonitor c m =
  Thread
  { threadName       = nm
  , threadFeatures   = catMaybes (lefts handlerInputs) ++ handlerEmitters allEmitters
  , threadProperties = props
  , threadComments   = concatMap A.handler_comments handlers
  }
  where
  nm = A.monitorName m
  handlers = A.monitor_handlers m
  handlerInputs = map (fromInputChan c WithFile True m) handlers
  allEmitters = concatMap fromEmitters handlers
  props =
    [ ExecTime execTime
    , SendEvents $ zip (map outputLabel outs) bnds
    , SourceText initFps
    , ThreadType Active
    , DispatchProtocol Sporadic
    , StackSize stackSize
    , Priority (getPriority nm (configPriorities c))
    ]
    where
    (initFps,_) = unzip
                $ concatMap initCallback
                $ rights handlerInputs
    (outs,bnds) = unzip allEmitters

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
  nm = A.monitorName m
  handlers = A.monitor_handlers m
  features = concatMap (fromExternalHandler c t m) handlers
  props =
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
    else catMaybes $ lefts [fromInputChan c NoFile False  m h]
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
              -> Bool
              -> A.Monitor
              -> A.Handler
              -> Either (Maybe Feature) Init
fromInputChan c f active m h =
  case A.handler_chan h of
    A.ChanSignal s
      -> Left
       $ Just
       $ InputFeature
       $ Input { inputId          = SignalChanId (fromIntegral (A.signal_number s))
               , inputLabel       = A.handlerName h
               , inputType        = I.TyInt I.Int64
               , inputCallback    = cbs
               , inputQueue       = Nothing
               , inputSendsEvents = events
               }
    A.ChanPeriod p
      -> Left
       $ Just
       $ InputFeature
       $ Input { inputId          = periodId p
               , inputLabel       = T.prettyTime (A.period_dt p)
               , inputType        = A.period_ty p
               , inputCallback    = cbs
               , inputQueue       = Nothing
               , inputSendsEvents = events
               }
    A.ChanSync s
      -> Left
       $ Just
       $ InputFeature
       $ Input { inputId          = SynchChanId (A.sync_chan_label s)
               , inputLabel       = A.handlerName h
               , inputType        = A.sync_chan_type s
               , inputCallback    = cbs
               , inputQueue       = if active then Just queueSize
                                      else Nothing
               , inputSendsEvents = events
               }
    A.ChanInit{}
      -> Right
       $ Init { initCallback = cbs
              , initOutput   = fromEmitters h
              }
  where
  events = zip (map outputLabel outs) bnds
  (outs, bnds) = unzip (fromEmitters h)
  cbs = mkCallbacksHandler c f h (threadFile m)

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
