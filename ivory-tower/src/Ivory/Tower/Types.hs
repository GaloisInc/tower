{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Tower.Types where

import MonadLib
import Data.Monoid

import Ivory.Language

-- Name ------------------------------------------------------------------------

-- | 'Name' is a synonym for an ordinary 'String'. Names should always be valid
--    C identifiers.
type Name = String

-- | 'Labeled' is used internally to pair a value with a String.
data Labeled a = Labeled a String deriving (Show)

instance Functor Labeled where
  fmap f (Labeled a s) = Labeled (f a) s

-- | unwraps a 'Labeled' value
unLabeled :: Labeled a -> a
unLabeled (Labeled a _) = a

-- Channel Types ---------------------------------------------------------------

-- | Channels are communication primitives in Tower which are scheduled for
--   best-effort asynchronous communication. We'll be working on better
--   guarantees on scheduling soon...

-- | The basic reference type underlying all Channels. UT stands for untyped,
--   this reference type is used when an 'Ivory.Language.Area' type parameter
--   is not needed.
data UTChannelRef = UTChannelRef { unUTChannelRef :: Name } deriving (Eq)

instance Show UTChannelRef where
  show utr = "UTChannelRef " ++ (show (unUTChannelRef utr))

-- | The typed Channel reference type, paramaterized by a type of kind
--   'Ivory.Language.Area'. A simple wrapper on 'UTChannelRef'
data ChannelRef (area :: Area) = ChannelRef { unChannelRef :: UTChannelRef
      } deriving (Eq)

-- | A wrapper on 'ChannelRef' used designating a Source, the end of a Channel
--   which is written to. The only valid operation on a 'ChannelSource' is
--   'Ivory.Tower.Tower.withChannelEmitter'
newtype ChannelSource area = ChannelSource { unChannelSource :: ChannelRef area}

newtype ChannelEmitter area = ChannelEmitter { unChannelEmitter :: ChannelRef area}

-- | A wrapper on 'ChannelRef' used designating a Sink, the end of a Channel
--   which is read from. The only valid operation on a 'ChannelSink' is
--   'Ivory.Tower.Tower.withChannelReceiver' 
newtype ChannelSink area   = ChannelSink   { unChannelSink   :: ChannelRef area}

newtype ChannelReceiver area = ChannelReceiver { unChannelReceiver :: ChannelRef area}

-- Compiled Connectors --------------------------------------------------------
-- XXX this is a fucking mess, fix it
-- | Internal to Tower and 'Ivory.Tower.Compile' implementations
data CompiledChannelName
  = ChannelName
      { ccn_utchref :: UTChannelRef
      , ccn_endpoint :: Name
      }
  | DataPortName
     { ccn_dataportname :: Name }
  deriving (Eq, Show)

-- | Internal to Tower and 'Ivory.Tower.Compile' implementations
compiledChannelName :: CompiledChannelName -> String
compiledChannelName (ChannelName r e) = (unUTChannelRef r) ++ "_endpoint_" ++ e
compiledChannelName (DataPortName n)  = n

-- Dataport Types --------------------------------------------------------------

-- | Underlying type of all Tower Data. DataPorts are communication primitives
--   in Tower which have no impact on the schedule - they are essentially just
--   shared state.
data DataPort area =
  DataPort
    { data_name  :: Name
    , data_read  :: forall s eff cs . (eff `AllocsIn` cs)
                 => Ref s area -> Ivory eff ()
    , data_write :: forall s eff cs . (eff `AllocsIn` cs)
                 => ConstRef s area -> Ivory eff ()
    , data_cch   :: CompiledChannel
    }

-- | A wrapper on 'DataPort' used designating a Source, the end of a 'DataPort'
--   which is written to. The only valid operation on 'DataSource' is
--   'Ivory.Tower.Tower.withDataWriter'
newtype DataSource area = DataSource { unDataSource :: (DataPort area) }

-- | A wrapper on 'DataPort' used designating a Sink, the end of a 'DataPort'
--   which is read from. The only valid operation on 'DataSink' is
--   'Ivory.Tower.Tower.withDataReader'
newtype DataSink area   = DataSink   { unDataSink   :: (DataPort area) }

-- | An implementation of a reader on a channel. The only valid operation on
--   a 'DataReader' is 'Ivory.Tower.DataPort.readData', which unpacks the
--   implementation into the correct 'Ivory.Language.Ivory' effect scope.
newtype DataReader area = DataReader { unDataReadable :: (DataPort area) }

-- | An implementation of a writer on a channel. The only valid operation on
--   a 'DataWriter' is 'Ivory.Tower.DataPort.writeData', which unpacks the
--   implementation into the correct 'Ivory.Language.Ivory' effect scope.
newtype DataWriter area = DataWriter { unDataWritable :: (DataPort area) }

-- EventLoop types -------------------------------------------------------------

-- | Internal only. Wrapped up to make it easier to deal with the types.
data TaskBody = TaskBody { unTaskBody :: forall eff cs . (eff `AllocsIn` cs)
                                      => (Ivory eff (EventLoop eff))}

-- | Internal only - a single event handler'Ivory.Tower.EventLoop.onChannel'
--   and 'Ivory.Tower.EventLoop.onTimer' wrapped into data.
data EventLoopImpl eff
 =  forall area cs . (eff `AllocsIn` cs, IvoryType area, IvoryZero area) =>
    EventLoopChannel
      (ChannelReceiver area)
      (ConstRef (Stack cs) area -> Ivory eff ())

 | forall cs . (eff `AllocsIn` cs) =>
   EventLoopPeriod
    Integer (Uint32 -> Ivory eff ())

-- | Pairs of events and handlers. Event handling will be multiplexed into an
--   task loop, see 'Ivory.Tower.Tower.taskBody'.
--   Combine EventLoops to be scheduled as part of the same task loop using
--   'Data.Monoid'
newtype EventLoop eff = EventLoop { unEventLoop :: [EventLoopImpl eff] }

instance Monoid (EventLoop eff) where
  mempty = EventLoop []
  mappend el1 el2 = EventLoop ((unEventLoop el1) ++ (unEventLoop el2))

-- | internal only
data CompiledChannel =
  CompiledChannel
    { cch_name        :: CompiledChannelName
    , cch_initializer :: Def('[]:->())
    , cch_moddefs     :: ModuleDef
    , cch_type        :: String
    }

instance Eq CompiledChannel where
  -- Invariant: only compare valid compiled channels created under the same tower
  -- base monad. (hope this works out?)
  c1 == c2 = (cch_name c1) == (cch_name c2)

instance Show CompiledChannel where
  show cc = "CompiledChannel " ++ (show (cch_name cc))

-- | internal only
newtype CompiledTaskBody =
  CompiledTaskBody { unCompiledTaskBody :: Def('[]:->()) }

-- Period ----------------------------------------------------------------------
-- | Wrapper type for periodic schedule, created using
-- 'Ivory.Tower.Tower.withPeriod'
newtype Period = Period { unPeriod :: Integer }

-- Get Time Millis -------------------------------------------------------------
-- | Wrapper type for OS get time millis implementation , created using
-- 'Ivory.Tower.Tower.withPeriod'
newtype OSGetTimeMillis =
  OSGetTimeMillis { unOSGetTimeMillis :: forall eff . Ivory eff Uint32 }

-- Task State-------------------------------------------------------------------

-- | Internal only: this is the result of a complete 'Ivory.Tower.Monad.Task'
--   context. The 'OS' implementation will use this to create a 'TowerSchedule'
data TaskSt =
  TaskSt
    { taskst_name        :: Name
    , taskst_emitters    :: [Labeled UTChannelRef]
    , taskst_receivers   :: [Labeled UTChannelRef]
    , taskst_datareaders :: [Labeled CompiledChannel] -- xxx fix these CompiledChannels
    , taskst_datawriters :: [Labeled CompiledChannel]
    , taskst_periods     :: [Integer]
    , taskst_stacksize   :: Maybe Integer
    , taskst_priority    :: Maybe Integer
    , taskst_moddef      :: ModuleDef
    , taskst_extern_mods :: [Module]
    }

-- | Internal only: basis for 'Ivory.Tower.Monad.Task' runner
emptyTaskSt :: Name -> TaskSt
emptyTaskSt n = TaskSt
  { taskst_name        = n
  , taskst_emitters    = []
  , taskst_receivers   = []
  , taskst_datareaders = []
  , taskst_datawriters = []
  , taskst_periods     = []
  , taskst_stacksize   = Nothing
  , taskst_priority    = Nothing
  , taskst_moddef      = return ()
  , taskst_extern_mods = []
  }

-- Tower State -----------------------------------------------------------------

data TowerSt =
  TowerSt
    { towerst_modules   :: [Module]
    , towerst_dataports :: [CompiledChannel] -- XXX fix these
    , towerst_channels  :: [UTChannelRef]
    , towerst_tasksts   :: [TaskSt]
    }

emptyTowerSt :: TowerSt
emptyTowerSt = TowerSt
  { towerst_modules = []
  , towerst_dataports = []
  , towerst_channels = []
  , towerst_tasksts = []
  }

-- Compiled Schedule -----------------------------------------------------------

-- | Internal only: produced by the 'OS', used for evaluating each 'Scheduled'
--   context
data TowerSchedule =
  TowerSchedule
    { scheduleEmitter :: forall area . (IvoryType area)
           => ChannelRef area -> ChannelEmitter area
    , scheduleTaskBody :: TaskSt -> TaskBody -> CompiledTaskBody
    , scheduleInitializer :: Def ('[]:->())
    , scheduleModuleDef :: ModuleDef
    }

-- Operating System ------------------------------------------------------------

-- | Backend for targeting Tower to a particular Operating System. Should be
--   implemented by a module in 'Ivory.Tower.Compile'.
data OS =
  OS
    { osDataPort      :: forall area . (IvoryType area)
                      => Name -- Unique dataport name
                      -> DataPort area
    , osGetTimeMillis :: forall eff  . Ivory eff Uint32
    , osSchedule      :: [UTChannelRef] -> [TaskSt] -> TowerSchedule
    , osCreateChannel :: forall area . (IvoryType area)
                      => ChannelRef area
                      -> CompiledChannelName
                      -> (ChannelReceiver area)
    }

-- Monad Types -----------------------------------------------------------------

-- | Tower monad: context for instantiating connections (channels & dataports)
--   and tasks.
newtype Tower a = Tower
  { unTower :: StateT TowerSt Base a
  } deriving (Functor, Monad)

-- | Task monad: context for scheduling channel receivers.
newtype Task a = Task
  { unTask :: StateT TaskSt Tower a
  } deriving (Functor, Monad)

-- | Base monad: internal only. State on fresh names, Reader on OS
newtype Base a = Base
  { unBase :: ReaderT OS (StateT Int Id) a
  } deriving (Functor, Monad)

-- | BaseUtils: internal only. Everyone deriving from 'Base' should
--   have a trivial implementation of these methods.
class (Monad m) => BaseUtils m where
  fresh :: m Int
  freshname :: m Name
  freshname = do
    n <- fresh
    return ("_" ++ (show n))
  getOS :: m OS

instance BaseUtils Base where
  fresh = Base $ do
    n <- get
    set (n + 1)
    return n
  getOS = Base ask

instance BaseUtils Tower where
  fresh = Tower $ lift fresh
  getOS = Tower $ lift getOS

instance BaseUtils Task where
  fresh = Task $ lift fresh
  getOS = Task $ lift getOS

-- Assembly --------------------------------------------------------------------

-- | Rich result of a Tower compilation. Use this to implement various metadata
--   backends such as 'Ivory.Tower.Graphviz'

-- XXX need to figure out what to do with this mess
data Assembly =
  Assembly
    { asm_channels   :: [CompiledChannel]
    , asm_tasks      :: [TaskSt]
    , asm_deps       :: [Module]
    , asm_schedule   :: TowerSchedule
    }

