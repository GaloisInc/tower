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

type Name = String
data Labeled a = Labeled a String deriving (Show)
unLabeled :: Labeled a -> a
unLabeled (Labeled a _) = a
instance Functor Labeled where
  fmap f (Labeled a s) = Labeled (f a) s

-- Channel Types ---------------------------------------------------------------

data UTChannelRef = UTChannelRef { unUTChannelRef :: Name } deriving (Eq)

instance Show UTChannelRef where
  show utr = "UTChannelRef " ++ (show (unUTChannelRef utr))

data CompiledChannelName
  = ChannelName
      { ccn_utchref :: UTChannelRef
      , ccn_endpoint :: Name
      }
  | DataPortName
     { ccn_dataportname :: Name }
  deriving (Eq, Show)

compiledChannelName :: CompiledChannelName -> String
compiledChannelName (ChannelName r e) = (unUTChannelRef r) ++ "_endpoint_" ++ e
compiledChannelName (DataPortName n)  = n

data ChannelRef (area :: Area) = ChannelRef { unChannelRef :: UTChannelRef
      } deriving (Eq)

newtype ChannelSource area = ChannelSource { unChannelSource :: ChannelRef area}
newtype ChannelSink area   = ChannelSink   { unChannelSink   :: ChannelRef area}

newtype ChannelEmitter area =
  ChannelEmitter
    { unChannelEmitter :: forall s eff cs . (eff `AllocsIn` cs)
                 => ConstRef s area -> Ivory eff ()
    }

data ScheduledReceiver area =
  ScheduledReceiver
    { sr_receiver :: forall eff cs s . (eff `AllocsIn` cs)
                => Ref s area -> Ivory eff IBool
    , sr_utref :: UTChannelRef
    , sr_compiledchannel :: CompiledChannel
    }

-- Dataport Types --------------------------------------------------------------

data DataPort area =
  DataPort
    { data_name  :: Name
    , data_read  :: forall s eff cs . (eff `AllocsIn` cs)
                 => Ref s area -> Ivory eff ()
    , data_write :: forall s eff cs . (eff `AllocsIn` cs)
                 => ConstRef s area -> Ivory eff ()
    , data_cch   :: CompiledChannel
    }

newtype DataSource area = DataSource { unDataSource :: (DataPort area) }
newtype DataSink area   = DataSink   { unDataSink   :: (DataPort area) }

newtype DataReader area = DataReader { unDataReadable :: (DataPort area) }
newtype DataWriter area = DataWriter { unDataWritable :: (DataPort area) }

-- EventLoop types -------------------------------------------------------------
data TaskLoop = TaskLoop { unTaskLoop :: forall eff cs . (eff `AllocsIn` cs)
                                      => (Ivory eff (EventLoop eff))}

data EventLoopImpl eff
 =  forall area cs . (eff `AllocsIn` cs, IvoryType area, IvoryZero area) =>
    EventLoopChannel
      (ScheduledReceiver area)
      (ConstRef (Stack cs) area -> Ivory eff ())

 | forall cs . (eff `AllocsIn` cs) =>
   EventLoopPeriod
    Integer (Uint32 -> Ivory eff ())

newtype EventLoop eff = EventLoop { unEventLoop :: [EventLoopImpl eff] }

instance Monoid (EventLoop eff) where
  mempty = EventLoop []
  mappend el1 el2 = EventLoop ((unEventLoop el1) ++ (unEventLoop el2))

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

newtype CompiledTaskLoop =
  CompiledTaskLoop { unCompiledTaskLoop :: Def('[]:->()) }

-- Schedule --------------------------------------------------------------------

data TaskSchedule =
  TaskSchedule
    { tasksch_name      :: Name
    , tasksch_receivers :: [Labeled UTChannelRef]
    , tasksch_channels  :: [CompiledChannel]
    } deriving (Show)


newtype Period = Period { unPeriod :: Integer }

emptyTaskSchedule :: Name -> TaskSchedule
emptyTaskSchedule n = TaskSchedule
  { tasksch_name = n
  , tasksch_receivers = []
  , tasksch_channels = []
  }

data TowerSchedule =
  TowerSchedule
    { scheduleEmitter :: forall area . (IvoryType area)
           => ChannelRef area -> ChannelEmitter area
    , scheduleTaskLoop :: TaskSchedule -> TaskLoop -> CompiledTaskLoop
    , scheduleInitializer :: Def ('[]:->())
    , scheduleModuleDef :: ModuleDef
    }

-- Operating System ------------------------------------------------------------

data OS =
  OS
    { osDataPort      :: forall area . (IvoryType area)
                      => Name -- Unique dataport name
                      -> DataPort area
    , osGetTimeMillis :: forall eff  . Ivory eff Uint32
    , osSchedule      :: [UTChannelRef] -> [UncompiledTask] -> TowerSchedule
    , osCreateChannel :: forall area . (IvoryType area)
                      => ChannelRef area
                      -> CompiledChannelName
                      -> (ScheduledReceiver area)
    }

-- Monad Types -----------------------------------------------------------------

newtype Tower a = Tower
  { unTower :: WriterT [UncompiledTower] Base a
  } deriving (Functor, Monad)

newtype Task a = Task
  { unTask :: StateT TaskSchedule Base a
  } deriving (Functor, Monad)

newtype Scheduled a = Scheduled
  { unScheduled :: ReaderT TowerSchedule (StateT TaskResult Base) a
  } deriving (Functor, Monad)

type TaskConstructor = Task (Scheduled ())

newtype Base a = Base
  { unBase :: ReaderT OS (StateT Int Id) a
  } deriving (Functor, Monad)

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

instance BaseUtils Scheduled where
  fresh = Scheduled $ lift $ lift fresh
  getOS = Scheduled $ lift $ lift getOS

data UncompiledTask =
  UncompiledTask
    { ut_taskSch        :: TaskSchedule
    , ut_scheduledMonad :: (Scheduled ())
    }

ut_name :: UncompiledTask -> Name
ut_name = tasksch_name . ut_taskSch

data UncompiledTower = UModule     Module
                     | UCompiledData CompiledChannel
                     | UChannelRef (Labeled UTChannelRef)
                     | UTask       UncompiledTask
-- Assembly --------------------------------------------------------------------

data Assembly =
  Assembly
    { asm_channels   :: [CompiledChannel]
    , asm_tasks      :: [TaskResult]
    , asm_deps       :: [Module]
    , asm_schedule   :: TowerSchedule
    }

instance Show Assembly where
  show asm = "Assembly " ++ (show (asm_channels asm)) ++ (show (asm_tasks asm))

-- Task Types ------------------------------------------------------------------

data TaskResult =
  TaskResult
    { taskres_tldef       :: Maybe (Def('[]:->()))
    , taskres_moddef      :: ModuleDef
    , taskres_priority    :: Maybe Integer
    , taskres_stacksize   :: Maybe Integer
    , taskres_extern_mods :: [Module]
    , taskres_taggedchs   :: [TaggedChannel]
    , taskres_periodic    :: [Integer]
    , taskres_schedule    :: TaskSchedule
    }

instance Show TaskResult where
  show tr = "TaskResult { priority = "  ++ (show (taskres_priority tr))
                   ++ " , stacksize = " ++ (show (taskres_stacksize tr))
                   ++ " , taggedchs = " ++ (show (taskres_taggedchs tr))
                   ++ " , periodic = "  ++ (show (taskres_periodic tr))
                   ++ " , schedule = ("  ++ (show (taskres_schedule tr))
                   ++ ") }"

taskres_name :: TaskResult -> Name
taskres_name = tasksch_name . taskres_schedule

emptyTaskResult :: TaskSchedule -> TaskResult
emptyTaskResult sch = TaskResult
  { taskres_tldef       = Nothing
  , taskres_moddef      = return ()
  , taskres_priority    = Nothing
  , taskres_stacksize   = Nothing
  , taskres_extern_mods = []
  , taskres_taggedchs   = scheduleTaggedChannels sch
  , taskres_periodic    = []
  , taskres_schedule    = sch
  }
  where
  scheduleTaggedChannels :: TaskSchedule -> [TaggedChannel]
  scheduleTaggedChannels s = map tcr (tasksch_receivers s)
    where
    tcr (Labeled utref lbl) = TagChannelReceiver lbl utref

-- Connector -------------------------------------------------------------------

data TaggedChannel
  = TagChannelEmitter  String UTChannelRef
  | TagChannelReceiver String UTChannelRef
  | TagDataReader      String CompiledChannel
  | TagDataWriter      String CompiledChannel
  deriving (Show)

