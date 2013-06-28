{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Ivory.Tower.Types where

import GHC.TypeLits
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
-- best-effort asynchronous communication. We'll be working on better
-- guarantees on scheduling soon...

-- | The basic reference type underlying all Channels. Internal only.
data ChannelId =
  ChannelId
    { chan_id :: Integer
    , chan_size :: Integer
    } deriving (Eq, Show)

-- | Designates a Source, the end of a Channel which is written to. The only
-- valid operation on a 'ChannelSource' is
-- 'Ivory.Tower.Tower.withChannelEmitter'
newtype ChannelSource (n :: Nat) (area :: Area) =
  ChannelSource { unChannelSource :: ChannelId }

-- | Designates a Sink, the end of a Channel which is read from. The only
-- valid operation on a 'ChannelSink' is
-- 'Ivory.Tower.Tower.withChannelReceiver'
newtype ChannelSink (n :: Nat) (area :: Area) =
  ChannelSink { unChannelSink :: ChannelId }

-- | a 'ChannelSource' which has been registered in the context of a 'Task'
-- can then be used with 'Ivory.Tower.Channel.emit' to create Ivory code.
newtype ChannelEmitter (n :: Nat) (area :: Area) =
  ChannelEmitter { unChannelEmitter :: ChannelId }

-- | a 'ChannelSink' which has been registered in the context of a 'Task'
-- can then be used with 'Ivory.Tower.EventLoop.onChannel' to create an Ivory
-- event handler.
newtype ChannelReceiver (n :: Nat) (area :: Area) =
  ChannelReceiver { unChannelReceiver :: ChannelId }

-- Dataport Types --------------------------------------------------------------

-- | The basic reference type underlying all Dataports. Internal only.
newtype DataportId = DataportId { unDataportId :: Int } deriving (Eq)

instance Show DataportId where
  show dpid = "DataportId " ++ (show (unDataportId dpid))

-- | Designates a Dataport Source, the end of a Dataport
--   which is written to. The only valid operation on 'DataSource' is
--   'Ivory.Tower.Tower.withDataWriter'
newtype DataSource (area :: Area) = DataSource { unDataSource :: DataportId  }

-- | Designates a Dataport sink, the end of a Dataport
--   which is read from. The only valid operation on 'DataSink' is
--   'Ivory.Tower.Tower.withDataReader'
newtype DataSink (area :: Area) = DataSink { unDataSink   :: DataportId }

-- | An implementation of a reader on a channel. The only valid operation on
--   a 'DataReader' is 'Ivory.Tower.DataPort.readData', which unpacks the
--   implementation into the correct 'Ivory.Language.Ivory' effect scope.
newtype DataReader (area :: Area) = DataReader { unDataReader :: DataportId }

-- | An implementation of a writer on a channel. The only valid operation on
--   a 'DataWriter' is 'Ivory.Tower.DataPort.writeData', which unpacks the
--   implementation into the correct 'Ivory.Language.Ivory' effect scope.
newtype DataWriter (area :: Area) = DataWriter { unDataWriter :: DataportId }

-- EventLoop types -------------------------------------------------------------

-- | Pairs of events and handlers. Event handling will be multiplexed into an
--   task loop, see 'Ivory.Tower.Tower.taskBody'.
--   Combine EventLoops to be scheduled as part of the same task loop using
--   'Data.Monoid'
newtype EventLoop eff =
  EventLoop { unEventLoop :: [(TaskSchedule -> Ivory eff (Ivory eff ()))] }

instance Monoid (EventLoop eff) where
  mempty = EventLoop []
  mappend el1 el2 = EventLoop ((unEventLoop el1) ++ (unEventLoop el2))

-- Period ----------------------------------------------------------------------
-- | Wrapper type for periodic schedule, created using
-- 'Ivory.Tower.Tower.withPeriod'
newtype Period = Period { unPeriod :: Integer }

-- Get Time Millis -------------------------------------------------------------
-- | Wrapper type for OS get time millis implementation , created using
-- 'Ivory.Tower.Tower.withPeriod'
newtype OSGetTimeMillis =
  OSGetTimeMillis { unOSGetTimeMillis :: forall eff . Ivory eff Uint32 }

-- Node State-------------------------------------------------------------------

data NodeCodegen =
  NodeCodegen
    { ncg_init  :: Def('[]:->())
    , ncg_mdef  :: ModuleDef
    }

data NodeSt a =
  NodeSt
    { nodest_name        :: Name
    , nodest_emitters    :: [Labeled ChannelId]
    , nodest_receivers   :: [Labeled ChannelId]
    , nodest_datareaders :: [Labeled DataportId]
    , nodest_datawriters :: [Labeled DataportId]
    , nodest_codegen     :: [NodeCodegen]
    , nodest_impl        :: a
    } deriving (Functor)

emptyNodeSt :: Name -> a -> NodeSt a
emptyNodeSt n impl = NodeSt
  { nodest_name        = n
  , nodest_emitters    = []
  , nodest_receivers   = []
  , nodest_datareaders = []
  , nodest_datawriters = []
  , nodest_codegen     = []
  , nodest_impl        = impl
  }

-- Task State-------------------------------------------------------------------

-- | Internal only: this is the result of a complete 'Task' context.
data TaskSt =
  TaskSt
    { taskst_periods     :: [Integer]
    , taskst_stacksize   :: Maybe Integer
    , taskst_priority    :: Maybe Integer
    , taskst_moddef      :: TaskSchedule -> ModuleDef
    , taskst_extern_mods :: [Module]
    , taskst_taskbody    :: Maybe (TaskSchedule -> Def('[]:->()))
    }

-- | Internal only
emptyTaskSt :: TaskSt
emptyTaskSt = TaskSt
  { taskst_periods     = []
  , taskst_stacksize   = Nothing
  , taskst_priority    = Nothing
  , taskst_moddef      = const (return ())
  , taskst_extern_mods = []
  , taskst_taskbody    = Nothing
  }

type TaskNode = NodeSt TaskSt

-- Signal State-----------------------------------------------------------------

-- | Internal only: this is the result of a complete 'Signal' context.
data SignalSt =
  SignalSt
    { signalst_moddef :: SigSchedule -> ModuleDef
    , signalst_body   :: Maybe (SigSchedule -> Def('[]:->()))
    , signalst_cname  :: Maybe String
    }

emptySignalSt :: SignalSt
emptySignalSt = SignalSt
  { signalst_moddef = const (return ())
  , signalst_body   = Nothing
  , signalst_cname  = Nothing
  }

type SigNode = NodeSt SignalSt

-- Tower State -----------------------------------------------------------------

data TowerSt =
  TowerSt
    { towerst_modules      :: [Module]
    , towerst_dataports    :: [Labeled DataportId]
    , towerst_channels     :: [Labeled ChannelId]
    , towerst_tasknodes    :: [TaskNode]
    , towerst_signodes     :: [SigNode]
    , towerst_dataportinit :: [Def('[]:->())]
    , towerst_moddef       :: ModuleDef
    }

emptyTowerSt :: TowerSt
emptyTowerSt = TowerSt
  { towerst_modules = []
  , towerst_dataports = []
  , towerst_channels = []
  , towerst_tasknodes = []
  , towerst_signodes = []
  , towerst_dataportinit = []
  , towerst_moddef = return ()
  }

-- Compiled Schedule -----------------------------------------------------------

type IBoolRef eff cs = Ivory eff (Ref (Stack cs) (Stored IBool))

data TaskSchedule =
  TaskSchedule
    { tsch_mkDataReader :: forall area s eff cs
                         . (IvoryArea area, Allocs eff ~ Alloc cs)
                        => DataReader area -> Ref s area -> Ivory eff ()
    , tsch_mkDataWriter :: forall area s eff cs
                         . (IvoryArea area, Allocs eff ~ Alloc cs)
                        => DataWriter area -> ConstRef s area -> Ivory eff ()
    , tsch_mkEmitter :: forall n area s eff cs
                      . (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
                     => ChannelEmitter n area
                     -> ConstRef s area
                     -> IBoolRef eff cs
    , tsch_mkReceiver :: forall n area s eff cs
            . (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
           => ChannelReceiver n area
           -> Ref s area
           -> Ivory eff IBool
    , tsch_mkPeriodic :: forall eff cs . (Allocs eff ~ Alloc cs)
           => Period
           -> (Uint32 -> Ivory eff ())
           -> Ivory eff (Ivory eff ()) -- Outer part of the loop returns inner
                                       -- part of the loop
    , tsch_mkEventLoop :: forall eff cs . (Allocs eff ~ Alloc cs)
           => [Ivory eff (Ivory eff ())] -> Ivory eff ()
    , tsch_mkTaskBody :: (forall eff cs . (Allocs eff ~ Alloc cs )
                     => Ivory eff ()) -> Def('[]:->())
    }

data SigSchedule =
  SigSchedule
    { ssch_mkEmitter :: forall n area s eff cs
            . (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
           => ChannelEmitter n area
           -> ConstRef s area
           -> IBoolRef eff cs
    , ssch_mkReceiver :: forall n area s eff cs
            . (SingI n, IvoryArea area, Allocs eff ~ Alloc cs)
           => ChannelReceiver n area
           -> Ref s area
           -> Ivory eff IBool
    , ssch_mkSigBody :: (forall eff cs . (Allocs eff ~ Alloc cs)
                     => Ivory eff ()) -> Def('[]:->())
    }

-- Operating System ------------------------------------------------------------

-- | Backend for targeting Tower to a particular Operating System. Should be
--   implemented by a module in 'Ivory.Tower.Compile'.
data OS =
  OS
    { os_mkDataPort    :: forall area . (IvoryArea area)
                       => DataSource area
                       -> (Def ('[]:->()), ModuleDef)

    -- Generate code needed to implement Channel, given the endpoint TaskSt
    -- (really just for the name) and a ChannelReceiver.
    , os_mkChannel     :: forall area i n 
                        . (SingI n, IvoryArea area, IvoryZero area)
                       => ChannelReceiver n area
                       -> NodeSt i
                       -> (Def ('[]:->()), ModuleDef)

    -- Generate a Schedule for a particular Task, given the set of
    -- all tasks (sufficient for a fully described graph of channels)
    , os_mkTaskSchedule    :: [TaskNode] -> [SigNode] -> TaskNode -> TaskSchedule

    , os_mkSigSchedule     :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule

    -- Generate any code needed for the system as a whole
    , os_mkSysSchedule     :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))

    -- Utility function
    , os_getTimeMillis :: forall eff . Ivory eff Uint32
    }

-- Monad Types -----------------------------------------------------------------

-- | Tower monad: context for instantiating connections (channels & dataports)
--   and tasks.
newtype Tower a = Tower
  { unTower :: StateT TowerSt Base a
  } deriving (Functor, Monad)

-- | Node monad: context for task-specific code generation
newtype Node i a = Node
  { unNode :: StateT (NodeSt i) Tower a
  } deriving (Functor, Monad)

type Task a = Node TaskSt a
type Signal a = Node SignalSt a

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

instance BaseUtils (Node i) where
  fresh = Node $ lift fresh
  getOS = Node $ lift getOS

-- Assembly --------------------------------------------------------------------

data Assembly =
  Assembly
    { asm_towerst  :: TowerSt
    , asm_tasks    :: [AssembledNode TaskSt]
    , asm_sigs     :: [AssembledNode SignalSt]
    , asm_system   :: (ModuleDef, Def('[]:->()))
    }

data AssembledNode a =
  AssembledNode 
    { asmnode_nodest :: NodeSt a
    , asmnode_tldef  :: Def('[]:->())
    , asmnode_moddef :: ModuleDef
    }

