
module Ivory.Tower
  -- Types and convenience accessors:
  ( Channel
  , DataPort
  , src
  , snk
  -- Channel exports:
  , ChannelSource()
  , ChannelSink()
  , ChannelEmitter()
  , ChannelReceiver()

  , emit
  , emit_
  , emitV
  , emitV_
  --, onChannelV -- XXX need to implement.
  , receive

  -- DataPort exports:
  , DataSource()
  , DataSink()
  , DataReader()
  , DataWriter()

  , readData
  , writeData

  -- Monad exports:
  , getTimeMillis
  , Tower()
  , Task()
  , Signal()

  , OSGetTimeMillis()

  , freshname
  -- Tower exports:
  , task
  , signal
  , dataport
  , channel
  , channelWithSize
  , addModule
  , addDepends

  , withGetTimeMillis

  -- Node exports:
  , Node()
  , Channelable(..)
  , withChannelEmitter
  , withChannelReceiver
  , withDataReader
  , withDataWriter


  , channelSourceCallback
  -- Task exports:
  , taskLocal
  , taskLocalInit
  , withStackSize
  , withPriority
  , withModule

  , withChannelEvent
  , withPeriodicEvent
  , onEvent
  , onEventV

  , taskModuleDef
  , taskInit
  , taskDependency

  , onPeriod
  , onChannel
  , onChannelV

  -- Signal exports:
  , signalLocal
  , signalLocalInit
  , signalBody
  , signalName
  , signalModuleDef
  , signalInit

  -- Types
  , Name
  , Assembly()
  , TaskSchedule()
  , SigSchedule()
  , Event()

  -- Built in helpers
  , stateProxy
  -- GHC TypeLits: user needs these for constraints
  , Nat()
  , SingI()
  ) where

import GHC.TypeLits

import Ivory.Tower.Ivory
import Ivory.Tower.Tower
import Ivory.Tower.Task
import Ivory.Tower.Signal
import Ivory.Tower.Node
import Ivory.Tower.Types

-- | Type synonym for the return value of 'channel'
type Channel n area = (ChannelSource n area, ChannelSink n area)

-- | Type synonym for the return value of 'dataport'
type DataPort area  = (DataSource area,      DataSink area)

-- | Convenience function for taking a pair of (source, sink) like returned by
--   'dataport' or 'channel' and getting just the source.
src :: (a,b) -> a
src = fst

-- | Convenience function for taking a pair of (source, sink) like returned by
--   'dataport' or 'channel' and getting just the sink.
snk :: (a,b) -> b
snk = snd


