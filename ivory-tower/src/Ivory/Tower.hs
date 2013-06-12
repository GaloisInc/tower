
module Ivory.Tower
  -- Channel exports:
  ( ChannelSource()
  , ChannelSink()
  , ChannelEmitter()
  , ChannelReceiver()

  , emit

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

  , OSGetTimeMillis()

  , freshname
  -- Tower exports:
  , task
  , dataport
  , channel
  , addModule

  , withPeriod
  , withGetTimeMillis

  , withChannelEmitter
  , withChannelReceiver

  , withDataReader
  , withDataWriter

  , taskBody

  -- Task exports:
  , withStackSize
  , withPriority
  , withModule

  , taskModuleDef

  -- EventLoop exports:
  , onChannel
  , onTimer
  , eventLoop

  -- Types
  , Name
  , Assembly()
  , Schedule

  -- Built in helpers
  , stateProxy
  ) where

import Ivory.Tower.Ivory
import Ivory.Tower.Tower
import Ivory.Tower.Task
import Ivory.Tower.Types

