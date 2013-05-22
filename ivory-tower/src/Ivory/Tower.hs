
module Ivory.Tower
  -- Channel exports:
  ( ChannelSource()
  , ChannelSink()
  , ChannelEmitter()
  , ChannelReceiver()

  , emit

  -- DataPort exports:
  , DataPort()
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
  , handlers

  -- Types
  , Name
  , Assembly()
  ) where

import Ivory.Tower.Channel
import Ivory.Tower.DataPort
import Ivory.Tower.EventLoop
import Ivory.Tower.Monad
import Ivory.Tower.Tower
import Ivory.Tower.Task
import Ivory.Tower.Types

