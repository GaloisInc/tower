
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

import Ivory.Tower.Channel
import Ivory.Tower.DataPort
import Ivory.Tower.EventLoop
import Ivory.Tower.Tower
import Ivory.Tower.Task
import Ivory.Tower.Types

import Ivory.Language

stateProxy :: (IvoryType area, IvoryZero area)
           => ChannelSink area -> Tower (DataSink area)
stateProxy chsink = do
  (src_data, snk_data) <- dataport
  task "stateProxy" $ do
    chrxer <- withChannelReceiver chsink "proxy event"
    data_writer <- withDataWriter src_data "proxy data"
    taskBody $ \schedule ->
      eventLoop schedule $ onChannel chrxer $ \val -> do
        writeData schedule data_writer val
  return snk_data

