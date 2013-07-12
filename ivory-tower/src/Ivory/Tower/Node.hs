{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Node where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Monad

-- | Transform a 'DataSink' into a 'DataReader' in the context of a
--   'Node'. Provide a human-readable name as a debugging aid.
withDataReader :: (DataPortable i, IvoryArea area)
               => DataSink area -> String -> Node i (DataReader area)
withDataReader ds label = do
  nodeStAddDataReader (unDataSink ds) label
  nodeDataReader ds

-- | Transform a 'DataSource' into a 'DataWriter' in the context of a
--   'Node'. Provide a human-readable name as a debugging aid.
withDataWriter :: (DataPortable i, IvoryArea area)
               => DataSource area -> String -> Node i (DataWriter area)
withDataWriter ds label = do
  nodeStAddDataWriter (unDataSource ds) label
  nodeDataWriter ds

class DataPortable i where
  nodeDataReader :: (IvoryArea area)
                 => DataSink   area -> Node i (DataReader area)
  nodeDataWriter :: (IvoryArea area)
                 => DataSource area -> Node i (DataWriter area)

-- | Transform a 'ChannelSink' into a 'ChannelReceiver' in the context of a
--   'Task'.
--   A human-readable name is provided to aid in debugging.
withChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area, Channelable i)
      => ChannelSink n area -> String -> Node i (ChannelReceiver n area)
withChannelReceiver chsink label = do
  rxer <- nodeChannelReceiver chsink
  -- Register the receiver into the graph context
  nodeStAddReceiver (unChannelSink chsink) label
  -- Generate code implementing the channel for this receiver.
  codegenChannelReceiver rxer
  return rxer

-- | Transform a 'ChannelSource' into a 'ChannelEmitter' in the context of a
--   'Task'.
--   Provide a human-readable name as a debugging aid.
withChannelEmitter :: (SingI n, IvoryArea area, Channelable i)
      => ChannelSource n area -> String -> Node i (ChannelEmitter n area)
withChannelEmitter chsrc label = do
  emitter <- nodeChannelEmitter chsrc
  nodeStAddEmitter (unChannelSource chsrc) label
  return emitter

-- | private
codegenChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area)
                       => ChannelReceiver n area -> Node i ()
codegenChannelReceiver rxer = do
  os <- getOS
  thisnode <- getNode
  let (channelinit, mdef) = os_mkChannel os rxer thisnode
  nodeStAddCodegen channelinit mdef

class Channelable i where
  nodeChannelEmitter :: (SingI n, IvoryArea area)
        => ChannelSource n area -> Node i (ChannelEmitter n area)
  nodeChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area)
        => ChannelSink n area -> Node i (ChannelReceiver n area)
