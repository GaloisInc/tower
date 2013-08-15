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
               => DataSink area -> String -> Node i p (DataReader area)
withDataReader ds label = do
  (dr, drname) <- nodeDataReader ds
  nodeStAddDataReader (unDataSink ds) label drname
  return dr

-- | Transform a 'DataSource' into a 'DataWriter' in the context of a
--   'Node'. Provide a human-readable name as a debugging aid.
withDataWriter :: (DataPortable i, IvoryArea area)
               => DataSource area -> String -> Node i p (DataWriter area)
withDataWriter ds label = do
  (dw, dwname) <- nodeDataWriter ds
  nodeStAddDataWriter (unDataSource ds) label dwname
  return dw

class DataPortable i where
  nodeDataReader :: (IvoryArea area)
                 => DataSink   area -> Node i p (DataReader area, String)
  nodeDataWriter :: (IvoryArea area)
                 => DataSource area -> Node i p (DataWriter area, String)

-- | Transform a 'ChannelSink' into a 'ChannelReceiver' in the context of a
--   'Task'.
--   A human-readable name is provided to aid in debugging.
withChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area, Channelable i)
      => ChannelSink n area -> String -> Node i p (ChannelReceiver n area)
withChannelReceiver chsink label = do
  (rxer, rxername) <- nodeChannelReceiver chsink
  -- Register the receiver into the graph context
  nodeStAddReceiver (unChannelSink chsink) label rxername
  -- Generate code implementing the channel for this receiver.
  codegenChannelReceiver rxer
  return rxer

-- | Transform a 'ChannelSource' into a 'ChannelEmitter' in the context of a
--   'Task'.
--   Provide a human-readable name as a debugging aid.
withChannelEmitter :: (SingI n, IvoryArea area, Channelable i)
      => ChannelSource n area -> String -> Node i p (ChannelEmitter n area)
withChannelEmitter chsrc label = do
  (emitter, emname) <- nodeChannelEmitter chsrc
  nodeStAddEmitter (unChannelSource chsrc) label emname
  return emitter

-- | private
codegenChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area)
                       => ChannelReceiver n area -> Node i p ()
codegenChannelReceiver rxer = do
  os <- getOS
  thisnode <- getNode
  let (channelinit, mdef) = os_mkChannel os rxer thisnode
  nodeStAddCodegen channelinit mdef

class Channelable i where
  nodeChannelEmitter :: (SingI n, IvoryArea area)
        => ChannelSource n area -> Node i p (ChannelEmitter n area, String)
  nodeChannelReceiver :: (SingI n, IvoryArea area, IvoryZero area)
        => ChannelSink n area -> Node i p (ChannelReceiver n area, String)
