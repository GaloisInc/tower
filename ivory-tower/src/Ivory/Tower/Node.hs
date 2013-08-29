{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Node where

import MonadLib (lift)

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Monad
import Ivory.Tower.Tower (channelWithSize)

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
--   'Node'.
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
--   'Node'.
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

-- Will expose this publicly:
channelSourceCallback :: forall n area
            . (forall s eff . ConstRef s area -> Ivory eff ())
           -> ModuleDef
           -> ChannelSource n area
           -> ChannelSource n area
channelSourceCallback k mdef cs = ChannelSourceExtended k mdef cs

-- Private
getChannelSourceCallback :: ChannelSource n a
           -> (ConstRef s a -> Ivory eff (), ModuleDef)
getChannelSourceCallback cs = aux cs (const (return ()), return ())
  where
  aux (ChannelSourcePrim _) acc = acc
  aux (ChannelSourceExtended cb mdef rest) acc =
    (\r -> cb r >> cb' r, mdef >> mdef')
    where (cb', mdef') = aux rest acc

nodeInit :: ( forall s . Ivory (ProcEffects s ()) () ) -> Node i p ()
nodeInit i = do
  s <- getNode
  n <- getNodeName
  case nodest_nodeinit s of
    Nothing -> setNode $ s { nodest_nodeinit = Just (initproc n) }
    Just _ -> (err n)
  where
  err nodename = error ("multiple nodeInit definitions in node named "
                          ++ nodename)
  initproc nodename = proc ("nodeInit_" ++ nodename) $ body i

-- | Private: for use by StateMachine
nodeChannel :: forall area p n. (IvoryArea area, SingI n)
        => Task p (ChannelSource n area, ChannelSink n area)
nodeChannel = Node $ lift channelWithSize

