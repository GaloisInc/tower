{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Tower where

import GHC.TypeLits
import Control.Monad (when)

import Ivory.Language

import Ivory.Tower.Types
import Ivory.Tower.Monad

-- Public Tower functions ------------------------------------------------------

-- | Tower assembler. Given a complete 'Tower' monad, apply the operating system
--   ('OS') and collect the generated components into an 'Assembly'
assembleTower :: Tower p () -> OS -> Assembly
assembleTower t os = runBase (runTower t) os

-- | Instantiate a 'Task' into a task. Provide a name as a
--   human-readable debugging aid.
task :: Name -> Task p () -> Tower p ()
task name t = do
  taskNode <- runTask name t
  addTaskNode taskNode

signal :: Name -> Signal p () -> Tower p ()
signal name s = do
  sigNode <- runSignal name s
  addSigNode sigNode

-- | Instantiate a data port. Result is a matching pair of 'DataSource' and
--   'DataSink'.
dataport :: forall area p . (IvoryArea area)
         => Tower p (DataSource area, DataSink area)
dataport = do
  dpid <- mkDataport (Proxy :: Proxy area)
  let (source, sink) = (DataSource dpid, DataSink dpid)
  codegenDataport source
  return (source, sink)
  where
  -- Need this broken out separately to give it a type signature,
  -- for some reason GHC won't infer the type properly:
  codegenDataport :: (IvoryArea area) => DataSource area -> Tower p ()
  codegenDataport datasource = do
    os <- getOS
    let (initializer,mdef) = os_mkDataPort os datasource
    addDataportCodegen initializer mdef


-- | Instantiate a channel with the default FIFO buffer depth.
--   Result is a matching pair of 'ChannelSource' and 'ChannelSink'.
channel :: forall area p. (IvoryArea area)
        => Tower p (ChannelSource 16 area, ChannelSink 16 area)
channel = channelWithSize

-- | Instantiate a channel with a given FIFO buffer depth.
--   Result is a matching pair of 'ChannelSource' and 'ChannelSink'.
channelWithSize :: forall area n p . (SingI n, IvoryArea area)
                => Tower p (ChannelSource n area, ChannelSink n area)
channelWithSize = do
  when (size < 1) $ error "channelWithSize: size must be at least 1"
  chid <- mkChannel size (Proxy :: Proxy area)
  return (ChannelSource chid, ChannelSink chid)
  where
  size = fromSing (sing :: Sing n)

-- | Add an arbitrary Ivory 'Module' to Tower. The module will be present in the
--   compiled 'Assembly'. This is provided as a convenience so users do not have
--   to append to an 'Assembly' at a later stage.
addModule :: Module -> Tower p ()
addModule m = do
  s <- getTowerSt
  setTowerSt $ s { towerst_modules = m : (towerst_modules s) }

addDepends :: Module -> Tower p ()
addDepends m = do
  s <- getTowerSt
  setTowerSt $ s { towerst_depends = m : (towerst_depends s) }

