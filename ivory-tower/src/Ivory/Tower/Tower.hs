{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Tower where

import GHC.TypeLits
import Control.Monad (when)

import Ivory.Language
import qualified Ivory.Language.Area as IArea

import Ivory.Tower.Types
import Ivory.Tower.Monad

-- Public Tower functions ------------------------------------------------------

-- | Tower assembler. Given a complete 'Tower' monad, apply the operating system
--   ('OS') and collect the generated components into an 'Assembly'
assembleTower :: Tower () -> OS -> Assembly
assembleTower t os = runBase (runTower t) os

-- | Instantiate a 'Task' into a task. Provide a name as a
--   human-readable debugging aid.
task :: Name -> Task () -> Tower ()
task name t = do
  taskNode <- runTask name t
  addTaskNode taskNode

signal :: Name -> Signal () -> Tower ()
signal name s = do
  sigNode <- runSignal name s
  addSigNode sigNode

-- | Instantiate a data port. Result is a matching pair of 'DataSource' and
--   'DataSink'.
dataport :: forall area . (IvoryArea area) => Tower (DataSource area, DataSink area)
dataport = do
  dpid <- mkDataport tyname
  let (source, sink) = (DataSource dpid, DataSink dpid)
  codegenDataport source
  return (source, sink)
  where
  tyname = show $ IArea.ivoryArea (Proxy :: Proxy area)
  -- Need this broken out separately to give it a type signature,
  -- for some reason GHC won't infer the type properly:
  codegenDataport :: (IvoryArea area) => DataSource area -> Tower ()
  codegenDataport datasource = do
    os <- getOS
    let (initializer,mdef) = os_mkDataPort os datasource
    addDataportCodegen initializer mdef


-- | Instantiate a channel with the default FIFO buffer depth.
--   Result is a matching pair of 'ChannelSource' and 'ChannelSink'.
channel :: forall area . (IvoryArea area) => Tower (ChannelSource 16 area, ChannelSink 16 area)
channel = channelWithSize

-- | Instantiate a channel with a given FIFO buffer depth.
--   Result is a matching pair of 'ChannelSource' and 'ChannelSink'.
channelWithSize :: forall area n . (SingI n, IvoryArea area)
                => Tower (ChannelSource n area, ChannelSink n area)
channelWithSize = do
  when (size < 1) $ error "channelWithSize: size must be at least 1"
  chid <- mkChannel size tyname
  return (ChannelSource chid, ChannelSink chid)
  where
  tyname = show $ IArea.ivoryArea (Proxy :: Proxy area)
  size = fromSing (sing :: Sing n)

-- | Add an arbitrary Ivory 'Module' to Tower. The module will be present in the
--   compiled 'Assembly'. This is provided as a convenience so users do not have
--   to append to an 'Assembly' at a later stage.
addModule :: Module -> Tower ()
addModule m = do
  s <- getTowerSt
  setTowerSt $ s { towerst_modules = m : (towerst_modules s) }

