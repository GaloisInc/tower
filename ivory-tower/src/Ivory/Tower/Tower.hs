{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Tower where

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
  n <- fresh
  let dpid = DataportId n
      (source, sink) = (DataSource dpid, DataSink dpid)
  describeDataport dpid
  codegenDataport source
  return (source, sink)
  where
  codegenDataport :: (IvoryArea area) => DataSource area -> Tower ()
  codegenDataport datasource = do
    os <- getOS
    let (initializer,mdef) = os_mkDataPort os datasource
    s <- getTowerSt
    setTowerSt $ s { towerst_dataportinit = initializer : (towerst_dataportinit s)
                   , towerst_moddef       = mdef >> (towerst_moddef s) }

  describeDataport :: DataportId -> Tower ()
  describeDataport dpid = do
    s <- getTowerSt
    setTowerSt $ s { towerst_dataports = (Labeled dpid tyname) : (towerst_dataports s) }
    where
    tyname = show $ IArea.ivoryArea (Proxy :: Proxy area)


-- | Instantiate a channel. Result is a matching pair of 'ChannelSource' and
--   'ChannelSink'.
channel :: forall area . (IvoryArea area) => Tower (ChannelSource area, ChannelSink area)
channel = do
  cid <- freshChannelId
  st <- getTowerSt
  setTowerSt $ st { towerst_channels = (Labeled cid tyname) : towerst_channels st }
  return (ChannelSource cid, ChannelSink cid)
  where
  freshChannelId = fresh >>= \n -> return (ChannelId n)
  tyname = show $ IArea.ivoryArea (Proxy :: Proxy area)

-- | Add an arbitrary Ivory 'Module' to Tower. The module will be present in the
--   compiled 'Assembly'. This is provided as a convenience so users do not have
--   to append to an 'Assembly' at a later stage.
addModule :: Module -> Tower ()
addModule m = do
  s <- getTowerSt
  setTowerSt $ s { towerst_modules = m : (towerst_modules s) }



