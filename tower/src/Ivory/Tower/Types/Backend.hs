{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Types.Backend where

import Ivory.Language

data SomeHandler backend = forall a. SomeHandler (TowerBackendHandler backend a)


class TowerBackendTypes backend where
  -- XXX should probably be type families, not data families, and maybe at the
  -- top-level (without relying on the class).

  -- Type correponds to the channel type
  data TowerBackendCallback  backend :: Area * -> *
  data TowerBackendEmitter   backend :: *
  -- Type correponds to the channel type
  data TowerBackendHandler   backend :: Area * -> *
  data TowerBackendMonitor   backend :: *
  data TowerBackendOutput    backend :: *