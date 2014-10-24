{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Tower.Types.EmitterCode
  ( EmitterCode(..)
  , SomeEmitterCode(..)
  , someemittercode_deliver
  , someemittercode_init
  , someemittercode_gen
  , someemittercode_user
  ) where

import Ivory.Language

data SomeEmitterCode = forall a . SomeEmitterCode (EmitterCode a)

someemittercode_deliver :: SomeEmitterCode -> Def('[]:->())
someemittercode_deliver (SomeEmitterCode c) = emittercode_deliver c
someemittercode_init :: SomeEmitterCode -> Def('[]:->())
someemittercode_init (SomeEmitterCode c) = emittercode_init c
someemittercode_gen :: SomeEmitterCode -> ModuleDef
someemittercode_gen (SomeEmitterCode c) = emittercode_gen c
someemittercode_user :: SomeEmitterCode -> ModuleDef
someemittercode_user (SomeEmitterCode c) = emittercode_user c

data EmitterCode (a :: Area *) = EmitterCode
  { emittercode_init :: Def('[]:->())
  , emittercode_emit :: forall s . Def('[ConstRef s a]:->())
  , emittercode_deliver :: Def('[]:->())
  , emittercode_user :: ModuleDef
  , emittercode_gen :: ModuleDef
  }
