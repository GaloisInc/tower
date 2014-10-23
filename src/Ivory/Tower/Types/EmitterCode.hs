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

import Ivory.Tower.ToyObjLang

data SomeEmitterCode = forall a . SomeEmitterCode (EmitterCode a)

someemittercode_deliver :: SomeEmitterCode -> Def('[]:->())
someemittercode_deliver  = undefined
someemittercode_init :: SomeEmitterCode -> Def('[]:->())
someemittercode_init  = undefined
someemittercode_gen :: SomeEmitterCode -> ModuleDef
someemittercode_gen  = undefined
someemittercode_user :: SomeEmitterCode -> ModuleDef
someemittercode_user  = undefined

data EmitterCode (a :: Area *) = EmitterCode
  { emittercode_init :: Def('[]:->())
  , emittercode_emit :: forall s . Def('[ConstRef s a]:->())
  , emittercode_deliver :: Def('[]:->())
  , emittercode_user :: ModuleDef
  , emittercode_gen :: ModuleDef
  }
