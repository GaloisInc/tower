{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Sensor.Gyroscope where

import Ivory.Language
import Ivory.Serialize
import Ivory.Tower.Types.Time

gyroscopeTypesModule :: Module
gyroscopeTypesModule = package "gyroscope_types" $ do
  defStruct (Proxy :: Proxy "gyroscope_sample")
  depend serializeModule
  wrappedPackMod gyroscopeWrapper

[ivory|
struct gyroscope_sample
  { initfail   :: Stored IBool
  ; samplefail :: Stored IBool
  ; sample     :: Array 3 (Stored IFloat) -- degrees/sec
  ; temp       :: Stored IFloat -- degrees Celsius
  ; time       :: Stored ITime
  }
|]

packIBool :: PackRep (Stored IBool)
packIBool = repackV (/=? 0) (? (1, 0)) (packRep :: PackRep (Stored Uint8))

packITime :: PackRep (Stored ITime)
packITime = repackV fromIMicroseconds toIMicroseconds (packRep :: PackRep (Stored Sint64))

gyroscopeWrapper :: WrappedPackRep (Struct "gyroscope_sample")
gyroscopeWrapper = wrapPackRep "gyroscope_sample" $ packStruct
  [ packLabel' initfail packIBool
  , packLabel' samplefail packIBool
  , packLabel sample
  , packLabel temp
  , packLabel' time packITime
  ]

instance Packable (Struct "gyroscope_sample") where
  packRep = wrappedPackRep gyroscopeWrapper
