{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS.SharedState where

import Prelude hiding (read)

import Ivory.Language

import qualified Ivory.OS.FreeRTOS.Semaphore as S

import Ivory.Tower.Types

data FreeRTOSDataport area =
  FreeRTOSDataport
    { fdp_name :: String
    , fdp_read :: forall eff s . Ref s area -> Ivory eff ()
    , fdp_write :: forall eff s . ConstRef s area -> Ivory eff ()
    , fdp_initDef :: Def('[]:->())
    , fdp_moduleDef :: ModuleDef
    , fdp_dataportid :: DataportId
    }

sharedState :: forall area . (IvoryArea area)
            => DataportId
            -> FreeRTOSDataport area
sharedState dataportid = FreeRTOSDataport
  { fdp_name = name
  , fdp_read = read
  , fdp_write = write
  , fdp_initDef = initDef
  , fdp_moduleDef = m
  , fdp_dataportid = dataportid
  }
  where
  unique :: String -> String
  unique n = n ++ ("_dataport" ++ (show (unDataportId dataportid)))

  name = unique "freertos_sharedState"

  stateArea  :: MemArea area
  stateArea   = area (unique "freertos_sharedState_state") Nothing
  state       = addrOf stateArea
  semArea    :: MemArea S.Semaphore
  semArea     = area (unique "freertos_sharedState_sem") Nothing
  sem         = addrOf semArea

  withSemaphore :: Ivory eff () -> Ivory eff ()
  withSemaphore action = do
    call_ S.takeBlocking sem
    _ <- action
    call_ S.give sem

  write :: ConstRef s area -> Ivory eff ()
  write v = do
    withSemaphore $ refCopy state v

  read :: Ref s area -> Ivory eff ()
  read v = do
    withSemaphore $ refCopy v state

  initDef :: Def ('[] :-> ())
  initDef = proc (unique "freertos_sharedState_init") $ body $
    call_ S.create sem

  m = do
    incl initDef
    private $ do
      defMemArea stateArea
      defMemArea semArea

