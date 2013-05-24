{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.FreeRTOS.SharedState where

import Prelude hiding (read)

import Ivory.Language
import qualified Ivory.Language.Type as I

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

sharedState :: forall area . (IvoryType area)
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

  localState :: MemArea area
  localState = area (unique "freertos_sharedState_state") Nothing
  localSem   :: MemArea S.Semaphore
  localSem    = area (unique "freertos_sharedState_Sem") Nothing

  withSemaphore :: Ivory eff () -> Ivory eff ()
  withSemaphore action = do
    sem <- addrOf localSem
    call_ S.takeBlocking sem
    _ <- action
    call_ S.give sem

  write :: ConstRef s area -> Ivory eff ()
  write v = do
    sharedRef <- addrOf localState
    withSemaphore $ refCopy sharedRef v

  read :: Ref s area -> Ivory eff ()
  read v = do
    sharedRef <- addrOf localState
    withSemaphore $ refCopy v sharedRef

  initDef :: Def ('[] :-> ())
  initDef = proc (unique "freertos_sharedState_init") $ body $ do
    sem <- addrOf localSem
    call_ S.create sem

  m = do
    depend S.semaphoreModule
    incl initDef
    private $ do
      defMemArea localState
      defMemArea localSem

