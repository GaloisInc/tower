{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Ivory.Tower.Compile.FreeRTOS.SharedState
  ( sharedState
  ) where

import Prelude hiding (read)

import Ivory.Language
import qualified Ivory.Language.Type as I

import qualified Ivory.OS.FreeRTOS.Semaphore as S

import Ivory.Tower.Types

sharedState :: forall area . (IvoryType area)
            => String
            -> DataPort area
sharedState uniquename = DataPort
  { data_name = name
  , data_read = read
  , data_write = write
  , data_cch = CompiledChannel
    { cch_name        = DataPortName name
    , cch_initializer = initDef
    , cch_moddefs     = m
    , cch_type        = show (I.ivoryType (Proxy :: Proxy area))
    }
  }
  where
  unique :: String -> String
  unique n = n ++ uniquename

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

