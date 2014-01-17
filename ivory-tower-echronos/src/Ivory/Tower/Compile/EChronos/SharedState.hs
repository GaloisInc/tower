{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.EChronos.SharedState where

import Prelude hiding (read)

import Ivory.Language

import qualified Ivory.OS.EChronos.Semaphore as S

import Ivory.Tower.Types

data EChronosDataport area =
  EChronosDataport
    { fdp_name :: String
    , fdp_read :: forall eff s . Ref s area -> Ivory eff ()
    , fdp_write :: forall eff s . ConstRef s area -> Ivory eff ()
    , fdp_initDef :: Def('[]:->())
    , fdp_moduleDef :: ModuleDef
    , fdp_dataportid :: DataportId
    }

sharedState :: forall area . (IvoryArea area)
            => DataportId
            -> Maybe (Init area)
            -> EChronosDataport area
sharedState dataportid dpival = EChronosDataport
  { fdp_name = name
  , fdp_read = read
  , fdp_write = write
  , fdp_initDef = initDef
  , fdp_moduleDef = m
  , fdp_dataportid = dataportid
  }
  where
  unique :: String -> String
  unique n = n ++ ("_dataport" ++ (show (dp_id dataportid)))

  name = unique "sharedState"

  stateArea  :: MemArea area
  stateArea   = area (unique "sharedState_state") dpival
  state       = addrOf stateArea
  semArea    :: MemArea S.Semaphore
  semArea     = area (unique "sharedState_sem") Nothing
  sem         = addrOf semArea

  withSemaphore :: Ivory eff () -> Ivory eff ()
  withSemaphore action = do
    call_ S.takeBlocking sem
    action
    call_ S.give sem

  write :: ConstRef s area -> Ivory eff ()

  write v = call_ writeProc v

  writeProc :: Def ('[ConstRef s area] :-> ())
  writeProc = proc (unique "sharedState_write") $ \v -> body $
    withSemaphore $ refCopy state v

  read :: Ref s area -> Ivory eff ()
  read v = call_ readProc v

  readProc :: Def ('[Ref s area] :-> ())
  readProc = proc (unique "sharedState_read") $ \v -> body $
    withSemaphore $ refCopy v state

  initDef :: Def ('[] :-> ())
  initDef = proc (unique "sharedState_init") $ body $
    call_ S.create sem

  m = do
    incl initDef
    incl writeProc
    incl readProc
    private $ do
      defMemArea stateArea
      defMemArea semArea

