{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Compile.FreeRTOS.EventNotify
  ( EventNotify(..)
  , taskEventNotify
  ) where

import           Ivory.Language
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Types.Time

import qualified Ivory.OS.FreeRTOS.CountingSemaphore as S

data EventNotify =
  EventNotify
    { evtn_trigger           :: forall eff . Ivory eff ()
    , evtn_trigger_from_isr  :: forall eff . Ivory eff ()
    , evtn_guard             :: forall eff . ITime -> Ivory eff ()
    , evtn_init              :: forall eff . Ivory eff ()
    , evtn_code              :: ModuleDef
    }

taskEventNotify :: Unique -> EventNotify
taskEventNotify taskname = EventNotify
  { evtn_trigger           = call_ trigger
  , evtn_trigger_from_isr  = call_ trigger_from_isr
  , evtn_guard             = call_ guard
  , evtn_init              = call_ ini
  , evtn_code              = code
  }
  where
  sem_area = area (named "semaphore") Nothing
  sem_ref :: Ref Global (Stored S.CountingSemaphore)
  sem_ref = addrOf sem_area

  ini :: Def('[]:->())
  ini = proc (named "init") $ body $ do
    -- currently use maxBound for count maximum, but perhaps we
    -- may want to limit it?
    call_ S.create sem_ref maxBound 0

  trigger :: Def('[]:->())
  trigger = proc (named "trigger") $ body $ do
    call_ S.give sem_ref

  trigger_from_isr :: Def('[]:->())
  trigger_from_isr  = proc (named "trigger_from_isr") $ body $ do
    call_ S.giveFromISR sem_ref

  guard :: Def('[ITime]:->())
  guard = proc (named "guard") $ \time -> body $ do
    let time_micros = toIMicroseconds time
    time_millis <- assign (castWith maxBound (time_micros `iDiv` 1000))
    call_ S.take sem_ref time_millis

  code = do
    inclHeader "freertos_semaphore_wrapper.h"
    sourceDep "freertos_semaphore_wrapper.h"
    sourceDep "freertos_semaphore_wrapper.c"
    incl ini
    incl trigger
    incl trigger_from_isr
    incl guard
    defMemArea sem_area

  named n = "eventnotify_" ++ n ++ "_" ++ tn
  tn = showUnique taskname


