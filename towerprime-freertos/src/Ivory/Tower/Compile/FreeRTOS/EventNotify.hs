{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Compile.FreeRTOS.EventNotify
  ( EventNotify(..)
  , taskEventNotify
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Types.Time

import qualified Ivory.OS.FreeRTOS.Semaphore as S

data EventNotify =
  EventNotify
    { evtn_trigger :: forall eff . Ivory eff ()
    , evtn_guard   :: forall eff . ITime -> Ivory eff ()
    , evtn_init    :: forall eff . Ivory eff ()
    , evtn_code    :: ModuleDef
    }

taskEventNotify :: AST.Task -> EventNotify
taskEventNotify taskast = EventNotify
  { evtn_trigger = call_ trigger
  , evtn_guard   = call_ guard
  , evtn_init    = call_ ini
  , evtn_code    = code
  }
  where
  sem_area = area (named "semaphore") Nothing
  sem_ref :: Ref Global (Stored S.CountingSemaphore)
  sem_ref = addrOf sem_area

  ini :: Def('[]:->())
  ini = proc (named "init") $ body $ do
    call_ S.create sem_ref maxBound 0 -- XXX may want to limit max count?

  trigger :: Def('[]:->())
  trigger = proc (named "trigger") $ body $ do
    call_ S.give sem_ref

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
    incl guard
    defMemArea sem_area

  named n = "eventnotify_" ++ n ++ "_" ++ tn
  tn = showUnique (AST.task_name taskast)


