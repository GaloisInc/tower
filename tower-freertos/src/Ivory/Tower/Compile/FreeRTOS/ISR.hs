{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Compile.FreeRTOS.ISR
  ( gen_signal
  , get_sigreceiver
  ) where

import           Data.List (find)
import           Ivory.Language
import           Ivory.Tower
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.SignalCode

import qualified Ivory.OS.FreeRTOS.BinarySemaphore as S
import           Ivory.Tower.Compile.FreeRTOS.EventNotify

gen_signal :: forall p
            . (Signalable p)
           => SignalType p
           -> (forall eff . Ivory eff ())
           -> SignalCode p
gen_signal sig callback = SignalCode
  { signalcode_init = \sys -> mapM_ isrsignal_init (isrsigs sys)
  , signalcode_moddef = code
  , signalcode_receiver = get_sigreceiver
  }
  where
  signame = signalName sig
  handlerproc :: AST.System p -> Def('[]:->())
  handlerproc sys = proc signame $ body $ do
    callback
    mapM_ isrsignal_send (isrsigs sys)
  code sys = do
    mapM_ isrsignal_codegen (isrsigs sys)
    incl (handlerproc sys)

  isrsigs sys = map (uncurry isrSignal) (AST.signal_receivers sys sig)

get_sigreceiver :: forall p eff
                 . (Signalable p)
                => AST.System p
                -> AST.SignalReceiver (SignalType p)
                -> Ivory eff IBool
get_sigreceiver sys sigrxer = isrsignal_check (isrSignal sigrxer rxingtask)
  where
  rxingtask = snd . maybe (error "impossible get_sigreceiver") id $
              find (\(rxer, _) -> eqrxers rxer sigrxer)
                   (AST.signal_receivers sys (unrxer sigrxer))
  eqrxers a b = signalName (unrxer a) == signalName (unrxer b)
  unrxer = AST.signalreceiver_signal

data ISRSignal =
  ISRSignal
    { isrsignal_codegen :: ModuleDef
    , isrsignal_init    :: forall eff . Ivory eff ()
    , isrsignal_send    :: forall eff . Ivory eff ()
    , isrsignal_check   :: forall eff . Ivory eff IBool
    }

isrSignal :: (Signalable p)
          => AST.SignalReceiver (SignalType p)
          -> AST.Task p
          -> ISRSignal
isrSignal sigrxer receivingtask = ISRSignal
  { isrsignal_codegen = moddef
  , isrsignal_init    = ini
  , isrsignal_send    = send
  , isrsignal_check   = check
  }
  where
  named n = n ++ "_" ++  showUnique (AST.signalreceiver_name sigrxer)
  ready_area :: MemArea (Stored S.BinarySemaphore)
  ready_area = area (named "ready") Nothing
  ready = addrOf ready_area

  ini = call_ S.create ready

  check = call S.take ready 0

  send = do
    call_ S.giveFromISR ready
    evtn_trigger_from_isr (taskEventNotify (AST.task_name receivingtask))

  moddef = do
    defMemArea ready_area

