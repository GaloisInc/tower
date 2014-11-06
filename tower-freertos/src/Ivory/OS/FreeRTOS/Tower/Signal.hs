{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.OS.FreeRTOS.Tower.Signal
  ( CodegenSignal(..)
  , codegenSignal
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Language

import qualified Ivory.OS.FreeRTOS.BinarySemaphore as Semaphore

data CodegenSignal =
  CodegenSignal
    { codegensignal_moddef  :: ModuleDef
    , codegensignal_init    :: forall eff . Ivory eff ()
    , codegensignal_wait    :: forall eff . Ivory eff ()
    , codegensignal_ready   :: forall eff . Ivory eff ()
    }

codegenSignal :: AST.Thread -> CodegenSignal
codegenSignal thr@(AST.SignalThread _ ) = CodegenSignal
  { codegensignal_moddef = do
      Semaphore.moddef
      defMemArea sem_area
  , codegensignal_init  = call_ Semaphore.create sem
  , codegensignal_wait  = call_ Semaphore.take sem
  , codegensignal_ready = call_ Semaphore.giveFromISR sem
  }
  where
  sem :: Ref Global (Stored Semaphore.BinarySemaphore)
  sem = addrOf sem_area
  sem_area :: MemArea (Stored Semaphore.BinarySemaphore)
  sem_area = area ("signal_semaphore_" ++ AST.threadName thr) Nothing
codegenSignal _ = CodegenSignal
  { codegensignal_moddef = return ()
  , codegensignal_init   = return ()
  , codegensignal_wait   = return ()
  , codegensignal_ready  = return ()
  }

