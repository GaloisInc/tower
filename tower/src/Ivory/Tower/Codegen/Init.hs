{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Codegen.Init
  ( CodegenInit(..)
  , codegenInit
  ) where

import qualified Ivory.Tower.AST as AST
import Ivory.Language

import qualified Ivory.OS.FreeRTOS.BinarySemaphore as Semaphore

data CodegenInit =
  CodegenInit
    { codegeninit_moddef  :: ModuleDef
    , codegeninit_init    :: forall eff . Ivory eff ()
    , codegeninit_block   :: forall eff . Ivory eff ()
    , codegeninit_unblock :: forall eff . Ivory eff ()
    }

codegenInit :: AST.Thread -> CodegenInit
codegenInit (AST.InitThread _ ) = CodegenInit
  { codegeninit_moddef = return ()
  , codegeninit_init   = return ()
  , codegeninit_block  = return ()
  , codegeninit_unblock = return ()
  }
codegenInit thr = CodegenInit
  { codegeninit_moddef = do
      Semaphore.moddef
      defMemArea sem_area
  , codegeninit_init   = call_ Semaphore.create sem
  , codegeninit_block  = call_ Semaphore.take sem
  , codegeninit_unblock = call_ Semaphore.give sem
  }
  where
  sem :: Ref Global (Stored Semaphore.BinarySemaphore)
  sem = addrOf sem_area
  sem_area :: MemArea (Stored Semaphore.BinarySemaphore)
  sem_area = area ("init_semaphore_" ++ AST.threadName thr) Nothing

