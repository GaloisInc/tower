{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.Types.OS
  ( OS(..)
  ) where

import           GHC.TypeLits
import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.SystemCode
import           Ivory.Tower.Types.TaskCode


data OS =
  OS
    { gen_channel :: forall (n :: Nat) area
                   . (SingI n, IvoryArea area, IvoryZero area)
                  => AST.System
                  -> AST.Chan
                  -> Proxy n
                  -> Proxy area
                  -> (Def ('[]:->()), ModuleDef)

    , get_emitter :: forall area s eff
                   . (IvoryArea area, IvoryZero area)
                  => AST.System
                  -> AST.Chan
                  -> ConstRef s area
                  -> Ivory eff ()

    , get_receiver :: forall area s eff
                    . (IvoryArea area, IvoryZero area)
                   => AST.System
                   -> AST.Task
                   -> AST.Chan
                   -> Ref s area
                   -> Ivory eff IBool

    , codegen_task :: AST.System
                   -> AST.Task
                   -> TaskCode
                   -> ([Module],ModuleDef)

    , codegen_sysinit :: AST.System
                      -> SystemCode
                      -> ModuleDef -- Collected task moduledefs (appended for each codegen_task)
                      -> [Module]
    }

