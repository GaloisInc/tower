{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Types.OS
  ( OS(..)
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.SystemCode
import           Ivory.Tower.Types.TaskCode


data OS =
  OS
    { gen_channel :: forall area
                   . (IvoryArea area, IvoryZero area)
                  => AST.System
                  -> AST.Chan
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

