{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Types.OS
  ( OS(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Unique

import qualified Ivory.Tower.AST as AST

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

    , get_reader :: forall area s eff
                  . (IvoryArea area, IvoryZero area)
                 => AST.System
                 -> AST.Task
                 -> AST.Chan
                 -> Ref s area
                 -> Ivory eff ()
    }

