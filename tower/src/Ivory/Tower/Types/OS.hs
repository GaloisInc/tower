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
import           Ivory.Tower.Types.Signalable
import           Ivory.Tower.Types.SignalCode
import           Ivory.Tower.Types.SystemCode
import           Ivory.Tower.Types.TaskCode

data OS =
  OS
    { gen_channel :: forall (n :: Nat) area p
                   . (ANat n, IvoryArea area, IvoryZero area)
                  => AST.System p
                  -> AST.Chan
                  -> Proxy n
                  -> Proxy area
                  -> Maybe (Init area)
                  -> (Def ('[]:->()), ModuleDef)

    , get_emitter :: forall area s eff p
                   . (IvoryArea area, IvoryZero area)
                  => AST.System p
                  -> AST.Chan
                  -> ConstRef s area
                  -> Ivory eff ()

    , get_receiver :: forall area s eff p
                    . (IvoryArea area, IvoryZero area)
                   => AST.System p
                   -> AST.ChanReceiver
                   -> Ref s area
                   -> Ivory eff IBool


    , get_reader  :: forall area s eff p
                    . (IvoryArea area, IvoryZero area)
                   => AST.System p
                   -> AST.ChanReader
                   -> Ref s area
                   -> Ivory eff IBool

    , gen_signal :: forall p
                  . (Signalable p)
                 => SignalType p
                 -> (forall eff . Ivory eff ())
                 -> SignalCode p

    , codegen_task :: forall p
                    . AST.System p
                   -> TaskCode
                   -> ([Module],ModuleDef)

    , codegen_sysinit :: forall p
                       . AST.System p
                      -> SystemCode
                      -> ModuleDef -- Collected task moduledefs (appended for each codegen_task)
                      -> [Module]
    }

