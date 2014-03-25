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
import           Ivory.Tower.Types.Signalable


data OS =
  OS
    { gen_channel :: forall (n :: Nat) area p
                   . (SingI n, IvoryArea area, IvoryZero area)
                  => AST.System p
                  -> AST.Chan
                  -> Proxy n
                  -> Proxy area
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

    , gen_signal :: forall eff p
                  . (Signalable p)
                 => AST.System p
                 -> SignalType p
                 -> (Ivory eff (), ModuleDef)

    , get_sigreceiver :: forall eff p
                       . (Signalable p)
                      => AST.System p
                      -> AST.SignalReceiver (SignalType p)
                      -> Ivory eff IBool

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

