{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Compile.AADL
  ( os
  , systemDoc
  , searchDir
  ) where

import           GHC.TypeLits

import           Ivory.Language
import           Ivory.Tower
import qualified Ivory.Tower.AST as AST
import qualified Ivory.Tower.Types.OS as OS
import           Ivory.Tower.Types.TaskCode
import           Ivory.Tower.Types.SignalCode
import           Ivory.Tower.Types.SystemCode

import Ivory.Tower.Compile.AADL.SearchDir (searchDir)
import Ivory.Tower.Compile.AADL.SystemDoc (systemDoc)


os :: OS.OS
os = OS.OS
  { OS.gen_channel     = gen_channel
  , OS.get_emitter     = get_emitter
  , OS.get_receiver    = get_receiver
  , OS.get_reader      = get_reader
  , OS.gen_signal      = gen_signal
  , OS.codegen_task    = codegen_task
  , OS.codegen_sysinit = codegen_sysinit
  }

gen_channel :: forall (n :: Nat) area p
             . (SingI n, IvoryArea area, IvoryZero area)
            => AST.System p
            -> AST.Chan
            -> Proxy n
            -> Proxy area
            -> Maybe (Init area)
            -> (Def('[]:->()), ModuleDef)
gen_channel _sys _chan _n _area _i =
  (error "gen_channel unimplemented", return ())

get_emitter :: forall area eff s p
             . (IvoryArea area, IvoryZero area)
            => AST.System p
            -> AST.Chan
            -> ConstRef s area
            -> Ivory eff ()
get_emitter _sys _chan = const (return ())


get_receiver :: forall p area eff s
              . (IvoryArea area, IvoryZero area)
             => AST.System p
             -> AST.ChanReceiver
             -> Ref s area
             -> Ivory eff IBool
get_receiver _sys _chanrxer = const (return false)

get_reader :: forall p area eff s
            . (IvoryArea area, IvoryZero area)
           => AST.System p
           -> AST.ChanReader
           -> Ref s area
           -> Ivory eff IBool
get_reader _sys _chanreader = const (return false)


gen_signal :: forall p
            . (Signalable p)
           => SignalType p
           -> (forall eff . Ivory eff ())
           -> SignalCode p
gen_signal _sig _callback = SignalCode
  { signalcode_init = ini
  , signalcode_moddef = moddef
  , signalcode_receiver = rx
  }
  where
  ini _sys = return ()
  moddef _sys = return ()
  rx _sys _rxer = return false

codegen_task :: AST.System p
             -> TaskCode
             -> ([Module],ModuleDef)
codegen_task _sys taskcode = ([loop_mod, user_mod], deps)
  where
  deps = do
    depend user_mod
    depend loop_mod

  user_mod = package (named "tower_task_usercode") $ do
    depend loop_mod

  loop_mod = package (named "tower_task_loop") $ do
    depend user_mod

  named n = n ++ "_" ++ (showUnique (taskcode_taskname taskcode))

codegen_sysinit :: AST.System p
                -> SystemCode
                -> ModuleDef
                -> [Module]
codegen_sysinit _sysast _syscode _taskmoddefs = []

