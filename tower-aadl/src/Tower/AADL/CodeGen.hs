{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Code generation of handler state machines, etc. for AADL targets.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.CodeGen where

import qualified Ivory.Language as I
import qualified Ivory.Compile.C.CmdlineFrontend as C

import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Backend
import qualified Ivory.Tower.Types.Dependencies as T
import qualified Ivory.Tower.Types.Emitter      as T
import qualified Ivory.Tower.Types.SignalCode   as T
import qualified Ivory.Tower.Types.Unique       as T

import qualified Data.Map.Strict as M
import           Data.Monoid

--------------------------------------------------------------------------------

data AADLBackend = AADLBackend

instance TowerBackend AADLBackend where
  newtype TowerBackendCallback AADLBackend a
    = AADLCallback I.ModuleDef
    deriving Monoid
  data    TowerBackendEmitter  AADLBackend    = AADLEmitter
  newtype TowerBackendHandler  AADLBackend  a = AADLHandler I.ModuleDef
   deriving Monoid
  -- Return monitor name and its modules.
  data TowerBackendMonitor     AADLBackend
    = AADLMonitor (String, I.ModuleDef)
  -- Pass in dependency modules
  newtype TowerBackendOutput   AADLBackend
    = AADLOutput ([I.Module] -> [I.Module])

  callbackImpl _be sym f =
        AADLCallback
      $ I.incl
      $ I.voidProc (T.showUnique sym)
      $ \r -> I.body
      $ I.noReturn
      $ f r

  emitterImpl _be emitterAst _impl =
    ( T.Emitter $ \ref -> I.call_ procFromEmitter ref
    , AADLEmitter
    )
    where
    sym = T.showUnique (AST.emitter_name emitterAst)
    procFromEmitter :: I.IvoryArea b
                    => I.Def('[I.ConstRef s b] I.:-> ())
    procFromEmitter = I.voidProc sym $ \_ref -> I.body I.retVoid

  handlerImpl _be _ast _emitters callbacks =
    AADLHandler $
      case mconcat callbacks of
        AADLCallback defs -> defs

  monitorImpl _be ast handlers moddef =
    AADLMonitor (nm, mconcat (map handlerModules handlers) >> moddef)
    where
    nm = T.showUnique (AST.monitor_name ast)
    handlerModules :: SomeHandler AADLBackend -> I.ModuleDef
    handlerModules (SomeHandler (AADLHandler h)) = h

  towerImpl _be _ast ms =
    AADLOutput $ \deps -> [ mkMod m deps | AADLMonitor m <- ms ]
    where
    mkMod (nm, mMod) deps = I.package nm $ mapM_ I.depend deps >> mMod

genIvoryCode :: C.Opts
             -> TowerBackendOutput AADLBackend
             -> T.Dependencies
             -> T.SignalCode
             -> IO ()
genIvoryCode opts
  (AADLOutput modsF)
  T.Dependencies
  { T.dependencies_modules   = mods
  , T.dependencies_depends   = depends
  , T.dependencies_artifacts = artifacts
  }
  T.SignalCode
  { T.signalcode_signals     = signals
  } = C.runCompiler modules artifacts opts
  where
  modules = mods
         ++ depends
         ++ modsF depends
         ++ go mkSignalCode  signals
  go c cs = M.elems $ M.mapWithKey c cs

mkSignalCode :: String -> T.GeneratedSignal -> I.Module
mkSignalCode sigNm
  T.GeneratedSignal { T.unGeneratedSignal = s }
  -- XXX assuming for now that we don't have unsafe signals. Pass the platform
  -- signal continuation here for eChronos.
  = I.package sigNm (s (return ()))
