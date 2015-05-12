{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Code generation for AADL targets.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.CodeGen where

import qualified Ivory.Language as I
import qualified Ivory.Artifact as I

import qualified Ivory.Tower.AST                as A
import           Ivory.Tower.Backend
import qualified Ivory.Tower.Types.Dependencies as T
import qualified Ivory.Tower.Types.Emitter      as T
import qualified Ivory.Tower.Types.SignalCode   as T
import qualified Ivory.Tower.Types.Unique       as T

import           Tower.AADL.Names

import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Maybe (catMaybes)

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
    sym = T.showUnique (A.emitter_name emitterAst)
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
    nm = threadFile ast
    handlerModules :: SomeHandler AADLBackend -> I.ModuleDef
    handlerModules (SomeHandler (AADLHandler h)) = h

  towerImpl _be ast ms =
      AADLOutput
    $ \deps -> [ mkMod m deps | AADLMonitor m <- ms ]
      ++ activeSrcs ast
    where
    mkMod (nm, mMod) deps = I.package nm $ mapM_ I.depend deps >> mMod

activeSrcs :: A.Tower -> [I.Module]
activeSrcs t = catMaybes $ map activeSrc (A.towerThreads t)

activeSrc :: A.Thread -> Maybe I.Module
activeSrc t =
  case t of
    A.PeriodThread p
      -> Just $ I.package (periodicCallback p) $ I.incl $ mkPerCallback p
    _ -> Nothing
  where
  mkPerCallback :: A.Period
                -> I.Def ('[I.ConstRef s (I.Stored I.Sint64)] I.:-> ())
  mkPerCallback p =
    I.proc (periodicCallback p)
    $ \time -> I.body
    $ I.call_ (emitter p) time
  emitter :: A.Period -> I.Def ('[I.ConstRef s (I.Stored I.Sint64)] I.:-> ())
  emitter p = I.externProc (periodicEmitter p)

genIvoryCode :: TowerBackendOutput AADLBackend
             -> T.Dependencies
             -> T.SignalCode
             -> ([I.Module],[I.Artifact])
genIvoryCode
  (AADLOutput modsF)
  T.Dependencies
  { T.dependencies_modules   = mods
  , T.dependencies_depends   = depends
  , T.dependencies_artifacts = artifacts
  }
  T.SignalCode
  { T.signalcode_signals     = signals
  } = (modules,artifacts)
  where
  modules = mods
         ++ depends
         ++ modsF depends
         ++ go mkSignalCode signals
  go c cs = M.elems $ M.mapWithKey c cs

mkSignalCode :: String -> T.GeneratedSignal -> I.Module
mkSignalCode sigNm
  T.GeneratedSignal { T.unGeneratedSignal = s }
  -- XXX assuming for now that we don't have unsafe signals. Pass the platform
  -- signal continuation here for eChronos.
  = I.package sigNm (s (return ()))
