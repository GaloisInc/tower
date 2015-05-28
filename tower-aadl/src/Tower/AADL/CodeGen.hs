{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  data    TowerBackendEmitter  AADLBackend    = AADLEmitter (String -> I.ModuleDef)
  newtype TowerBackendHandler  AADLBackend  a = AADLHandler (String -> I.ModuleDef)
   deriving Monoid
  -- Takes a ModuleDef (containing the emitter declaration) and returns the
  -- monitor module name and monitor module.
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

  emitterImpl _be emitterAst _impl = emitterCode
    where
    emitterCode :: forall b. I.IvoryArea b
                => (T.Emitter b, TowerBackendEmitter AADLBackend)
    emitterCode =
      ( T.Emitter $ \ref -> I.call_ (procFromEmitter undefined) ref
      , AADLEmitter
         (\monName -> I.incl (procFromEmitter monName
                              :: I.Def('[I.ConstRef s b] I.:-> ())
                             ))
      )
      where
      sym = T.showUnique (A.emitter_name emitterAst)
      procFromEmitter :: I.IvoryArea b
                      => String
                      -> I.Def('[I.ConstRef s b] I.:-> ())
      procFromEmitter monName = I.importProc sym hdr
        where hdr = smaccmPrefix $ monName ++ ".h"

  handlerImpl _be _ast emittersDefs callbacks =
    AADLHandler $
      case mconcat callbacks of
        AADLCallback cdefs -> \monName -> cdefs >> mconcat (edefs monName)
    where
    edefs monName = map (\(AADLEmitter edef) -> edef monName) emittersDefs

  monitorImpl _be ast handlers moddef =
    AADLMonitor $
      ( nm
      , do mconcat $ map handlerModules handlers
           moddef
      )
    where
    nm = threadFile ast
    handlerModules :: SomeHandler AADLBackend -> I.ModuleDef
    handlerModules (SomeHandler (AADLHandler h)) = h (A.monitorName ast)

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
      -> Just $ I.package (periodicCallback p) $ do
           I.incl $ mkPerCallback p
           I.incl $ emitter p
    _ -> Nothing
  where
  mkPerCallback :: A.Period
                -> I.Def ('[I.ConstRef s (I.Stored I.Sint64)] I.:-> ())
  mkPerCallback p =
    I.proc (periodicCallback p)
    $ \time -> I.body
    $ I.call_ (emitter p) time
  emitter :: A.Period -> I.Def ('[I.ConstRef s (I.Stored I.Sint64)] I.:-> ())
  emitter p = I.importProc (periodicEmitter p) (threadEmitterHeader t)

genIvoryCode :: TowerBackendOutput AADLBackend
             -> T.Dependencies
             -> T.SignalCode
             -> ([I.Module], [I.Located I.Artifact])
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
         ++ modsF depends
         ++ go mkSignalCode signals
  go c cs = M.elems $ M.mapWithKey c cs

mkSignalCode :: String -> T.GeneratedSignal -> I.Module
mkSignalCode sigNm
  T.GeneratedSignal { T.unGeneratedSignal = s }
  -- XXX assuming for now that we don't have unsafe signals. Pass the platform
  -- signal continuation here for eChronos.
  = I.package sigNm (s (return ()))
