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

import           Prelude ()
import           Prelude.Compat

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

--------------------------------------------------------------------------------

type PackageName = String

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
    = AADLOutput ([PackageName], [I.Module] -> [I.Module])

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
      ( T.Emitter $ \ref -> I.call_ (procFromEmitter "") ref
      , AADLEmitter
         (\monName -> I.incl (procFromEmitter monName
                              :: I.Def('[I.ConstRef s b] 'I.:-> ())
                             ))
      )
      where
      sym = T.showUnique (A.emitter_name emitterAst)
      procFromEmitter :: I.IvoryArea b
                      => String
                      -> I.Def('[I.ConstRef s b] 'I.:-> ())
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
      ( map (\(AADLMonitor m) -> fst m) ms ++ actPkgs
      , \deps -> [ mkMod m deps | AADLMonitor m <- ms ]
              ++ actMods
      )
    where
    (actPkgs, actMods) = activeSrcs ast
    mkMod (nm, mMod) deps = I.package nm $ mapM_ I.depend deps >> mMod

activeSrcs :: A.Tower -> ([PackageName], [I.Module])
activeSrcs t = unzip $ map activeSrc (A.towerThreads t)

activeSrc :: A.Thread -> (PackageName, I.Module)
activeSrc t =
  case t of
    A.PeriodThread p
      -> ( pkg
         , I.package pkg $ do
           I.incl $ mkPeriodCallback p
           I.incl $ mkPeriodEmitter  p
         )
      where pkg = periodicCallback p
    A.InitThread{}
      -> ( initCallback
         , I.package initCallback $ do
           I.incl mkInitCallback
           I.incl mkInitEmitter
         )
    A.SignalThread s
      -> ( pkg
         , I.package pkg $ do
           I.incl $ mkSignalCallback s
           I.incl $ mkSignalEmitter  s
         )
      where pkg = signalCallback s

mkPeriodCallback :: A.Period
                 -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkPeriodCallback p =
  I.proc (periodicCallback p) $ \time -> I.body $
    I.call_ (mkPeriodEmitter p) time

mkPeriodEmitter :: A.Period -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkPeriodEmitter p = I.importProc (periodicEmitter p) (threadEmitterHeader $ A.PeriodThread p) -- XXX pass in higher up

mkInitCallback :: I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkInitCallback =
  I.proc initCallback $ \time -> I.body $
    I.call_ mkInitEmitter time

mkInitEmitter :: I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkInitEmitter = I.importProc initEmitter (threadEmitterHeader $ A.InitThread A.Init) -- XXX pass in higher up

mkSignalCallback :: A.Signal
                 -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkSignalCallback s =
  I.proc (signalCallback s) $ \time -> I.body $
    I.call_ (mkSignalEmitter s) time

mkSignalEmitter :: A.Signal -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkSignalEmitter s = I.importProc (signalEmitter s) (threadEmitterHeader $ A.SignalThread s)

genIvoryCode :: TowerBackendOutput AADLBackend
             -> T.Dependencies
             -> T.SignalCode
             -> ([String], [I.Module], [I.Located I.Artifact])
genIvoryCode
  (AADLOutput (packages, modsF))
  T.Dependencies
  { T.dependencies_modules   = modDeps
  , T.dependencies_depends   = depends
  , T.dependencies_artifacts = artifacts
  }
  T.SignalCode
  { T.signalcode_signals     = signals
  } = (packages, modules, artifacts)
  where
  modules = modDeps
         ++ modsF depends
         ++ go (mkSignalCode (modsF depends)) signals
  go c cs = M.elems $ M.mapWithKey c cs

mkSignalCode :: [I.Module] -> String -> T.GeneratedSignal -> I.Module
mkSignalCode deps sigNm
  T.GeneratedSignal { T.unGeneratedSignal = s }
  = I.package sigNm (mapM_ I.depend deps >> (s (return ())))

type TowerTime = I.Sint64
