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

import qualified Ivory.Tower.AST                as AST
import           Ivory.Tower.Backend
import           Ivory.Tower.Types.Backend
import qualified Ivory.Tower.Types.Dependencies as T
import qualified Ivory.Tower.Types.Emitter      as T
import qualified Ivory.Tower.Types.SignalCode   as T
import qualified Ivory.Tower.Types.Unique       as T
import qualified Ivory.Language.Syntax.AST as IAST
import qualified Ivory.Language.Syntax.Type as TIAST

import qualified Ivory.Language.Module as Mod

import           Tower.AADL.Names

import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import Ivory.Language.Proc (initialClosure, genVar)
import MonadLib (put)

--------------------------------------------------------------------------------

type PackageName = String

data AADLBackend = AADLBackend

instance TowerBackendTypes AADLBackend where
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


instance TowerBackend AADLBackend where
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
      sym = T.showUnique (AST.emitter_name emitterAst)
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
    handlerModules (SomeHandler (AADLHandler h)) = h (AST.monitorName ast)

  towerImpl _be ast ms =
    AADLOutput
      ( map (\(AADLMonitor m) -> fst m) ms ++ actPkgs
      , \deps -> [ mkMod m deps | AADLMonitor m <- ms ]
              ++ actMods
      )
    where
    (actPkgs, actMods) = activeSrcs ast
    mkMod (nm, mMod) deps = I.package nm $ mapM_ I.depend deps >> mMod

activeSrcs :: AST.Tower -> ([PackageName], [I.Module])
activeSrcs t = unzip $ map activeSrc (AST.towerThreads t)

activeSrc :: AST.Thread -> (PackageName, I.Module)
activeSrc t =
  case t of
    AST.PeriodThread p
      -> ( pkg
         , I.package pkg $ do
           I.incl $ mkPeriodCallback p
           I.incl $ mkPeriodEmitter  p
         )
      where pkg = periodicCallback p
    AST.InitThread{}
      -> ( initCallback
         , I.package initCallback $ do
           I.incl mkInitCallback
           I.incl mkInitEmitter
         )
    AST.SignalThread s
      -> ( pkg
         , I.package pkg $ do
           I.incl $ mkSignalCallback s
           I.incl $ mkSignalEmitter  s
         )
      where pkg = signalCallback s

mkPeriodCallback :: AST.Period
                 -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkPeriodCallback p =
  I.proc (periodicCallback p) $ \time -> I.body $
    I.call_ (mkPeriodEmitter p) time

mkPeriodEmitter :: AST.Period -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkPeriodEmitter p = I.importProc (periodicEmitter p) (threadEmitterHeader $ AST.PeriodThread p) -- XXX pass in higher up

mkInitCallback :: I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkInitCallback =
  I.proc initCallback $ \time -> I.body $
    I.call_ mkInitEmitter time

mkInitEmitter :: I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkInitEmitter = I.importProc initEmitter (threadEmitterHeader $ AST.InitThread AST.Init) -- XXX pass in higher up

mkSignalCallback :: AST.Signal
                 -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkSignalCallback s =
  I.proc (signalCallback s) $ \time -> I.body $
    I.call_ (mkSignalEmitter s) time

mkSignalEmitter :: AST.Signal -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkSignalEmitter s = I.importProc (signalEmitter s) (threadEmitterHeader $ AST.SignalThread s)

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



-------------------
-- * TOP DOWN ANALYSIS
-------------------


-- | Top-Down implementation of a callback. 
callbackImplTD :: T.Unique    -- ^ The name of the callback
               -> IAST.Proc   -- ^ The body of the callback (untyped ivory)
               -> I.ModuleDef -- ^ The ivory module containing the callback.
callbackImplTD sym f = 
  let p = f {IAST.procSym = (T.showUnique sym)}
      inclp = put (mempty { IAST.modProcs   = Mod.visAcc Mod.Public p }) in
  inclp


-- | Top-Down implementation of an emitter. 
emitterImplTD :: AST.Tower    -- ^ The full tower AST (as returned by the 'runTower' function).
              -> AST.Emitter  -- ^ The emitter AST to compile
              -> String       -- ^ The name of the monitor in which the emitter is defined.
              -> I.ModuleDef  -- ^ The untyped ivory implementation of the handler.
emitterImplTD tow ast monName = 
  let inclprocFromEmitter = put (mempty { IAST.modImports   = [procFromEmitter]}) in
  inclprocFromEmitter
  where
    emitter_type :: TIAST.Type
    emitter_type =  case IAST.procArgs $ NE.head $ AST.handler_callbacksAST $ subscribedHandler of
      [] -> err "callback without procedure arguments."
      a:_ -> TIAST.tType a

    subscribedHandler = 
      case filter (\x -> isListening $ AST.handler_chan x) allHandlers of
        [] -> err $ "no handler listening for the emitter " ++ sym
        a:_ -> a


    allHandlers = concat $ map (AST.monitor_handlers) (AST.tower_monitors tow)
    isListening (AST.ChanSync sc) = sc == (AST.emitter_chan ast)
    isListening _ = False

    sym = T.showUnique (AST.emitter_name ast)
    procFromEmitter :: IAST.Import
    procFromEmitter = IAST.Import
                      { IAST.importSym      = sym
                      , IAST.importFile     = hdr
                      , IAST.importRetTy    = TIAST.TyVoid
                      , IAST.importArgs     = [TIAST.Typed emitter_type var]
                      , IAST.importRequires = []
                      , IAST.importEnsures  = []
                      }
      where 
        hdr = smaccmPrefix $ monName ++ ".h"
        (var,_) = genVar initialClosure


-- | Top-Down implementation of a handler. 
handlerImplTD :: AST.Tower    -- ^ The full tower AST (as returned by the 'runTower' function).
              -> AST.Handler  -- ^ The handler AST to compile
              -> String       -- ^ The name of the monitor in which the handler is defined.
              -> I.ModuleDef  -- ^ The untyped ivory implemntation of the handler.
handlerImplTD tow ast monName =
  let cbdefs::(NE.NonEmpty I.ModuleDef) = NE.map (\(x,y) -> callbackImplTD x y) (NE.zip (AST.handler_callbacks ast) (AST.handler_callbacksAST ast)) in
  let emitters = map (emitterImplTD tow) $ AST.handler_emitters ast in
  let ems = [ e monName | e <- emitters ]
  in
  (sequence_ cbdefs) >> (mconcat ems)

-- | Top-Down implementation of a monitor. 
monitorImplTD :: AST.Tower                         -- ^ The full tower AST (as returned by the 'runTower' function).
              -> AST.Monitor                       -- ^ The monitor AST to compile.
              -> TowerBackendMonitor AADLBackend   -- ^ The name and untyped ivory implementation of the monitor.
monitorImplTD tow ast = 
  let (moddef::I.ModuleDef) = put $ AST.monitor_moduledef ast in
  let handlers = [ handlerImplTD tow hast (AST.monitorName ast) |hast <- reverse $ AST.monitor_handlers ast] in 
  AADLMonitor $
    ( nm
    , do mconcat handlers
         moddef
    )
  where
    nm = threadFile ast

err :: String -> a
err msg = error ("Tower.AADL.CodeGen: " ++ msg)