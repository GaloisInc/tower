{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Tower.Mini where

import Control.Monad
import Data.List
import qualified Data.Map as M
import MonadLib hiding (local)
import System.FilePath
import System.Exit (exitFailure)
import Text.PrettyPrint.Mainland ((<+>), putDoc, ppr, text)

import Ivory.Artifact as I
import Ivory.Language as I

import Ivory.Compile.C.CmdlineFrontend as C
import Ivory.Compile.C.Types as C

import Ivory.Tower hiding (prettyTime, systemInit)
import qualified Ivory.Tower as T
import qualified Ivory.Tower.AST as TAST
--import qualified Ivory.Tower.AST.Period as TAST
--import qualified Ivory.Tower.AST.Signal as TAST
import Ivory.Tower.Backend
import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.Tower.Types.Dependencies
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.SignalCode
--import Ivory.Tower.Types.Unique

import Tower.Mini.Component

type PackageName = String
type TowerTime = I.Sint64

data HandlerType = Sync | Period | Init deriving (Show)

data MiniBackend = MiniBackend
  deriving (Show)

instance TowerBackend MiniBackend where
  data TowerBackendCallback MiniBackend a =
    MiniCallback {
        callbackName :: String
      , callbackDef  :: ModuleDef
      }
  data TowerBackendEmitter MiniBackend   =
    MiniEmitter {
        emitterDef :: TAST.Tower -> ModuleDef
      }
  data TowerBackendHandler MiniBackend a =
    MiniHandler {
        handlerType :: HandlerType
      , handlerCallbacks :: [TowerBackendCallback MiniBackend a]
      , handlerDef :: TAST.Tower -> ModuleDef
      }
  data TowerBackendMonitor MiniBackend =
    MiniMonitor {
        monitorInitCallbacks :: [String]
      , monitorPeriodCallbacks :: [String]
      , monitorName :: String
      , monitorDef :: (TAST.Tower -> ModuleDef)
      }
  data TowerBackendOutput MiniBackend =
    MiniOutput {
        outputInitCallbacks :: [(String, PackageName)]
      , outputPeriodCallbacks :: [(String, PackageName)]
      , outputPackageNames ::[PackageName]
      , outputMkModules :: [Module] -> [Module]
      }

  callbackImpl _be sym f =
      MiniCallback nm cbdef
      where nm = (showUnique sym)
            cbdef = incl
                  $ voidProc (showUnique sym)
                  $ \r -> body
                  $ noReturn
                  $ f r

  emitterImpl _be emitterAst targetHandlers = emitterCode
    where
    emitterCode :: forall b. IvoryArea b
                => (Emitter b, TowerBackendEmitter MiniBackend)
    emitterCode =
      ( Emitter $ \ref ->
          forM_ targetHandlers $ \(MiniHandler _ cbs _) ->
            forM_ cbs $ \cb ->
              call_ (importProc (callbackName cb) "" :: Def('[ConstRef s b] ':-> ())) ref
      , MiniEmitter (\tow -> mkDepends tow)
      )
      where
      mkDepends tow = forM_ targets $ \mon -> dependByName mon
        where targets = map threadFile mons
              mons = map fst (TAST.towerChanHandlers tow (TAST.ChanSync (TAST.emitter_chan emitterAst)))

  handlerImpl _be ast emittersDefs callbacks =
    MiniHandler ty callbacks mkMod
    where
      ty = case TAST.handler_chan ast of
        TAST.ChanSync   _ -> Sync
        TAST.ChanPeriod _ -> Period
        TAST.ChanInit   _ -> Init
        TAST.ChanSignal _ ->
          error "Tower.Mini: Signal handling not supported directly; use component inputPort instead"
      edefs tow = map (\e -> emitterDef e tow) emittersDefs
      mkMod tow = do
        mapM_ callbackDef callbacks
        mconcat (edefs tow)

  monitorImpl _be ast handlers moddef =
    MiniMonitor initCbs periodCbs nm genMod
    where
      nm = threadFile ast
      genMod tow = do
        mconcat $ map (handlerModules tow) handlers
        moddef
      initCbs   = concat [ map callbackName cbs
                         | SomeHandler (MiniHandler Init cbs _) <- handlers ]
      periodCbs = concat [ map callbackName cbs
                         | SomeHandler (MiniHandler Period cbs _) <- handlers ]
      handlerModules :: TAST.Tower -> SomeHandler MiniBackend -> I.ModuleDef
      handlerModules tow (SomeHandler (MiniHandler _ _ mkMod)) = mkMod tow

  towerImpl _be ast mons =
    MiniOutput initCbs periodCbs packageNames mkModules
    where
      initCbs   = concat [ zip cbNames (repeat monName)
                         | MiniMonitor cbNames _ monName _ <- mons]
      periodCbs = concat [ zip cbNames (repeat monName)
                         | MiniMonitor _ cbNames monName _ <- mons]
      packageNames = map monitorName mons
      mkModules deps = [ mkMod (monName, monDef) deps
                       | MiniMonitor _ _ monName monDef <- mons ]
      mkMod (nm, mkMMod) deps = I.package nm $ mapM_ I.depend deps >> (mkMMod ast)

instance Monoid (TowerBackendOutput MiniBackend) where
    mempty = MiniOutput [] [] [] (const [])
    mappend mo1 mo2 =
      MiniOutput
        (outputInitCallbacks mo1   `mappend` outputInitCallbacks mo2)
        (outputPeriodCallbacks mo1 `mappend` outputPeriodCallbacks mo2)
        (outputPackageNames mo1    `mappend` outputPackageNames mo2)
        (\deps -> outputMkModules mo1 deps `mappend` outputMkModules mo2 deps)
      
-- introduce component packages in this way?
activeSrcs :: TAST.Tower -> ([PackageName], [I.Module])
activeSrcs t = unzip $ map activeSrc (TAST.towerThreads t)

activeSrc :: TAST.Thread -> (PackageName, I.Module)
activeSrc t =
  case t of
    TAST.PeriodThread p
      -> ( pkg
         , I.package pkg $ do
           I.incl $ mkPeriodCallback p
           I.incl $ mkPeriodEmitter  p
         )
      where pkg = periodicCallback p
    TAST.InitThread{}
      -> ( initCallback
         , I.package initCallback $ do
           I.incl mkInitCallback
           I.incl mkInitEmitter
         )
    TAST.SignalThread s
      -> ( pkg
         , I.package pkg $ do
           I.incl $ mkSignalCallback s
           I.incl $ mkSignalEmitter  s
         )
      where pkg = signalCallback s

mkPeriodCallback :: TAST.Period
                 -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkPeriodCallback p =
  I.proc (periodicCallback p) $ \time -> I.body $
    I.call_ (mkPeriodEmitter p) time

mkPeriodEmitter :: TAST.Period -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkPeriodEmitter p = I.importProc (periodicEmitter p) (threadEmitterHeader $ TAST.PeriodThread p) -- XXX pass in higher up

mkInitCallback :: I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkInitCallback =
  I.proc initCallback $ \time -> I.body $
    I.call_ mkInitEmitter time

mkInitEmitter :: I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkInitEmitter = I.importProc initEmitter (threadEmitterHeader $ TAST.InitThread TAST.Init) -- XXX pass in higher up

mkSignalCallback :: TAST.Signal
                 -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkSignalCallback s =
  I.proc (signalCallback s) $ \time -> I.body $
    I.call_ (mkSignalEmitter s) time

mkSignalEmitter :: TAST.Signal -> I.Def ('[I.ConstRef s ('I.Stored TowerTime)] 'I.:-> ())
mkSignalEmitter s = I.importProc (signalEmitter s) (threadEmitterHeader $ TAST.SignalThread s)

--------------------------------------------------------------------------------

miniConfigParser :: MiniConfig -> ConfigParser MiniConfig
miniConfigParser dflt = subsection "mini" p `withDefault` dflt
  where
  p = return dflt

data MiniConfig = MiniConfig
  deriving (Show)

defaultMiniConfig :: MiniConfig
defaultMiniConfig = MiniConfig

compileTowerMini :: (e -> MiniConfig) -> (TOpts -> IO e) -> [Component e] -> IO ()
compileTowerMini _fromEnv mkEnv comps = do
  (copts, topts)              <- towerGetOpts
  env                         <- mkEnv topts
--  let cfg'                    =  fromEnv env
--  cfg                         <- parseMiniOpts' cfg' topts
  (ast, packages, modsF, deps) <- mconcat `fmap` (mapM (buildComponent env) comps)
  putDoc $ ppr ast

  let (pkgs, mods, genAs)     = genIvoryCode packages modsF deps

  let addPrefix l = case l of
        Src  a -> Root (artifactPath ("libmini" </> "src") a)
        Incl a -> Root (artifactPath ("libmini" </> "include") a)
        _      -> l

  let libAs                   = map addPrefix genAs

--  let appname                 = takeFileName (fromMaybe "tower" (outDir copts))

  -- let as :: OSSpecific a -> [Located Artifact]
  --     as os = doc_as
  --       ++ libAs
  --       ++ [ Root deps_a
  --          , graphvizArtifact appname ast
  --          ]
  --       ++ osSpecificArtifacts os appname cfg (aadlDocNames aadl_docs)

  -- unless (validCIdent appname) $ error $ "appname must be valid c identifier; '"
  --                                       ++ appname ++ "' is not"

  cmodules <- compileUnits mods copts
  let (appMods, libMods) =
        partition (\m -> unitName m `elem` pkgs) cmodules
  outputCompiler appMods libAs copts
  outputCompiler libMods []    copts
  -- where

  -- -- | AADL assumes that our handlers will always have a callback define. So we
  -- -- search the Tower AST looking for handlers that missing callbacks.
  -- handlersMissingCallbacks :: A.Tower -> [A.Handler]
  -- handlersMissingCallbacks = concatMap monitorHasEmptyHandler . A.tower_monitors
  --   where
  --   monitorHasEmptyHandler :: A.Monitor -> [A.Handler]
  --   monitorHasEmptyHandler = catMaybes . map handlerIsEmpty . A.monitor_handlers
  --   handlerIsEmpty :: A.Handler -> Maybe A.Handler
  --   handlerIsEmpty h = if (null (A.handler_callbacks h))
  --                         then Just h
  --                         else Nothing
  return ()

buildComponent :: e
               -> Component e
               -> IO (TAST.Tower,
                      [PackageName],
                      ([Module] -> [Module]),
                      Dependencies)
buildComponent env (Component nm comp) = do
  let t = runWriterT (unComponentM comp)
      (((), (modDefs, runFns)), ast, code, deps, sigs) = runTower MiniBackend t env
  when (not (M.null (signalcode_signals sigs))) $ do
    putDoc $ "[tower-mini] error: components cannot contain signal handlers:" <+> text nm
    exitFailure
  when (not (null (outputPeriodCallbacks code))) $ do
    putDoc $ "[tower-mini] warning: component" <+> text nm <+> "contains periodic handlers;"
      <+> "these handlers will be called at a frequency determined by non-Tower code"
  let packages    = nm      : outputPackageNames code
      modsF deps' = compMod : outputMkModules code deps'
      compMod = package nm $ do
        forM_ (outputInitCallbacks code) $ \(_, monName) ->
          dependByName monName
        forM_ (outputPeriodCallbacks code) $ \(_, monName) ->
          dependByName monName
        let entryProc :: Def('[] ':-> ())
            entryProc = voidProc "component_entry" $ body $ do
              forM_ runFns call_
              zero <- constRef `fmap` local izero
              forM_ (outputPeriodCallbacks code) $ \(cbName, _) -> do
                call_ (importProc cbName "" :: Def('[ConstRef s ('Stored Sint64)] ':-> ())) zero
              retVoid
            initProc :: Def('[] ':-> ())
            initProc = voidProc "component_init" $ body $ do
              zero <- constRef `fmap` local izero              
              forM_ (outputInitCallbacks code) $ \(cbName, _) ->
                call_ (importProc cbName "" :: Def('[ConstRef s ('Stored Sint64)] ':-> ())) zero
        private modDefs
        incl entryProc
        incl initProc
  return (ast, packages, modsF, deps)
  

genIvoryCode :: [PackageName]
             -> ([Module] -> [Module])
             -> Dependencies
             -> ([PackageName], [Module], [Located Artifact])
genIvoryCode
  packages
  modsF
  Dependencies
  { dependencies_modules   = modDeps
  , dependencies_depends   = depends
  , dependencies_artifacts = artifacts
  } = (packages, modules, artifacts)
  where
  modules = modDeps ++ modsF depends

mkSignalCode :: [Module] -> String -> GeneratedSignal -> Module
mkSignalCode deps sigNm GeneratedSignal { unGeneratedSignal = s }
  = package sigNm (mapM_ depend deps >> (s (return ())))

-- | parseAADLOpts' is a wrapper around parseAADLOpts that
-- checks for errors and if '--help' was requested.
-- It calls exitFailure in the case of errors or help.
-- parseMiniOpts' :: MiniConfig -> TOpts -> IO MiniConfig
-- parseMiniOpts' cfg topts =
--   case parseMiniOpts cfg topts of
--   (c, [],        _) -> do
--     case topts_help topts of
--       True  -> topts_error topts "Usage:"
--       False -> return ()
--     return c
--   (_, _, _) -> finalizeOpts topts >> exitFailure


--------------------------------------------------------------------------------
-- Names

smaccmPrefix :: String -> String
smaccmPrefix = ("smaccm_" ++)

threadEmitterHeader :: TAST.Thread -> String
threadEmitterHeader t =
  smaccmPrefix $ TAST.threadName t ++ ".h"

------------------------------------------------------------

periodicEmitter :: TAST.Period -> String
periodicEmitter p = "emitter_" ++ prettyTime p

periodicCallback :: TAST.Period -> String
periodicCallback p = "callback_" ++ prettyTime p

------------------------------------------------------------

systemInit :: String
systemInit = "systemInit"

initEmitter :: String
initEmitter = "emitter_" ++ systemInit

initCallback :: String
initCallback = "callback_" ++ systemInit

------------------------------------------------------------

signalEmitter :: TAST.Signal -> String
signalEmitter s = "emitter_" ++ TAST.signal_name s

signalCallback :: TAST.Signal -> String
signalCallback s = "callback_" ++ TAST.signal_name s

------------------------------------------------------------

prettyTime :: TAST.Period -> String
prettyTime p = T.prettyTime (TAST.period_dt p)

threadFile :: TAST.Monitor -> String
threadFile m = TAST.monitorName m ++ "_monitor"
