{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Tower.Mini where

import Control.Monad
import Data.List
import qualified Data.Map as M
--import Data.Maybe
import System.FilePath
--import System.Exit (exitFailure)
import Text.PrettyPrint.Mainland (putDoc, ppr)

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

type PackageName = String
type TowerTime = I.Sint64

data MiniBackend = MiniBackend
  deriving (Show)

instance TowerBackend MiniBackend where
  data TowerBackendCallback MiniBackend a = MiniCallback (String, ModuleDef)
  data TowerBackendEmitter MiniBackend   = MiniEmitter (TAST.Tower -> ModuleDef)
  newtype TowerBackendHandler MiniBackend a =
    MiniHandler ([TowerBackendCallback MiniBackend a], (TAST.Tower -> ModuleDef))
  data TowerBackendMonitor MiniBackend = MiniMonitor (String, (TAST.Tower -> ModuleDef))
  newtype TowerBackendOutput MiniBackend =
    MiniOutput ([PackageName], [Module] -> [Module])

  callbackImpl _be sym f =
      MiniCallback (nm, cbdef)
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
          forM_ targetHandlers $ \(MiniHandler (cbs, _)) ->
            forM_ cbs $ \(MiniCallback (nm, _)) ->
              call_ (importProc nm "" :: Def('[ConstRef s b] ':-> ())) ref
      , MiniEmitter (\tow -> mkDepends tow)
      )
      where
      mkDepends tow = forM_ targets $ \mon -> dependByName mon
        where targets = map threadFile mons
              mons = map fst (TAST.towerChanHandlers tow (TAST.ChanSync (TAST.emitter_chan emitterAst)))

  handlerImpl _be _ast emittersDefs callbacks =
    MiniHandler (callbacks, \tow -> do
      forM_ callbacks $ \(MiniCallback (_nm, cbdef)) -> cbdef
      mconcat (edefs tow))
    where
    edefs tow = map (\(MiniEmitter edef) -> edef tow) emittersDefs

  monitorImpl _be ast handlers moddef =
    MiniMonitor $
      ( nm
      , \tow -> do mconcat $ map (handlerModules tow) handlers
                   moddef
      )
    where
    nm = threadFile ast
    handlerModules :: TAST.Tower -> SomeHandler MiniBackend -> I.ModuleDef
    handlerModules tow (SomeHandler (MiniHandler h)) = snd h tow

  towerImpl _be ast mons =
    MiniOutput
      ( map (\(MiniMonitor m) -> fst m) mons-- ++ actPkgs
      , \deps -> [ mkMod m deps | MiniMonitor m <- mons ]
--              ++ actMods
      )
    where
--    (actPkgs, actMods) = activeSrcs ast
    mkMod (nm, mkMMod) deps = I.package nm $ mapM_ I.depend deps >> (mkMMod ast)


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

compileTowerMini :: (e -> MiniConfig) -> (TOpts -> IO e) -> Tower e () -> IO ()
compileTowerMini _fromEnv mkEnv twr' = do
  (copts, topts)              <- towerGetOpts
  env                         <- mkEnv topts
--  let cfg'                    =  fromEnv env
--  cfg                         <- parseMiniOpts' cfg' topts
  let (ast, code, deps, sigs) =  runTower MiniBackend twr' env
  putDoc $ ppr ast

  let (pkgs, mods, genAs)     = genIvoryCode code deps sigs

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

genIvoryCode :: TowerBackendOutput MiniBackend
             -> Dependencies
             -> SignalCode
             -> ([String], [Module], [Located Artifact])
genIvoryCode
  (MiniOutput (packages, modsF))
  Dependencies
  { dependencies_modules   = modDeps
  , dependencies_depends   = depends
  , dependencies_artifacts = artifacts
  }
  SignalCode
  { signalcode_signals = signals
  } = (packages, modules, artifacts)
  where
  modules = modDeps
         ++ modsF depends
         ++ go (mkSignalCode (modsF depends)) signals
  go c cs = M.elems $ M.mapWithKey c cs

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
