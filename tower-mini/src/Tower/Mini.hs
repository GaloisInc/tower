{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tower.Mini (
    -- * Compilation
    compileTowerMini
  , MiniConfig
  , defaultMiniConfig
  , miniConfigParser
    -- * Components
  , Comp.Component
  , Comp.component
  , Comp.tower
    -- ** Ports and External Channels
  , Comp.ExternalInput
  , Comp.ExternalOutput
  , Comp.ExternalChan
  , Comp.ExternalInputChan
  , Comp.ExternalOutputChan
  , Comp.mkExternalChan
  , Comp.mkExternalInputChan
  , Comp.mkExternalOutputChan
  , Comp.inputPortChan
  , Comp.outputPortChan
  , Comp.inputPortChan'
  , Comp.outputPortChan'
  , Comp.inputPort
  , Comp.outputPort
  , Comp.inputPort'
  , Comp.outputPort'
  ) where

import Prelude ()
import Prelude.Compat

import Data.List                 (partition)
import MonadLib                  (forM_, runWriterT, when)
import System.FilePath           ((</>))
import System.Exit               (exitFailure)
import Text.PrettyPrint.Mainland ((<+>), putDoc, text)

import qualified Data.Map as Map

import Ivory.Artifact as I
import Ivory.Language as I

import Ivory.Compile.C.CmdlineFrontend as C
import Ivory.Compile.C.Types as C

import Ivory.Tower                    (runTower, showUnique)
import Ivory.Tower.Backend            (SomeHandler(..), TowerBackend(..))
import Ivory.Tower.Config             (ConfigParser, subsection, withDefault)
import Ivory.Tower.Options            (TOpts, towerGetOpts)
import Ivory.Tower.Types.Dependencies (Dependencies(..))
import Ivory.Tower.Types.Emitter      (Emitter(..))
import Ivory.Tower.Types.SignalCode   (signalcode_signals)

import qualified Ivory.Tower.AST as TAST

import Tower.Mini.Component (Component(..), unComponentM)
import qualified Tower.Mini.Component as Comp

type PackageName = String

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

  -- Callbacks produce their 'ModDef' along with their name so that
  -- emitters can call them. This is mostly a hack to get around the
  -- awkward quantification that would arise if we just returned and
  -- later called a 'Def' from elsewhere.
  callbackImpl _be sym f =
      MiniCallback nm cbdef
      where nm = (showUnique sym)
            cbdef = incl
                  $ voidProc (showUnique sym)
                  $ \r -> body
                  $ noReturn
                  $ f r

  -- Emitters in minitower are just function calls, but since we
  -- don't have access to the 'Def's they call, we have to use
  -- 'importProc' to call them by name.
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
      -- In order to bring the targets into scope, we have to depend
      -- on the modules generated for their monitors. However, those
      -- modules haven't been created yet, so we have to late-bind and
      -- depend on them by name. It's unfortunate we have to duplicate
      -- the work of looking up the targets, but the 'targetHandlers'
      -- argument doesn't give us enough information.
      mkDepends tow = forM_ targets $ \mon -> dependByName mon
        where targets = map mkMonitorName mons
              mons = map fst (TAST.towerChanHandlers tow (TAST.ChanSync (TAST.emitter_chan emitterAst)))

  -- Handlers are relatively straightforward in minitower, but they
  -- do have to note what type of channel they handle so that we can
  -- later generate the glue that will run periodic and init
  -- handlers. Note that it is an error to have a signal handler in
  -- minitower.
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

  -- For monitors, we mostly have to worry about running each
  -- handler's 'ModDef', but we also have to do bookkeeping by
  -- tracking the names of periodic and init callbacks for later
  -- codegen.
  monitorImpl _be ast handlers moddef =
    MiniMonitor initCbs periodCbs nm genMod
    where
      nm = mkMonitorName ast
      genMod tow = do
        mconcat $ map (handlerModules tow) handlers
        moddef
      initCbs   = concat [ map callbackName cbs
                         | SomeHandler (MiniHandler Init cbs _) <- handlers ]
      periodCbs = concat [ map callbackName cbs
                         | SomeHandler (MiniHandler Period cbs _) <- handlers ]
      handlerModules :: TAST.Tower -> SomeHandler MiniBackend -> I.ModuleDef
      handlerModules tow (SomeHandler (MiniHandler _ _ mkMod)) = mkMod tow

  -- At this stage, we finally build the Ivory module for each
  -- monitor, and record the full information for periodic and init
  -- handlers (callback function symbol and monitor name) that is
  -- necessary for glue code generation.
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

mkMonitorName :: TAST.Monitor -> String
mkMonitorName m = TAST.monitorName m ++ "_monitor"

instance Monoid (TowerBackendOutput MiniBackend) where
    mempty = MiniOutput [] [] [] (const [])
    mappend mo1 mo2 =
      MiniOutput
        (outputInitCallbacks mo1   `mappend` outputInitCallbacks mo2)
        (outputPeriodCallbacks mo1 `mappend` outputPeriodCallbacks mo2)
        (outputPackageNames mo1    `mappend` outputPackageNames mo2)
        (\deps -> outputMkModules mo1 deps `mappend` outputMkModules mo2 deps)

--------------------------------------------------------------------------------

-- | Currently, there are no minitower options to configure, but
-- this parser remains for future compatibility.
miniConfigParser :: MiniConfig -> ConfigParser MiniConfig
miniConfigParser dflt = subsection "mini" p `withDefault` dflt
  where
  p = return dflt

-- | Currently, there are no minitower options to configure, but
-- this type remains for future compatibility.
data MiniConfig = MiniConfig
  deriving (Show)

defaultMiniConfig :: MiniConfig
defaultMiniConfig = MiniConfig

-- | Compile a minitower program into C from a set of
-- components. Currently, there are no minitower options to
-- configure, but these arguments remain for future compatibility.
compileTowerMini :: (e -> MiniConfig) -> (TOpts -> IO e) -> [Component e] -> IO ()
compileTowerMini _fromEnv mkEnv comps = do
  (copts, topts)               <- towerGetOpts
  env                          <- mkEnv topts
  outs <- mapM (\c -> (componentName c,) <$> buildComponent env c) comps
  forM_ outs $ \(name, (_ast, packages, modsF, deps)) -> do
    let mods = dependencies_modules deps ++ modsF (dependencies_depends deps)
        libAs = map addPrefix (dependencies_artifacts deps)
        addPrefix = id
        -- addPrefix l = case l of
        --   Src  a -> Root (artifactPath ("libminitower" </> "src") a)
        --   Incl a -> Root (artifactPath ("libminitower" </> "include") a)
        --   _      -> l
        copts' =
          case outDir copts of
            Nothing -> copts -- stdout
            Just f -> copts { outDir = Just (f </> name </> "src")
                            , outHdrDir = Just (f </> name </> "include")
                            , outArtDir = Just (f </> name)
                            }
    cmodules <- compileUnits mods copts'

    let (appMods, libMods) =
          partition (\m -> unitName m `elem` packages) cmodules

    outputCompiler appMods libAs copts'
    outputCompiler libMods []    copts'

-- | Build an individual minitower component. This is where much of
-- the action is, as we finally run the underlying 'Tower' program of
-- the component, and then build the glue code around the metadata
-- that the 'ComponentM' layer produces.
buildComponent :: e
               -> Component e
               -> IO (TAST.Tower,
                      [PackageName],
                      ([Module] -> [Module]),
                      Dependencies)
buildComponent env (Component nm comp) = do
  let t = runWriterT (unComponentM comp)
      (((), (modDefs, runFns)), ast, code, deps, sigs) = runTower MiniBackend t env
  when (not (Map.null (signalcode_signals sigs))) $ do
    putDoc $ "[minitower] error: components cannot contain signal handlers:" <+> text nm
    exitFailure
  when (not (null (outputPeriodCallbacks code))) $ do
    putDoc $ "[minitower] warning: component" <+> text nm <+> "contains periodic handlers;"
      <+> "these handlers will be called at a frequency determined by non-Tower code"
  let packages    = nm      : outputPackageNames code
      modsF deps' = compMod : outputMkModules code deps'
      -- The module for each component contains two functions: one
      -- that runs at each period, and one that runs at system
      -- initialization time.
      compMod = package nm $ do
        -- First add dependencies based on the periodic and init
        -- metadata produced by 'runTower'
        forM_ (outputInitCallbacks code) $ \(_, monName) ->
          dependByName monName
        forM_ (outputPeriodCallbacks code) $ \(_, monName) ->
          dependByName monName
        let entryProc :: Def('[ConstRef s ('Stored Sint64)] ':-> ())
            entryProc = voidProc "component_entry" $ \i -> body $ do
              -- in the periodic loop, first call each of the
              -- functions generated for the input and output ports of
              -- the component
              forM_ runFns call_
              -- pass along the value coming from the glue code to periodic callbacks
              forM_ (outputPeriodCallbacks code) $ \(cbName, _) -> do
                call_ (importProc cbName "" :: Def('[ConstRef s ('Stored Sint64)] ':-> ())) i
              retVoid
            initProc :: Def('[ConstRef s ('Stored Sint64)] ':-> ())
            initProc = voidProc "component_init" $ \i -> body $ do
              -- pass along the value coming from the glue code to init callbacks
              forM_ (outputInitCallbacks code) $ \(cbName, _) ->
                call_ (importProc cbName "" :: Def('[ConstRef s ('Stored Sint64)] ':-> ())) i
        private modDefs
        incl entryProc
        incl initProc
  return (ast, packages, modsF, deps)
