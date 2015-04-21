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

import           Data.List (nub)
import qualified Data.DList      as D
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Control.Applicative

import           MonadLib

--------------------------------------------------------------------------------

type AADLBackend = (UniqueState, Warnings)

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

  uniqueImpl be@(st, ws) u =
    if member u st
      then if sameUnique u st
             then (be, nm)
             else (addWarning nm be, T.showUnique u)
      else ((M.insert nm i st, ws), nm)
    where
    nm = T.unique_name u
    i  = T.unique_fresh u

  callbackImpl be sym f =
    let (be', cbSym) = uniqueImpl be sym in
    ( be'
    ,   AADLCallback
      $ I.incl
      $ I.voidProc cbSym
      $ \r -> I.body
      $ I.noReturn
      $ f r
    )

  emitterImpl be emitterAst _impl =
    ( be'
    , T.Emitter $ \ref -> I.call_ procFromEmitter ref
    , AADLEmitter
    )
    where
    (be', sym) = uniqueImpl be (AST.emitter_name emitterAst)
    procFromEmitter :: I.IvoryArea b
                    => I.Def('[I.ConstRef s b] I.:-> ())
    procFromEmitter = I.voidProc sym $ \_ref -> I.body I.retVoid

  handlerImpl be _ast _emitters callbacks =
    ( be
    , AADLHandler $
        case mconcat callbacks of
          AADLCallback defs -> defs
    )

  monitorImpl be ast handlers moddef =
    ( be'
    , AADLMonitor $
        (nm , mconcat (map handlerModules handlers) >> moddef)
    )
    where
    (be', nm) = uniqueImpl be (AST.monitor_name ast)
    handlerModules :: SomeHandler AADLBackend -> I.ModuleDef
    handlerModules (SomeHandler (AADLHandler h)) = h

  towerImpl be _ast ms =
    ( be
    , AADLOutput $ \deps -> [ mkMod m deps | AADLMonitor m <- ms ]
    )
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

--------------------------------------------------------------------------------
-- Helpers

-- This is a data structure for producing unique strings, given as input unique
-- (String, Integer) pairs. We want to ensure the following:
--
--     For a pair (s,i), does there exist an (s,j) in the store, where i /=
--     j? If not, we can just use s. If so, we must make a string representation
--     for (s,i) unique and write a warning that s is unstable.
--
-- We'll assume that for each (s,i), (s',j), if (s,i) == (s',j), then the same
-- value is denoted.

type Hash = Int
type UniqueState = M.Map String Integer
type Warnings = D.DList String

newtype UniqueM a = UniqueM
                -- Unique Names: map from string s an index i. If s->i is in the
                -- map, then the UniqueM (s,i) has name s.
  { unUnique :: (StateT AADLBackend Id) a
                -- -- Warnings of name shadowing
                --        (WriterT Warnings Id) a
  } deriving (Functor, Applicative, Monad)

instance StateM UniqueM AADLBackend where
  get = UniqueM get
  set = UniqueM . set

initUniqueSt :: AADLBackend
initUniqueSt = (M.empty, D.empty)

runUnique :: AADLBackend -> UniqueM a -> (a, (UniqueState, Warnings))
runUnique be m = runId $ runStateT be (unUnique m)

----------------------------------------
-- API

-- | Lookup the T.Unique (s,i) in the map. If it doesn't exist, insert it and
-- return s. If (s,i) exists in the map, return s. If (s,j), j /= i is in the
-- map, return a unique name.
uniqueImplM :: T.Unique -> UniqueM String
uniqueImplM u = do
  s <- get
  let (s', nm) = uniqueImpl s u
  set s'
  return nm

member :: T.Unique -> UniqueState -> Bool
member u st = M.member (T.unique_name u) st

sameUnique :: T.Unique -> UniqueState -> Bool
sameUnique u st =
  case M.lookup (T.unique_name u) st of
    Nothing -> False
    Just i  -> i == T.unique_fresh u

addWarning :: String -> AADLBackend -> AADLBackend
addWarning w (st, ws) = (st, D.snoc ws w)

showWarnings :: Warnings -> String
showWarnings ws =
    "*** Warning: the following strings are unstable: "
 ++ unwords (nub (D.toList ws))
