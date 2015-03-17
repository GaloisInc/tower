{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

--
-- GHC plugin to generate srcloc info.
--
-- (c) 2014 Galois, Inc.
--

module Ivory.Tower.SrcLoc.Plugin (plugin) where

import           DynamicLoading
import           GhcPlugins

import GHC.Plugins.SrcSpan

#if __GLASGOW_HASKELL__ < 708
# error Ivory.Tower.Codegen.Plugin requires at least ghc-7.8
#endif

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals

  hsc_env <- getHscEnv

  Just withLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env hANDLER_MONAD_MODULE wITH_LOC
  withLocVar <- lookupId withLocName

  Just mkLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env hANDLER_MONAD_MODULE mK_LOC
  mkLocVar <- lookupId mkLocName

  Just handlerName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env hANDLER_MONAD_MODULE hANDLER
  handlerCon <- lookupTyCon handlerName

  let annotate loc expr = mkWithLocExpr handlerCon mkLocVar withLocVar loc expr
  let locpass = mkPass annotate killForeignStubs

  return $ (CoreDoPluginPass "Add Locations" locpass) : todos
  where
  killForeignStubs = "kill-foreign-stubs" `elem` opts


-- | Check that the expression is a handler monad type constructor.
isHandlerStmt :: TyCon -> CoreExpr -> Bool
isHandlerStmt handlerM expr@(App _ _)
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == handlerM
isHandlerStmt handlerM expr@(Var _)
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == handlerM
isHandlerStmt _ _
  = False

mkWithLocExpr :: TyCon -> Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkWithLocExpr handlerTyCon mkLocVar withLocVar (RealSrcSpan ss) expr
  | isHandlerStmt handlerTyCon expr = do
      loc <- mkLocExpr mkLocVar ss
      return $ mkCoreApps (Var withLocVar) (tys' ++ [loc, expr])
      where
      tys'     = map Type tys
      (_, tys) = splitAppTys $ exprType expr

mkWithLocExpr _ _ _ _ expr = return expr

mkLocExpr :: Var -> RealSrcSpan -> CoreM CoreExpr
mkLocExpr mkLocVar ss = do
  df   <- getDynFlags
  file <- mkStringExprFS $ srcSpanFile ss
  return $ mkCoreApps (Var mkLocVar) [ file
                                     , mkIntExprInt df (srcSpanStartLine ss)
                                     , mkIntExprInt df (srcSpanStartCol ss)
                                     , mkIntExprInt df (srcSpanEndLine ss)
                                     , mkIntExprInt df (srcSpanEndCol ss)
                                     ]

hANDLER_MONAD_MODULE :: ModuleName
hANDLER_MONAD_MODULE = mkModuleName "Ivory.Tower.Monad.Handler"

wITH_LOC, mK_LOC, hANDLER :: RdrName
wITH_LOC    = mkVarUnqual $ fsLit "withLocation"
mK_LOC      = mkVarUnqual $ fsLit "mkLocation"
hANDLER     = mkRdrQual hANDLER_MONAD_MODULE $ mkTcOcc "Handler"
