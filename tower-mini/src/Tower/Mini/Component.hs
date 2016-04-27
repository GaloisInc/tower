{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Tower.Mini.Component where

import MonadLib hiding (local)
import Control.Monad.Fix

import Ivory.Language
import Ivory.Tower

newtype Component e a = Component {
    unComponent :: WriterT (ModuleDef, [Def('[] ':-> ())]) (Tower e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

instance BaseUtils Component e where
  freshname n = Component $ lift $ freshname n
  getEnv = Component $ lift $ getEnv

putComponentCode :: ModuleDef -> Component e ()
putComponentCode c = Component $ put (c, mempty)

putRunCode :: (forall s . Ivory (ProcEffects s ()) ()) -> Component e ()
putRunCode c = do
  n <- freshname "run"
  let fn = voidProc (showUnique n) $ body $ c *> retVoid
  Component $ put (incl fn, [fn])

liftTower :: Tower e a -> Component e a
liftTower c = Component $ lift c

inputPort :: forall e a .
             (IvoryArea a, IvoryZero a)
          => String
          -> String
          -> Component e (ChanOutput a)
inputPort sym hdr = do
  n <- freshname ("input_" ++ sym ++ takeWhile (/= '.') hdr)
  let ext_get_data :: Def('[Ref s a] ':-> IBool)
      ext_get_data = importProc sym hdr
      gen_mon_callback :: Def('[ConstRef s a] ':-> ())
      gen_mon_callback = importProc ("callback_" ++ showUnique n) ""
  putComponentCode $ do
    incl $ ext_get_data
    dependByName (showUnique n ++ "_monitor")
  putRunCode $ do
    ext_data <- local izero
    ext_has_data <- call ext_get_data ext_data
    ifte_ ext_has_data
      (call_ gen_mon_callback (constRef ext_data))
      (return ())
  liftTower $ do
    (_, ext_chan_out) <- channel
    (chan_in, chan_out) <- channel
    externalMonitor (showUnique n) $
      handler (ext_chan_out :: ChanOutput a) (showUnique n) $ do
        e <- emitter chan_in 1
        callback $ \msg -> emit e msg
    return chan_out

outputPort :: forall e a . (IvoryArea a, IvoryZero a) => String -> String -> Component e (ChanInput a)
outputPort sym hdr = do
  n <- freshname ("output_" ++ sym ++ takeWhile (/= '.') hdr)
  let ext_put_data :: Def('[ConstRef s a] ':-> ())
      ext_put_data = importProc sym hdr
  putComponentCode $ do
    incl $ ext_put_data
  liftTower $ do
    (chan_in, chan_out) <- channel
    externalMonitor (showUnique n) $
      handler (chan_out :: ChanOutput a) (showUnique n) $
        callback $ \msg -> call_ ext_put_data msg
    return chan_in

component :: String -> Component e () -> Tower e ()
component nm c = do
  (_, (modDefs, runFns)) <- runWriterT (unComponent c)
  let run :: Def('[] ':-> ())
      run = voidProc "run" $ body $ do
        forM_ runFns $ \f -> call_ f
        retVoid
      compMod :: Module
      compMod = package nm $ do
        modDefs
        incl run
  towerModule compMod
  towerDepends compMod
