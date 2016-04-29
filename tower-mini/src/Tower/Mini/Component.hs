{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Tower.Mini.Component where

import MonadLib hiding (local)
import Control.Monad.Fix

import Ivory.Language
import Ivory.Tower

newtype ComponentM e a = ComponentM {
    unComponentM :: WriterT (ModuleDef, [Def('[] ':-> ())]) (Tower e) a
  } deriving (Functor, Monad, Applicative, MonadFix)

instance BaseUtils ComponentM e where
  freshname n = ComponentM $ lift $ freshname n
  getEnv = ComponentM $ lift $ getEnv

putComponentCode :: ModuleDef -> ComponentM e ()
putComponentCode c = ComponentM $ put (c, mempty)

putRunCode :: (forall s . Ivory (ProcEffects s ()) ()) -> ComponentM e ()
putRunCode c = do
  n <- freshname "component_entry_aux"
  let fn = voidProc (showUnique n) $ body $ c *> retVoid
  ComponentM $ put (incl fn, [fn])

liftTower :: Tower e a -> ComponentM e a
liftTower c = ComponentM $ lift c

-- | An channel implemented by an external C module.
data ExternalChan (a :: Area *) = ExternalChan {
    extChanGetSym :: String
    -- ^ Symbol @sym@ for a C function @bool sym(t *data)@ where
    -- @data@ is an output parameter and the return value indicates
    -- whether valid data has been written to @data@
  , extChanPutSym :: String
    -- ^ Symbol @sym@ for a C function @void sym(const t *data)@ where
    -- @data@ is an input parameter to write to the channel
  , extChanHeader :: String
    -- ^ Header file from which to import the channel's symbols
  } deriving (Show)

-- TODO: enforce that only one handler listens to the other end of this?

inputPort :: forall e a .
             (IvoryArea a, IvoryZero a)
          => String
          -> String
          -> ComponentM e (ChanOutput a)
inputPort sym hdr = do
  (chan_in, chan_out) <- liftTower channel
  inputPort' chan_in sym hdr
  return chan_out

inputPort' :: forall e a .
              (IvoryArea a, IvoryZero a)
           => ChanInput a
           -> String
           -> String
           -> ComponentM e ()
inputPort' chan_in sym hdr = do
  let n = "input_" ++ sym
  let ext_get_data :: Def('[Ref s a] ':-> IBool)
      ext_get_data = importProc sym hdr
      gen_mon_callback :: Def('[ConstRef s a] ':-> ())
      gen_mon_callback = importProc ("callback_" ++ n ++ "_handler") ""
  putComponentCode $ do
    incl $ ext_get_data
    dependByName (n ++ "_monitor")
  putRunCode $ do
    ext_data <- local izero
    ext_has_data <- call ext_get_data ext_data
    ifte_ ext_has_data
      (call_ gen_mon_callback (constRef ext_data))
      (return ())
  liftTower $ do
    (_, ext_chan_out) <- channel
    externalMonitor n $
      handler (ext_chan_out :: ChanOutput a) (n ++ "_handler") $ do
        e <- emitter chan_in 1
        callback $ \msg -> emit e msg

inputPortChan :: (IvoryArea a, IvoryZero a)
              => ExternalChan a
              -> ComponentM e (ChanOutput a)
inputPortChan ec =
  inputPort (extChanGetSym ec) (extChanHeader ec)

inputPortChan' :: (IvoryArea a, IvoryZero a)
               => ChanInput a
               -> ExternalChan a
               -> ComponentM e ()
inputPortChan' chan_in ec =
  inputPort' chan_in (extChanGetSym ec) (extChanHeader ec)

outputPort :: forall e a .
              (IvoryArea a, IvoryZero a)
           => String
           -> String
           -> ComponentM e (ChanInput a)
outputPort sym hdr = do
  (chan_in, chan_out) <- liftTower channel
  outputPort' chan_out sym hdr
  return chan_in

outputPort' :: forall e a .
               (IvoryArea a, IvoryZero a)
            => ChanOutput a
            -> String
            -> String
            -> ComponentM e ()
outputPort' chan_out sym hdr = do
  let n = "output_" ++ sym
  let ext_put_data :: Def('[ConstRef s a] ':-> ())
      ext_put_data = importProc sym hdr
  putComponentCode $ do
    incl $ ext_put_data
  liftTower $
    externalMonitor n $
      handler chan_out n $
        callback $ \msg -> call_ ext_put_data msg

outputPortChan :: (IvoryArea a, IvoryZero a)
               => ExternalChan a
               -> ComponentM e (ChanInput a)
outputPortChan ec =
  outputPort (extChanPutSym ec) (extChanHeader ec)

outputPortChan' :: (IvoryArea a, IvoryZero a)
                => ChanOutput a
                -> ExternalChan a
                -> ComponentM e ()
outputPortChan' chan_out ec =
  outputPort' chan_out (extChanPutSym ec) (extChanHeader ec)


data Component e = Component {
    componentName :: String
  , unComponent   :: ComponentM e ()
  }

component :: String -> ComponentM e () -> Component e
component nm c = Component nm c
