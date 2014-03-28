{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Signal
  ( withSignalEvent
  , withUnsafeSignalEvent
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST            as AST
import           Ivory.Tower.Types.Event
import qualified Ivory.Tower.Types.OS       as OS
import           Ivory.Tower.Types.Signalable
import           Ivory.Tower.Types.SignalCode
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Task

withSignalEvent :: forall p
                 . (Signalable p)
                => SignalType p
                -> String
                -> Task p (Event (Stored IBool))
withSignalEvent sig annotation =
  withUnsafeSignalEvent sig annotation (return ())

withUnsafeSignalEvent :: forall p
                       . (Signalable p)
                      => SignalType p
                      -> String
                      -> (forall eff . Ivory eff ())
                      -> Task p (Event (Stored IBool))
withUnsafeSignalEvent sig annotation callback = do

  taskname <- getTaskName
  evtname <- freshname (basename taskname)

  -- Write signal receiver to AST:
  let sigrxer :: AST.SignalReceiver (SignalType p)
      sigrxer = AST.SignalReceiver
        { AST.signalreceiver_name       = evtname
        , AST.signalreceiver_annotation = annotation
        , AST.signalreceiver_signal     = sig
        }
  putSignalReceiver sigrxer

  -- Write channel even tto AST:
  let astevt :: AST.Event
      astevt = AST.SignalEvt (signalName sig)-- signame
  putASTEvent astevt

  -- Generate Receiver code:
  os <- getOS
  let named n = (showUnique evtname) ++ "_" ++ n
      ready_area = area (named "ready") Nothing
      sigcode :: SignalCode p
      sigcode = OS.gen_signal os sig callback
  putCommprim $ \sys -> do
    signalcode_moddef sigcode sys
    defMemArea ready_area
  putSysInitCode $ \sys -> do
    signalcode_init sigcode sys
    store (addrOf ready_area) false

  putEventReceiverCode $ \sys -> do
    success <- signalcode_receiver sigcode sys sigrxer
    store (addrOf ready_area) success

  return $ Event
    { evt_get = \ref -> do
        ready <- deref (addrOf ready_area)
        store ref true
        return ready
    , evt_ast = astevt
    }
  where
  basename tname = "signal_" ++ (signalName sig)
                ++ "_evt_" ++ (showUnique tname)

