{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Signal where

import Text.Printf

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Monad
import Ivory.Tower.Node

instance Channelable SignalSt where
  nodeChannelEmitter  = signalChannelEmitter
  nodeChannelReceiver = signalChannelReceiver

signalChannelEmitter :: forall n area
                      . (SingI n, IvoryArea area)
                     => ChannelSource n area
                     -> Node SignalSt (ChannelEmitter n area)
signalChannelEmitter chsrc = do
  nodename <- getNodeName
  unique   <- freshname -- May not be needed.
  let chid = unChannelSource chsrc
      emitName = printf "emitFromSig_%s_chan%d%s" nodename (chan_id chid) unique
      externEmit :: Def ('[ConstRef s area] :-> IBool)
      externEmit = externProc emitName
      procEmit :: SigSchedule -> Def ('[ConstRef s area] :-> IBool)
      procEmit schedule = proc emitName $ \ref -> body $ do
        r <- ssch_mkEmitter schedule emitter ref
        ret r
      emitter = ChannelEmitter
        { ce_chid         = chid
        , ce_extern_emit  = call  externEmit
        , ce_extern_emit_ = call_ externEmit
        }
  signalModuleDef $ \sch -> do
    incl (procEmit sch)
  return emitter

signalChannelReceiver :: forall n area
                       . (SingI n, IvoryArea area, IvoryZero area)
                      => ChannelSink n area
                      -> Node SignalSt (ChannelReceiver n area)
signalChannelReceiver chsnk = do
  nodename <- getNodeName
  unique   <- freshname -- May not be needed.
  let chid = unChannelSink chsnk
      rxName = printf "receiveFromSig_%s_chan%d%s" nodename (chan_id chid) unique
      externRx :: Def ('[Ref s area] :-> IBool)
      externRx = externProc rxName
      procRx :: SigSchedule -> Def ('[Ref s area] :-> IBool)
      procRx schedule = proc rxName $ \ref -> body $ do
        r <- ssch_mkReceiver schedule rxer ref
        ret r
      rxer = ChannelReceiver
        { cr_chid      = chid
        , cr_extern_rx = call externRx
        }
  signalModuleDef $ \sch -> do
    incl (procRx sch)
  return rxer

-- | Track Ivory dependencies used by the 'Ivory.Tower.Tower.signalBody' created
--   in the 'Ivory.Tower.Types.Signal' context.
signalModuleDef :: (SigSchedule -> ModuleDef) ->  Signal ()
signalModuleDef = sigStAddModuleDef

-- | Declare a signal handler for a 'Signal'. The task body is an 'Ivory'
--   computation which handles the signal and Always terminates.
signalBody :: (SigSchedule -> (forall eff cs . (GetAlloc eff ~ Scope cs)
           => Ivory eff ()))
           -> Signal ()
signalBody k = do
  s <- getSignalSt
  case signalst_body s of
    Nothing -> setSignalSt $ s { signalst_body = Just undefined } -- XXX fixup later
    Just _ -> sigError  "multiple signalBody definitions"

-- | set signal handler name. this will be the name in the generated C code.
signalName :: String -> Signal ()
signalName n = do
  s <- getSignalSt
  case signalst_cname s of
    Nothing -> setSignalSt $ s { signalst_cname = Just n }
    Just _ -> sigError "multiple signalName definitions"

sigError :: String -> Signal ()
sigError msg = getNodeName >>= \name -> error (msg ++ " in signal named " ++ name)

signalLocal :: (IvoryArea area) => Name -> Signal (Ref Global area)
signalLocal n = siglocalAux n Nothing

signalLocalInit :: (IvoryArea area) => Name -> Init area -> Signal (Ref Global area)
signalLocalInit n i = siglocalAux n (Just i)

siglocalAux :: (IvoryArea area) => Name -> Maybe (Init area) -> Signal (Ref Global area)
siglocalAux n i = do
  f <- freshname
  let m = area (n ++ f) i
  sigStAddModuleDef (const (defMemArea m))
  return (addrOf m)

