{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Codegen.Emitter where

import Control.Monad (forM_)

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.EmitterCode
import Ivory.Tower.Codegen.Handler

import Ivory.Language
import Ivory.Stdlib (when)

emitterCode :: forall a
             . (IvoryArea a, IvoryZero a)
            => Emitter a -> AST.Tower -> AST.Thread -> EmitterCode a
emitterCode e@(Emitter ast) twr thr = EmitterCode
  { emittercode_init = call_ iproc
  , emittercode_deliver = call_ dproc
  , emittercode_user = do
      private $ incl trampoline
  , emittercode_gen = do
      mapM_ defMemArea messages
      defMemArea messageCount
      incl iproc
      incl eproc
      incl dproc
  }
  where
  max_messages = AST.emitter_bound ast - 1
  messageCount :: MemArea (Stored Uint32)
  messageCount = area (e_per_thread MsgCnt) Nothing
  messages :: [MemArea a]
  messages = [ area (e_per_thread (Msg d)) Nothing
             | d <- [0..max_messages] ]

  messageAt :: Uint32 -> Ref Global a
  messageAt idx = foldl aux dflt (zip messages [0..])
    where
    dflt = addrOf (messages !! 0) -- Should be impossible.
    aux basecase (msg, midx) =
      (fromIntegral (midx :: Integer) ==? idx) ? (addrOf msg, basecase)

  trampoline :: Def('[ConstRef s a]:->())
  trampoline = proc ename $ \msg -> body $ call_ eproc msg
  iproc :: Def('[]:->())
  iproc = proc (e_per_thread Init) $ body $
               store (addrOf messageCount) 0
  eproc :: Def('[ConstRef s a]:->())
  eproc = proc (e_per_thread Emit)  $ \msg -> body $ do
               mc <- deref (addrOf messageCount)
               when (mc <=? fromIntegral max_messages) $ do
                 store (addrOf messageCount) (mc + 1)
                 storedmsg <- assign (messageAt mc)
                 refCopy storedmsg msg

  dproc :: Def('[]:->())
  dproc = proc (e_per_thread Deliver) $ body $ do
            mc <- deref (addrOf messageCount)
            forM_ (zip messages [0..]) $ \(m, (index :: Integer)) ->
               when (fromIntegral index <? mc) $
                  forM_ (AST.towerChanHandlers twr chanast) $ \(_,h) ->
                    call_ (handlerproc_stub h) (constRef (addrOf m))

  handlerproc_stub :: AST.Handler -> Def('[ConstRef s a]:->())
  handlerproc_stub h = proc (handlerProcName h thr) $ \_msg -> body $
    return ()

  chanast = case e of Emitter (AST.Emitter _ chast _) -> chast
  ename = emitterProcName e
  e_per_thread = emitterThreadProcName thr e

emitterThreadProcName :: AST.Thread -> Emitter a -> EmitState -> String
emitterThreadProcName thr e suffix =
  emitterProcName e ++ "_" ++ tn ++ "_" ++ prettyEmitSuffix suffix
  where
  tn = AST.threadName thr
