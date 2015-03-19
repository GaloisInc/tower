{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Codegen.Emitter where

import Control.Monad (forM_)

import qualified Ivory.Tower.AST as AST

import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.EmitterCode

import Ivory.Language
import Ivory.Stdlib (when)

emitterCode :: (IvoryArea a, IvoryZero a)
            => AST.Emitter
            -> (forall s. AST.Monitor -> AST.Tower -> AST.Thread -> [Def ('[ConstRef s a] :-> ())])
            -> (Emitter a, AST.Monitor -> AST.Tower -> AST.Thread -> EmitterCode a)
emitterCode ast sinks =
  ( Emitter $ call_ $ trampolineProc ast $ const $ return ()
  , emitterCodePerThread ast sinks
  )

emitterCodePerThread :: forall a
                      . (IvoryArea a, IvoryZero a)
                     => AST.Emitter
                     -> (forall s. AST.Monitor -> AST.Tower -> AST.Thread -> [Def ('[ConstRef s a] :-> ())])
                     -> AST.Monitor -> AST.Tower -> AST.Thread -> EmitterCode a
emitterCodePerThread ast sinks mon twr thr = EmitterCode
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
  trampoline = trampolineProc ast $ call_ eproc
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
                  forM_ (sinks mon twr thr) $ \ p ->
                    call_ p (constRef (addrOf m))

  e_per_thread = emitterThreadProcName thr ast

trampolineProc :: IvoryArea a
               => AST.Emitter
               -> (forall eff. ConstRef s a -> Ivory eff ())
               -> Def ('[ConstRef s a] :-> ())
trampolineProc ast f = proc (AST.emitterProcName ast) $ \ r -> body $ f r

emitterThreadProcName :: AST.Thread -> AST.Emitter -> EmitState -> String
emitterThreadProcName thr ast suffix =
  AST.emitterProcName ast ++ "_" ++ tn ++ "_" ++ prettyEmitSuffix suffix
  where
  tn = AST.threadName thr
