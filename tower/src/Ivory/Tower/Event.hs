{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Event
  ( Event
  , handle
  , handleV
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Event
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Task

handleV :: forall p a
        . (IvoryVar a, IvoryArea (Stored a), IvoryZero (Stored a))
       => Event (Stored a)
       -> String
       -> (forall eff . a -> Ivory (ProcEffects eff ()) ())
       -> Task p ()
handleV evt annotation k = handle evt annotation $ \ref -> do
  v <- deref ref
  k v

handle :: forall p area
        . (IvoryArea area, IvoryZero area)
       => Event area
       -> String
       -> (forall s eff . ConstRef s area -> Ivory (ProcEffects eff ()) ())
       -> Task p ()
handle evt annotation k = do
  procname <- freshname pfix
  -- Write EventHandler into AST
  let ast = AST.EventHandler
        { AST.evthandler_name = procname
        , AST.evthandler_annotation = annotation
        , AST.evthandler_evt = evt_ast evt
        }
  putASTEventHandler ast

  -- Package handler into a procedure in usercode
  let handler_proc :: Def('[ConstRef s area]:->())
      handler_proc = proc (showUnique procname) $ \r -> body $ k r
  putUsercode $  do
    incl handler_proc
  -- Check for event and call handler from eventloop
  putEventLoopCode $ \_ -> do
    r <- local izero
    got <- evt_get evt r
    ifte_ got (call_ handler_proc (constRef r))
              (return ())
  where
  pfix = "evt_" ++ (AST.eventName (evt_ast evt)) ++ "_handler"


