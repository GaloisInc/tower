{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Event
  ( Event
  , handle
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Event
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Task

handle :: forall p area
        . (IvoryArea area, IvoryZero area)
       => Event area
       -> String
       -> (forall s eff . ConstRef s area -> Ivory (AllocEffects eff) ())
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
      handler_proc = proc (showUnique procname) $ \r -> body $ noReturn $ k r
  putUsercode $ \_ _ -> do
    incl handler_proc
  -- Check for event and call handler from eventloop
  putEventLoopCode $ \_ _ -> do
    r <- local izero
    got <- evt_get evt r
    ifte_ got (call_ handler_proc (constRef r))
              (return ())
  where
  pfix = "evt_" ++ (AST.eventName (evt_ast evt)) ++ "_handler"


