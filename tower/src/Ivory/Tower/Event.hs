{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Event
  ( Event
  , taskEventHandler
  , handle
  , handleV
  ) where

import           Ivory.Language
import           Ivory.Tower.Types.Event
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Task
import           Ivory.Tower.Monad.Handler

taskEventHandler :: forall p area
        . (IvoryArea area, IvoryZero area)
       => Event area
       -> String
       -> Handler p area ()
       -> Task p ()
taskEventHandler evt annotation m = do
  tname <- getTaskName
  name <- freshname (pfix tname)
  (ast, codegenerator) <- runHandler m name annotation (evt_trigger evt)
  putASTHandler ast
  -- XXX DO SOMETHING WITH CODE GENERATOR
  where
  pfix tname = "handler_" ++ (showUnique tname) ++ "_" ++ eventDescription evt

-- Export public ivoryHandler:

handle :: (forall s s' . ConstRef s area -> Ivory (AllocEffects s') ())
      -> Handler p area ()
handle cb = putHandlerCallback cb

handleV :: (IvoryVar a, IvoryArea (Stored a))
        => (forall s . a -> Ivory (AllocEffects s) ())
        -> Handler p (Stored a) ()
handleV cb = putHandlerCallback (\ref -> deref ref >>= cb)


