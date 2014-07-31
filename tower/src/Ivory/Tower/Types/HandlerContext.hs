{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.HandlerContext
  ( HandlerContext(..)
  ) where

import Ivory.Language
import Ivory.Tower.Types.Unique

data HandlerContext =
  HandlerContext
    { handlercontext_name     :: Unique
    }
-- XXX
