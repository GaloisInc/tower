{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Tower.AST.Emitter where

import Data.Monoid ((<>))
#if MIN_VERSION_mainland_pretty(0,6,0)
import           Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

import Ivory.Tower.AST.SyncChan
import Ivory.Tower.Types.Unique

data Emitter = Emitter
  { emitter_name :: Unique
  , emitter_chan :: SyncChan
  , emitter_bound :: Integer
  } deriving (Eq, Show, Ord)

emitterName :: Emitter -> String
emitterName = showUnique . emitter_name

emitter :: Unique -> SyncChan -> Integer -> Emitter
emitter i c b = Emitter
  { emitter_name  = i
  , emitter_chan  = c
  , emitter_bound = b
  }

instance Pretty Emitter where
  ppr e@Emitter{..} =
         text (emitterName e)
     <+> parens ("bound=" <> integer emitter_bound) <> colon
     <+> ppr emitter_chan
