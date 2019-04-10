module Ivory.Tower.Types.ThreadCode
  ( ThreadCode(..)
  ) where

import Prelude ()
import Prelude.Compat

import Data.Semigroup

import Ivory.Language

data ThreadCode =
  ThreadCode
    { threadcode_user    :: ModuleDef
    , threadcode_gen     :: ModuleDef
    , threadcode_emitter :: ModuleDef
    }

instance Semigroup ThreadCode where
  -- ModuleDef order doesn't matter, but Tower used to concatenate ThreadCode
  -- in reverse order, so this does too. It could be swapped if desired.
  (<>) b a = ThreadCode
    { threadcode_user    = threadcode_user    a >> threadcode_user    b
    , threadcode_gen     = threadcode_gen     a >> threadcode_gen     b
    , threadcode_emitter = threadcode_emitter a >> threadcode_emitter b
    }

instance Monoid ThreadCode where
  mempty = ThreadCode
    { threadcode_user    = return ()
    , threadcode_gen     = return ()
    , threadcode_emitter = return ()
    }

