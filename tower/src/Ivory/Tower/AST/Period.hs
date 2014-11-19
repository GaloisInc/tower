module Ivory.Tower.AST.Period where

import qualified Ivory.Language.Syntax.Type as I

import Ivory.Tower.Types.Time

data Period = Period
  { period_dt :: Microseconds
  , period_ty :: I.Type
  } deriving (Eq, Show, Ord)

