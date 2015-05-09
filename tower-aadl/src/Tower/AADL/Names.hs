--
-- String munging common across modules.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Names
  ( periodicEmitter
  , periodicCallback
  , prettyTime
  ) where

import qualified Ivory.Tower.Types.Time as T
import           Ivory.Tower.AST.Period as P


periodicEmitter :: P.Period -> String
periodicEmitter p = "emitter_" ++ prettyTime p

periodicCallback :: P.Period -> String
periodicCallback p = "callback_" ++ prettyTime p

prettyTime :: P.Period -> String
prettyTime p = T.prettyTime (P.period_dt p)
