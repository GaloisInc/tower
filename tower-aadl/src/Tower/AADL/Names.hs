--
-- String munging common across modules.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Names
  ( periodicEmitter
  , periodicCallback
  , prettyTime
  , threadFile
  , threadEmitterHeader
  , smaccmPrefix
  ) where

import qualified Ivory.Tower.AST        as A
import qualified Ivory.Tower.Types.Time as T
import           Ivory.Tower.AST.Period as P

-- add aadl2rtos prefix
smaccmPrefix :: String -> String
smaccmPrefix = ("smaccm_" ++)

threadEmitterHeader :: A.Thread -> String
threadEmitterHeader t =
  smaccmPrefix $ A.threadName t ++ ".h"

periodicEmitter :: P.Period -> String
periodicEmitter p = "emitter_" ++ prettyTime p

periodicCallback :: P.Period -> String
periodicCallback p = "callback_" ++ prettyTime p

prettyTime :: P.Period -> String
prettyTime p = T.prettyTime (P.period_dt p)

threadFile :: A.Monitor -> String
threadFile m = A.monitorName m ++ "_monitor"
