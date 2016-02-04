--
-- String munging common across modules.
--
-- (c) 2015 Galois, Inc.
--

module Tower.AADL.Names
  ( periodicEmitter
  , periodicCallback
  , signalEmitter
  , signalCallback
  , initEmitter
  , initCallback
  , prettyTime
  , threadFile
  , threadEmitterHeader
  , smaccmPrefix
  ) where

import qualified Ivory.Tower.AST        as A
import qualified Ivory.Tower.Types.Time as T
import qualified Ivory.Tower.AST.Period as P
import qualified Ivory.Tower.AST.Signal as S

-- add aadl2rtos prefix
smaccmPrefix :: String -> String
smaccmPrefix = ("smaccm_" ++)

threadEmitterHeader :: A.Thread -> String
threadEmitterHeader t =
  smaccmPrefix $ A.threadName t ++ ".h"

------------------------------------------------------------

periodicEmitter :: P.Period -> String
periodicEmitter p = "emitter_" ++ prettyTime p

periodicCallback :: P.Period -> String
periodicCallback p = "callback_" ++ prettyTime p

------------------------------------------------------------

initEmitter :: String -> String
initEmitter = ("emitter_" ++)

initCallback :: String -> String
initCallback = ("callback_" ++)

------------------------------------------------------------

signalEmitter :: S.Signal -> String
signalEmitter s = "emitter_" ++ S.signal_name s

signalCallback :: S.Signal -> String
signalCallback s = "callback_" ++ S.signal_name s

------------------------------------------------------------

prettyTime :: P.Period -> String
prettyTime p = T.prettyTime (P.period_dt p)

threadFile :: A.Monitor -> String
threadFile m = A.monitorName m ++ "_monitor"
