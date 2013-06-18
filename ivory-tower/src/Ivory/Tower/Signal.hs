{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Signal where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.Monad

-- | Declare a signal handler for a 'Signal'. The task body is an 'Ivory'
--   computation which handles the signal and Always terminates.
signalBody :: (SigSchedule -> (forall eff cs . (eff `AllocsIn` cs) => Ivory eff ()))
           -> Signal ()
signalBody k = do
  s <- getSignalSt
  case signalst_body s of
    Nothing -> setSignalSt $ s { signalst_body = Just sigbody }
    Just _ -> sigError  "multiple signalBody definitions"
 where
 sigbody sch = ssch_mkSigBody sch (k sch)

-- | set signal handler name. this will be the name in the generated C code.
signalName :: String -> Signal ()
signalName n = do
  s <- getSignalSt
  case signalst_cname s of
    Nothing -> setSignalSt $ s { signalst_cname = Just n }
    Just _ -> sigError "multiple signalName definitions"

sigError :: String -> Signal ()
sigError msg = getNodeName >>= \name -> error (msg ++ " in signal named " ++ name)

