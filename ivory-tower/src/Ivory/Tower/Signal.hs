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
    Just _ -> getNodeName >>= \name ->
              error ("multiple signalBody definitions in signal named " ++ name)
 where
 sigbody sch = ssch_mkSigBody sch (k sch)

