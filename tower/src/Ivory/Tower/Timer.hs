{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Timer
  ( getTime
  , withPeriodicEvent
  ) where

import           Ivory.Language
import           Ivory.Stdlib
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Time
import           Ivory.Tower.Types.Event
import           Ivory.Tower.Types.Unique
import           Ivory.Tower.Monad.Base
import           Ivory.Tower.Monad.Task

getTime :: Ivory eff ITime
getTime = call p
  where
  p :: Def('[] :-> ITime)
  p = externProc "getTimeMicros"

withPeriodicEvent :: Time a => a -> Task p (Event (Stored ITime))
withPeriodicEvent period = do
  tid <- fresh
  tname <- getTaskName
  let named n = (showUnique tname) ++ "_timer_event_" ++ n ++ "_" ++ show tid
  -- Write timer to AST:
  let astevt = AST.TimerEvt $ AST.Timer
        { AST.timer_per = toMicroseconds period
        }
  putASTEvent astevt

  -- Generate timer tick code:
  let per :: ITime
      per = fromIMicroseconds ((fromInteger (toMicroseconds period)) :: Sint64)
      lastPeriodArea = area (named "lastPeriod") Nothing
      lastPeriodStart = addrOf lastPeriodArea
      dueArea = area (named "due") (Just (ival false))
      dueRef = addrOf dueArea
      dueTimeArea = area (named "dueTime") Nothing
      dueTimeRef = addrOf dueTimeArea

      initDef :: Def('[]:->())
      initDef = proc (named "init") $ body $ do
        getTime >>= store lastPeriodStart
      tickDef :: Def('[Ref s (Stored ITime)] :-> ())
      tickDef = proc (named "tick") $
        \tasksNextDue -> body $ do
          now <- getTime
          lp  <- deref lastPeriodStart
          assume (now >=? lp) -- 64-bit us clock better be monotonic
          -- stert time of current period. (now / per) will round down.
          thisPeriodStart <- assign ((now `iDiv` per) * per)
          ticked <- assign (thisPeriodStart >? lp)
          store dueRef ticked
          when ticked $ do
            store dueTimeRef thisPeriodStart
            store lastPeriodStart thisPeriodStart
          -- Update tasks's next deadline if this one is sooner
          due <- assign (thisPeriodStart + per)
          tnd <- deref tasksNextDue
          when (due <? tnd) (store tasksNextDue due)
  putCommprim $ \_ -> do
    public $ do
      defMemArea dueArea
      defMemArea dueTimeArea
      incl initDef
    private $ do
      incl tickDef
      defMemArea lastPeriodArea
  putSysInitCode $ \_ ->
    call_ initDef
  putTimerCode $ call_ tickDef

  -- Return event check code:
  return $ Event
    { evt_get = \duetime -> do
        due <- deref dueRef
        t <- deref dueTimeRef
        when due $ store duetime t
        return due
    , evt_ast = astevt
    }

