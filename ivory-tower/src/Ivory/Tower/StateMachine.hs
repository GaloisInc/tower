{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Ivory.Tower.StateMachine
  ( onEvent
  , onTimeout

  , Runnable
  , active
  , begin
  , stateMachine

  , MachineM
  , Stmt

  , state
  , branch
  , goto
  , haltWhen
  , halt
  , liftIvory
  , liftIvory_
  ) where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.StateMachine.Types
import Ivory.Tower.StateMachine.Compile

onEvent :: forall area
         . (IvoryArea area, IvoryZero area)
        => Event area
        -> (forall s s' . ConstRef s' area -> StmtM s ())
        -> StateM ()
onEvent e stmtM = writeHandler $ EventHandler e (ScopedStatements stmts)
  where
  stmts :: ConstRef s' area -> [Stmt s]
  stmts ref = runStmtM (stmtM ref)

onTimeout :: Int -> (forall s . StmtM s ()) -> StateM ()
onTimeout t stmtM = writeHandler $ TimeoutHandler t (ScopedStatements (const (runStmtM stmtM)))

state :: StateM () -> Machine
state sh = do
  l <- label
  writeState $ runStateM sh l
  return l

branch :: (CFlowable m) => IBool -> StateLabel -> m ()
branch b lbl = cflow $ CFlowBranch b lbl

goto :: (CFlowable m) => StateLabel -> m ()
goto lbl = branch true lbl

haltWhen :: (CFlowable m) => IBool -> m ()
haltWhen p = cflow $ CFlowHalt p

halt :: (CFlowable m) => m ()
halt = haltWhen true

liftIvory_ :: Ivory (AllocEffects s) () -> StmtM s ()
liftIvory_ i = writeStmt $ Stmt (i >> return (return ()))

liftIvory :: Ivory (AllocEffects s) CFlow-> StmtM s ()
liftIvory i = writeStmt $ Stmt i

--------------------------------------------------------------------------------

test :: Machine
test = mdo
  a <- state $ do
        onTimeout 20 $ do
          liftIvory_ $ return ()
          goto b
        -- onEvent "SomeEventA" (nextState b)
  b <- state $ do
        onTimeout 10 (goto a)
        -- onEvent "SomeEventA" (nextState b)
  return a


