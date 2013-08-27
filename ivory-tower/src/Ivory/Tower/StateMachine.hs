{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Ivory.Tower.StateMachine where

import Ivory.Language
import Ivory.Tower.Types
import Ivory.Tower.StateMachine.Types

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

-- nextState :: StateLabel -> StmtM s ()
-- nextState s = writeStmt $ Stmt (return [(true, s)])

branch :: (CFlowable a) => IBool -> StateLabel -> a
branch b lbl = cflow $ CFlowBranch b lbl

goto :: (CFlowable a) => StateLabel -> a
goto lbl = branch true lbl

haltWhen :: (CFlowable a) => IBool -> a
haltWhen p = cflow $ CFlowHalt p

halt :: (CFlowable a) => a
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


