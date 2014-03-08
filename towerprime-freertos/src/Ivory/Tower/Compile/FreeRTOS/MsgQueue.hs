{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Compile.FreeRTOS.MsgQueue
  ( MsgQueue(..)
  , msgQueue
  ) where

import           Ivory.Language
import qualified Ivory.Tower.AST as AST
import           Ivory.Tower.Types.Unique

import qualified Ivory.OS.FreeRTOS.Semaphore as S

data MsgQueue area =
  MsgQueue
    { mq_push :: forall eff s . ConstRef s area -> Ivory eff ()
    , mq_pop  :: forall eff s . AST.Task -> Ref s area -> Ivory eff IBool
    , mq_init :: Def('[]:->())
    , mq_code :: ModuleDef
    }

msgQueue :: forall area . (IvoryArea area) => AST.System -> AST.Chan -> MsgQueue area
msgQueue sysast chanast = MsgQueue
  { mq_push = call_ push
  , mq_pop  = \rxertask -> call (pop rxertask)
  , mq_init = init
  , mq_code = code
  }
  where

  push :: Def('[ConstRef s area]:->())
  push = proc (named "push") $ \r -> body $ do
    return ()

  pop :: AST.Task -> Def('[Ref s area]:->IBool)
  pop t = proc (named (showUnique (AST.task_name t) ++ "_pop")) $ \r -> body $ do
    ret false

  init = proc (named "init") $ body $ do
    return ()

  poppingtasks = [] -- XXX

  code = do
    incl push
    mapM_ (\t -> incl (pop t)) poppingtasks
    incl init

  named n = "chan_" ++ (show (AST.chan_id chanast)) ++ "_" ++ n

