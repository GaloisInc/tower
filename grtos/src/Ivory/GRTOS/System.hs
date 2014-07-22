{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds #-}

module Ivory.GRTOS.System where

import Ivory.Language
import Ivory.GRTOS.AST
import Ivory.GRTOS.Kernel
import Ivory.GRTOS.PriorityGroup

data SystemScheduler =
  SystemScheduler
    { ss_module    :: Module
    , ss_launch    :: Def('[]:->())
    , ss_scheduler :: forall s . Def('[Ref s (Stored (Ptr Global (Struct "task_control_block")))]:->())
    , ss_tick      :: Def('[]:->())
    }

systemScheduler :: [(Priority, PriorityGroup)] -> SystemScheduler
systemScheduler ps = SystemScheduler
  { ss_module    = package "system" md
  , ss_launch    = launch_proc
  , ss_scheduler = scheduler_proc
  , ss_tick      = tick_proc
  }
  where
  named n = "system_" ++ n
  dummy_tcb_area = area (named "dummy_tcb") Nothing
  md = do
    depend kernel
    mapM_ (depend . pg_module . snd) ps
    defMemArea dummy_tcb_area

    incl launch_proc
    incl scheduler_proc
    incl tick_proc

  scheduler :: Ivory eff (Ref Global (Struct "task_control_block"))
  scheduler = return (addrOf dummy_tcb_area)

  launch_proc = proc (named "launch") $ body $ do
    return ()

  scheduler_proc :: Def('[Ref s (Stored (Ptr Global (Struct "task_control_block")))]:->())
  scheduler_proc = proc "scheduler_pick_next_task" $ \tcb -> body $ do
    res <- scheduler
    store tcb (refToPtr res)

  tick_proc = proc "scheduler_tick" $ body $ do
    return ()

