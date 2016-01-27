{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.HAL.Bus.CAN.Sched
  ( CANTask()
  , canTask
  , canScheduler
  ) where

import Control.Monad (forM, forM_)
import Data.Bits (shiftL)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

[ivory|
struct can_transmit_result
  { task_idx :: Stored Uint8
  ; task_success :: Stored IBool
  }
|]

-- | One step in inserting a new task into a priority-sorted array. If
-- the new task should go before the existing task, then exchange the
-- two; afterward, the existing task becomes a "new" task to insert
-- before the next element in the array.
shiftUp :: Def ('[ Ref s0 ('Stored Uint8)
                 , Ref s1 ('Stored Uint32), Ref s2 ('Stored Uint8)
                 , Ref s3 ('Stored Uint32), Ref s4 ('Stored Uint8)
                 ] ':-> IBool)
shiftUp = proc "shift_task_up" $ \ insert_position new_prio new_task current_prio current_task -> body $ do
  new <- deref new_prio
  when (new ==? maxBound) $ ret true
  current <- deref current_prio
  assert (new /=? current)
  ifte_ (new >? current) (insert_position %= (+ 1)) $ do
    temp_task <- deref current_task
    refCopy current_prio new_prio
    refCopy current_task new_task
    store new_prio current
    store new_task temp_task
  ret false

-- | One step in removing an old task from a sorted array. When we reach
-- the target element of the array, pull the next element down to this
-- one, overwriting it; afterward, the new deletion target is the
-- now-duplicated task that we just pulled forward.
shiftDown :: Def ('[ Ref s0 ('Stored Uint8)
                   , Ref s1 ('Stored Uint32), Ref s2 ('Stored Uint8)
                   , ConstRef s3 ('Stored Uint32), ConstRef s4 ('Stored Uint8)
                   ] ':-> IBool)
shiftDown = proc "shift_task_down" $ \ target_ref current_prio current_task next_prio next_task -> body $ do
  target <- deref target_ref
  when (target ==? maxBound) $ ret true
  current <- deref current_task
  when (current ==? target) $ do
    refCopy current_prio next_prio
    refCopy current_task next_task
    comment "We just duplicated the next task, so arrange to delete that next."
    refCopy target_ref next_task
  ret false

-- | These definitions do not depend on the number of mailboxes or
-- tasks, so factor them into a separate module to reduce code
-- duplication.
schedulerHelperModule :: Module
schedulerHelperModule = package "can_scheduler_helper" $ do
  defStruct (Proxy :: Proxy "can_transmit_result")
  incl shiftUp
  incl shiftDown

-- | Pass this data to 'canScheduler' to tie the corresponding client to
-- a given collection of multiplexed hardware mailboxes.
data CANTask = CANTask
  { canTaskReq :: ChanOutput ('Struct "can_message")
  , canTaskRes :: ChanInput ('Stored IBool)
  , canTaskAbortReq :: ChanOutput ('Stored IBool)
  }

-- | Construct a virtual CAN transmit mailbox that a client can use as
-- if it had sole ownership of the bus. The returned 'CANTask' must be
-- passed to an instance of 'canScheduler'; otherwise, the virtual
-- mailbox will discard requests sent to it and will never complete
-- them.
canTask :: Tower e (CANTask, AbortableTransmit ('Struct "can_message") ('Stored IBool))
canTask = do
  (abortableTransmit, canTaskReq) <- channel
  (canTaskRes, abortableComplete) <- channel
  (abortableAbort, canTaskAbortReq) <- channel
  return (CANTask { .. }, AbortableTransmit { .. })

-- | Multiplex a collection of CAN transmit tasks onto a collection of
-- hardware transmit mailboxes. The transmit mailboxes must ensure that
-- the highest-priority message queued on any of them is sent first.
canScheduler :: [AbortableTransmit ('Struct "can_message") ('Stored IBool)]
             -> [CANTask]
             -> Tower e ()
canScheduler mailboxes tasks = do
  (doTaskComplete, taskCompleteChan) <- channel
  (doTaskAbort, taskAbortChan) <- channel

  towerModule schedulerHelperModule
  towerDepends schedulerHelperModule

  monitor "can_scheduler" $ do
    -- Maintain a priority queue of tasks. The priority is stored in
    -- state variables in prio_queue, and the corresponding task index
    -- in task_queue. We use linear-time insertion in sorted order to
    -- maintain the priority queue invariants, rather than a more
    -- complex data structure like a heap.
    --
    -- The state variables are allocated in separate groups to minimize
    -- padding waste, since task-ID is 8-bit and priority is 32-bit.
    --
    -- The highest-priority tasks are those which *should* be in the
    -- hardware mailboxes right now. However, since we have to wait for
    -- completion notifications after hardware aborts, the current
    -- contents of each mailbox can lag behind the current set of
    -- highest priority tasks.
    --
    -- The sentinels are global only because we place references to them
    -- in a list that also contains references to the queues themselves,
    -- and Ivory's embedding in Haskell's type system requires the list
    -- elements to all have the same scope.

    prio_queue <- forM (zipWith const [(0 :: Int) ..] tasks) $ \ idx -> do
      stateInit ("prio_" ++ show idx) $ ival maxBound

    sentinel_prio <- fmap constRef $ stateInit "prio_sentinel" $ ival maxBound

    task_queue <- forM (zipWith const [(0 :: Int) ..] tasks) $ \ idx -> do
      stateInit ("task_" ++ show idx) $ ival maxBound

    sentinel_task <- fmap constRef $ stateInit "task_sentinel" $ ival maxBound

    -- For each mailbox, we need to track which task is actually in the
    -- mailbox right now. That's the task to notify when that mailbox
    -- reports completion. One extra value (maxBound) is reserved to
    -- indicate that the mailbox is currently empty.
    mbox_states <- forM (zip [0..] mailboxes) $ \ (idx, mbox) -> do
      current <- stateInit ("current_task_in_" ++ show idx) $ ival maxBound
      return (idx, mbox, current)

    -- If we queue up two aborts to the same mailbox, then there could
    -- be an interleaving of threads with this sequence:
    -- 1. Mailbox processes abort #1.
    -- 2. Hardware reports completion.
    -- 3. We queue the next request on this mailbox.
    -- 4. Mailbox processes abort #2, aborting the wrong request.
    -- We don't want to spuriously abort requests, so keep a flag per
    -- mailbox to record whether we already have a pending abort there.
    mbox_aborting <- stateInit "mbox_aborting" (izero :: Init ('Stored Uint8))

    task_states <- forM (zip [0..] tasks) $ \ (idx, task) -> do
      -- We buffer one request from each task. They aren't allowed to
      -- send another until we send them a completion notification,
      -- although they can trigger that early by sending us an abort
      -- request. A sentinel message ID (maxBound) indicates that there
      -- is no pending request from this task.
      last_request <- stateInit ("last_request_for_" ++ show idx) $ istruct
        [ can_message_id .= ival (fromRep maxBound) ]
      return (idx, task, last_request)

    -- Global properties

    -- - If a task ID is in some 'current' variable in 'mbox_states',
    --   then we're waiting for it to complete in some hardware mailbox.
    --
    --   - If it's in the high-priority elements of task_queue, then the
    --     task is in the "on-hardware" state.
    --   - If it's in the low-priority elements of task_queue, then the
    --     task is in the "reschedule" state, waiting to be moved out of
    --     the mailbox to make room for an incoming higher-priority task.
    --   - If it's missing from task_queue, then the task is in the
    --     "abort" state, waiting for the hardware to notify us of its
    --     final disposition.
    --
    -- - If a task ID is not in a mailbox:
    --
    --   - If it's in task_queue somewhere, then it's in the "schedule"
    --     state, waiting for the current mailbox contents to get sent
    --     so that there's an empty mailbox to put this task into.
    --   - Otherwise the task is idle. Its "last_request" variable must
    --     have a can_message_id of 'maxBound'.
    --
    -- See sched.dot in this directory for a state machine graph
    -- describing the legal transitions between the above five states.

    -- Procedures for manipulating the priority queue:

    -- Return whether the given task is currently in the task queue.
    -- Used to determine whether a task which completed unsuccessfully
    -- (which can only happen if we abort its mailbox) is in
    -- "reschedule" or "abort" state.
    let isTaskQueued = proc "is_task_queued" $ \ task -> body $ do
          forM_ task_queue $ \ current_task -> do
            current <- deref current_task
            when (current ==? maxBound) $ ret false
            when (current ==? task) $ ret true
          ret false

    -- Return whether the given task is currently in a hardware mailbox.
    -- Used in nextTask to determine whether a high-priority task is in
    -- "on-hardware" or "schedule" state.
    let isTaskCurrent = proc "is_task_current" $ \ task -> body $ do
          forM_ mbox_states $ \ (_, _, current) -> do
            current_task <- deref current
            when (task ==? current_task) $ ret true
          ret false

    -- Return a reference to the given task's current request. The task
    -- ID must be a valid index in task_states.
    let getTaskRequest = proc "get_task_request" $ \ task -> body $ do
          let ((last_idx, _, last_task) : ts) = reverse task_states
          forM_ (reverse ts) $ \ (idx, _, last_request) -> do
            when (task ==? fromInteger idx) $ ret (constRef last_request)
          assert (task ==? fromInteger last_idx)
          ret (constRef last_task)

    -- Select the highest-priority task which is not already in a
    -- hardware mailbox, or return 'maxBound' if there is no such task.
    -- Used when a mailbox reports completion so we're ready to place a
    -- new request in it.
    --
    -- It doesn't make sense to choose a task which would be in the
    -- "reschedule" state immediately, so we only look at the tasks
    -- which are in the high-priority portion of the queue. Taking one
    -- of those tasks from not-current to current implies a transition
    -- from "schedule" to "on-hardware" state.
    let nextTask = proc "next_task" $ body $ do
          forM_ (zipWith const task_queue mbox_states) $ \ task -> do
            target_task <- deref task
            comment "Stop at the end of the list."
            when (target_task ==? maxBound) $ ret maxBound
            comment "Skip tasks that are already on the hardware."
            is_current <- call isTaskCurrent target_task
            unless is_current $ ret target_task
          ret maxBound

    -- Add a task to the task_queue, possibly returning a reschedule
    -- request for some lower-priority task which should be bounced out
    -- of a hardware mailbox, or a schedule request to place this task
    -- into a currently-empty mailbox.
    let insertTask :: Def ('[ Uint8
                            , Ref s0 ('Stored Uint8)
                            , Ref s1 ('Stored Uint8)
                            , Ref s2 ('Struct "can_message")
                            , ConstRef s3 ('Struct "can_message")
                            ] ':-> IBool)
        insertTask = proc "insert_task" $ \ task resched_task resched_mbox last_request req -> body $ do
          comment "Task must not have an outstanding request already."
          last_id <- deref $ last_request ~> can_message_id
          assert (toRep last_id ==? maxBound)

          comment "Save this request until we can deliver it."
          refCopy last_request req

          insert_position_ref <- local (izero :: Init ('Stored Uint8))

          new_prio <- local =<< do
            req_id <- deref $ req ~> can_message_id
            return $ ival $ toRep req_id
          new_task <- local $ ival task

          let checkPlace (current_prio, current_task) next = do
                done <- call shiftUp insert_position_ref new_prio new_task current_prio current_task
                unless done next
          foldr checkPlace (return ()) $ zip prio_queue task_queue

          comment "Check if we overflowed the queue and still have a task left to insert."
          final_task <- deref new_task
          assert (final_task ==? maxBound)

          insert_position <- deref insert_position_ref

          let positions = fromIntegral (length tasks)
          let mbox_count = fromIntegral (length mailboxes)
          assert (insert_position <? positions)

          when (positions <=? mbox_count .|| insert_position <? mbox_count) $ do
            comment "Priority is high enough to get a mailbox immediately."

            bounce_task <- case drop (length mailboxes) task_queue of
              [] -> do
                comment "No more tasks than mailboxes, so there must be a free mailbox."
                return maxBound
              bounce_task : _ -> do
                comment "Reschedule the task that we just shoved out of the high-priority group."
                deref bounce_task

            when (bounce_task ==? maxBound) $ do
              comment "Put this new task in a free mailbox."
              store resched_task task
            conds <- forM mbox_states $ \ (mbox_idx, _, current) -> do
              pending_task <- deref current
              return
                (pending_task ==? bounce_task ==> do
                  store resched_mbox (fromInteger mbox_idx)
                  ret true
                )
            cond_ conds

          ret false

    -- Remove a task from task_queue, either because it has completed,
    -- or because its client has requested to abort it.
    let removeTask = proc "remove_task" $ \ initial_task -> body $ do
          target <- local $ ival initial_task

          let current_queue = zip prio_queue task_queue
          let next_queue = [ (constRef prio, constRef task) | (prio, task) <- drop 1 current_queue ] ++ [(sentinel_prio, sentinel_task)]

          let checkPlace ((current_prio, current_task), (next_prio, next_task)) next = do
                done <- call shiftDown target current_prio current_task next_prio next_task
                unless done next
          foldr checkPlace (return ()) (zip current_queue next_queue)

          final_task <- deref target
          when (initial_task ==? final_task) $ do
            comment "Task not found, hopefully because it was previously aborted."
            ret false

          comment "Task found; check that we reached the end of the list."
          assert (final_task ==? maxBound)
          ret true

    monitorModuleDef $ do
      incl getTaskRequest
      incl isTaskQueued
      incl nextTask
      incl insertTask
      incl removeTask
      private $ do
        incl isTaskCurrent

    -- Channel handlers:

    forM_ mbox_states $ \ (idx, mbox, current) -> do
      -- Handle a transmit-complete event. On entry, the task must be in
      -- "on-hardware", "reschedule", or "abort" states. On exit, the
      -- task will be "idle" if it completed successfully, or either
      -- "schedule" or "idle" if unsuccessful. In addition, if any other
      -- task was in "schedule" state, on exit the highest-priority of
      -- those will be in "on-hardware" state.
      --
      -- We can't check the state precondition at runtime because the
      -- response from the driver doesn't indicate which message
      -- completed, only the status of the given mailbox. Instead we
      -- assume that we've maintained the invariants on our "current"
      -- state variables.
      handler (abortableComplete mbox) ("mailbox_" ++ show idx ++ "_complete") $ do
        taskComplete <- emitter doTaskComplete 1
        sendReq <- emitter (abortableTransmit mbox) 1

        callbackV $ \ success -> do
          current_task <- deref current
          assert (current_task /=? maxBound)

          res <- fmap constRef $ local $ istruct
            [ task_idx .= ival current_task
            , task_success .= ival success
            ]

          ifte_ success
            (do
              comment "On success, always report back to the task."
              call_ removeTask current_task
              emit taskComplete res
            ) (do
              comment "On failure: did the task abort, or are we rescheduling?"
              still_queued <- call isTaskQueued current_task
              unless still_queued $ do
                comment "Task aborted and is no longer queued. Complete it."
                emit taskComplete res
            )

          store current maxBound
          let bit = fromInteger (1 `shiftL` fromInteger idx)
          already_aborting <- deref mbox_aborting
          store mbox_aborting (already_aborting .& iComplement bit)

          next <- call nextTask
          when (next /=? maxBound) $ do
            req <- call getTaskRequest next
            store current next
            emit sendReq req

    -- Handle a taskAbort event. On entry, the task may be in any state,
    -- including abort or idle. On exit, if the task was current, then
    -- its new state is "abort"; otherwise, it is "idle".
    handler taskAbortChan "task_abort" $ do
      emitters <- forM mbox_states $ \ (idx, mbox, current) -> do
        e <- emitter (abortableAbort mbox) 1
        return (idx, current, e)
      taskComplete <- emitter doTaskComplete 1
      callbackV $ \ task -> do
        removed <- call removeTask task
        when removed $ do
          -- If this task is current in some mailbox, abort that mailbox.
          abort_msg <- fmap constRef $ local $ ival true
          already_aborting <- deref mbox_aborting
          current_conds <- forM emitters $ \ (idx, current, e) -> do
            current_task <- deref current
            return $ (task ==? current_task ==>) $ do
              let bit = fromInteger (1 `shiftL` fromInteger idx)
              when ((already_aborting .& bit) ==? 0) $ do
                store mbox_aborting (already_aborting .| bit)
                emit e abort_msg

          -- Otherwise, we hadn't handed the task off to the hardware yet,
          -- so we can immediately report that it wasn't sent, without
          -- waiting for a hardware abort.
          cond_ $ current_conds ++ [ true ==> do
              res <- fmap constRef $ local $ istruct
                [ task_idx .= ival task
                , task_success .= ival false
                ]
              emit taskComplete res
            ]

    forM_ task_states $ \ (idx, task, last_request) -> do
      -- Handle a taskRequest event. On entry, the task must be in
      -- "idle" state. On exit, it will either be in "schedule" or
      -- "on-hardware" states, according to whether there's a free
      -- mailbox available.
      handler (canTaskReq task) ("task_" ++ show idx ++ "_request") $ do
        emitters <- forM mbox_states $ \ (mbox_idx, mbox, current) -> do
          sendReq <- emitter (abortableTransmit mbox) 1
          abort <- emitter (abortableAbort mbox) 1
          return (mbox_idx, current, sendReq, abort)

        callback $ \ req -> do
          resched_task <- local $ ival maxBound
          resched_mbox <- local $ ival maxBound
          needs_resched <- call insertTask (fromInteger idx) resched_task resched_mbox last_request req
          when needs_resched $ do
            target_task <- deref resched_task
            mailbox <- deref resched_mbox
            ifte_ (target_task ==? maxBound)
              (do
                abortReq <- fmap constRef $ local $ ival true
                already_aborting <- deref mbox_aborting
                cond_
                  [ mailbox ==? fromInteger mbox_idx ==> do
                      let bit = fromInteger (1 `shiftL` fromInteger mbox_idx)
                      when ((already_aborting .& bit) ==? 0) $ do
                        store mbox_aborting (already_aborting .| bit)
                        emit abort abortReq
                  | (mbox_idx, _, _, abort) <- emitters
                  ]
              ) (do
                taskReq <- call getTaskRequest target_task
                cond_
                  [ mailbox ==? fromInteger mbox_idx ==> do
                      store current target_task
                      emit sendReq taskReq
                  | (mbox_idx, current, sendReq, _) <- emitters
                  ]
              )

      -- Delegate taskAbort events to the common task abort handler,
      -- above. This only adds the internal task ID to the request.
      -- Note: aborts can race with completion, and this handler
      -- delegates to another handler, which ends this critical section.
      -- So this handler should not modify any monitor state; the common
      -- abort handler is atomic with respect to other task aborts and
      -- any completions.
      handler (canTaskAbortReq task) ("task_" ++ show idx ++ "_abort") $ do
        taskAbort <- emitter doTaskAbort 1
        callback $ const $ emitV taskAbort $ fromInteger idx

    -- Deliver a task-complete notification to its client, and record
    -- that this task is now allowed to submit another request. This
    -- must be triggered when and only when the task has just
    -- transitioned to the "idle" state from any other state.
    --
    -- However, it doesn't need to run immediately in the same critical
    -- section as that state transition, because: (1) further task abort
    -- requests will no-op in the "idle" state; (2) a well-behaved
    -- client won't send another request until this handler emits its
    -- completion notice; (3) a poorly-behaved client will trigger an
    -- assert in its canTaskReq handler.
    --
    -- So this code is factored out to a separate handler because it's
    -- safe to do so and this reduces code duplication elsewhere.
    handler taskCompleteChan "task_complete" $ do
      emitters <- forM task_states $ \ (idx, task, last_request) -> do
        e <- emitter (canTaskRes task) 1
        return (idx, e, last_request)

      callback $ \ res -> do
        task <- deref $ res ~> task_idx
        assert (task <? fromIntegral (length tasks))
        cond_
          [ task ==? fromInteger idx ==> do
              store (last_request ~> can_message_id) $ fromRep maxBound
              emit e $ res ~> task_success
          | (idx, e, last_request) <- emitters
          ]
