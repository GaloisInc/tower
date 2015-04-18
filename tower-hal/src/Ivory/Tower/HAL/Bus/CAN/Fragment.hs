{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.HAL.Bus.CAN.Fragment
  ( MessageType
  , messageType, messageType'
  , fragmentSender, fragmentSenderBlind
  , FragmentReceiveHandler
  , fragmentReceiveHandler
  , fragmentReceiver
  ) where

import Control.Monad (forM)
import Data.Either
import Data.List
import Data.Ord
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Numeric

data MessageType a = forall len. ANat len => MessageType
  { fragmentBaseID :: Int
  , fragmentIDExtended :: Bool
  , fragmentLength :: Proxy len
  , fragmentPackRep :: PackRep a
  }

messageType :: (ANat len, Packable a)
            => Int
            -> Bool
            -> Proxy len
            -> MessageType a
messageType baseID ide bound = messageType' baseID ide bound packRep

messageType' :: ANat len
             => Int
             -> Bool
             -> Proxy len
             -> PackRep a
             -> MessageType a
messageType' baseID ide bound rep
  | packSize rep /= fromTypeNat bound = error $
      "wrong buffer size " ++ show (fromTypeNat bound)
      ++ " given for CAN ID 0x" ++ showHex baseID ": should be "
      ++ show (packSize rep)
  | otherwise = MessageType
      { fragmentBaseID = baseID
      , fragmentIDExtended = ide
      , fragmentLength = bound
      , fragmentPackRep = rep
      }

fragmentSender :: (IvoryArea a, IvoryZero a)
               => MessageType a
               -> AbortableTransmit (Struct "can_message") (Stored IBool)
               -> Tower e (ChanInput a, ChanInput (Stored IBool), ChanOutput (Stored IBool))
fragmentSender (MessageType baseID ide bound rep) tx = do
  (reqChan, reqSrc) <- channel
  (abortChan, abortSrc) <- channel
  (resDst, resChan) <- channel

  let idstr = "0x" ++ showHex baseID ""
  monitor ("fragment_" ++ idstr) $ do
    sent <- stateInit ("fragment_sent_" ++ idstr) (izero :: Init (Stored Uint8))
    buf <- stateInit ("fragment_buf_" ++ idstr) (izerolen bound)
    aborting <- state ("fragment_aborting_" ++ idstr)

    let sendFragment idx = do
          let remaining_len = arrayLen buf - 8 * idx
          let len = (remaining_len >? 8) ? (8, remaining_len)
          msg <- local $ istruct
            [ can_message_id .= ival (if ide
                then extendedCANID (fromRep $ fromIntegral baseID + safeCast idx) (boolToBit false)
                else standardCANID (fromRep $ fromIntegral baseID + safeCast idx) (boolToBit false))
            , can_message_len .= ival (toIx len)
            ]
          for (toIx len) $ \ i -> refCopy (msg ~> can_message_buf ! i) (buf ! toIx (safeCast idx * 8 + fromIx i))
          store sent (idx + 1)
          return msg

    handler reqSrc ("fragment_req_" ++ idstr) $ do
      txReq <- emitter (abortableTransmit tx) 1
      callback $ \ req -> do
        was_sent <- deref sent
        assert $ was_sent ==? 0

        packInto' rep buf 0 req
        msg <- sendFragment 0
        emit txReq $ constRef msg

    handler (abortableComplete tx) ("fragment_complete_" ++ idstr) $ do
      txReq <- emitter (abortableTransmit tx) 1
      res <- emitter resDst 1
      callbackV $ \ success -> do
        let finished v = do
              emitV res v
              store sent 0
              store aborting false

        ifte_ (iNot success) (finished false) $ do
          already_sent <- deref sent
          assert $ already_sent >? 0

          ifte_ (arrayLen buf <=? 8 * (safeCast already_sent :: Uint16)) (finished true) $ do
            should_abort <- deref aborting
            ifte_ should_abort (finished false) $ do
              msg <- sendFragment already_sent
              emit txReq $ constRef msg

    handler abortSrc ("fragment_abort_" ++ idstr) $ do
      txAbort <- emitter (abortableAbort tx) 1
      callback $ const $ do
        store aborting true
        emitV txAbort true

  return (reqChan, abortChan, resChan)

-- | Like fragmentSender, but provides no feedback about the success or
-- failure of the transmission. Useful when the caller doesn't need to
-- know whether the message made it onto the bus.
fragmentSenderBlind :: (IvoryArea a, IvoryZero a)
                    => ChanOutput a
                    -> MessageType a
                    -> AbortableTransmit (Struct "can_message") (Stored IBool)
                    -> Tower e ()
fragmentSenderBlind src mt tx = do
  (fragReq, fragAbort, fragDone) <- fragmentSender mt tx

  let idstr = "0x" ++ showHex (fragmentBaseID mt) ""
  monitor ("fragment_blindly_" ++ idstr) $ do
    msg <- state ("msg_" ++ idstr)
    in_progress <- state ("in_progress_" ++ idstr)
    abort_pending <- state ("abort_pending_" ++ idstr)

    handler src "new_msg" $ do
      toFrag <- emitter fragReq 1
      doAbort <- emitter fragAbort 1
      callback $ \ new_msg -> do
        refCopy msg new_msg
        was_in_progress <- deref in_progress
        ifte_ was_in_progress (emitV doAbort true >> store abort_pending true) (emit toFrag (constRef msg) >> store in_progress true)

    handler fragDone "fragment_done" $ do
      toFrag <- emitter fragReq 1
      callback $ const $ do
        was_aborting <- deref abort_pending
        when was_aborting $ do
          emit toFrag $ constRef msg
          store abort_pending false
        store in_progress was_aborting

data FragmentReceiveHandler = forall a. (IvoryArea a, IvoryZero a) => FragmentReceiveHandler
  { fragmentReceiveChannel :: ChanInput a
  , fragmentReceiveType :: MessageType a
  }

-- | Associate reassembled messages of the given type with the given
-- channel.
fragmentReceiveHandler :: (IvoryArea a, IvoryZero a)
                       => ChanInput a
                       -> MessageType a
                       -> FragmentReceiveHandler
fragmentReceiveHandler chan mt = FragmentReceiveHandler
  { fragmentReceiveChannel = chan
  , fragmentReceiveType = mt
  }

data GeneratedHandler = GeneratedHandler
  { generatedBaseID :: Int
  , generatedHandler :: forall s s'. Uint8 -> ConstRef s (Struct "can_message") -> Ivory (AllocEffects s') ()
  }

-- | Attach all of the given 'FragmentReceiveHandler's to this stream
-- of incoming CAN messages, reassembling fragments appropriately.
fragmentReceiver :: ChanOutput (Struct "can_message")
                 -> [FragmentReceiveHandler]
                 -> Tower e ()
fragmentReceiver src handlers = do
  monitor "fragment_reassembly" $ do
    emitters <- forM handlers $ \ (FragmentReceiveHandler chan (MessageType baseID ide bound rep)) -> do
      let idstr = "0x" ++ showHex baseID ""
      next_idx <- stateInit ("reassembly_next_idx_" ++ idstr) (izero :: Init (Stored Uint8))
      buf <- stateInit ("reassembly_buf_" ++ idstr) (izerolen bound)

      let last_fragment_idx = fromInteger $ (arrayLen buf - 1) `div` 8
      let last_fragment_length = fromInteger $ arrayLen buf `mod` 8

      return $ do
        e <- emitter chan 1
        return $ (if ide then Left else Right) $ GeneratedHandler
          { generatedBaseID = baseID
          , generatedHandler = \ idx msg -> do
              expected_idx <- deref next_idx
              let is_retransmit = idx + 1 ==? expected_idx
              let is_not_mine = idx >? last_fragment_idx
              unless (is_retransmit .|| is_not_mine) $ do
                len <- fmap fromIx $ deref $ msg ~> can_message_len
                let is_new_fragment = idx ==? 0
                let is_expected_fragment = idx ==? expected_idx
                let has_bad_idx = iNot (is_new_fragment .|| is_expected_fragment)
                let expected_length = (idx ==? last_fragment_idx) ? (last_fragment_length, 8)
                let has_bad_length = len /=? expected_length
                let discard = store next_idx 0
                ifte_ (has_bad_idx .|| has_bad_length) discard $ do
                  arrayCopy buf (msg ~> can_message_buf) (safeCast idx * 8) len
                  ifte_ (idx <? last_fragment_idx) (store next_idx $ idx + 1) $ do
                    assembled <- local izero
                    unpackFrom' rep (constRef buf) 0 assembled
                    emit e $ constRef assembled
          }

    handler src "receive_msg" $ do
      (ext, std) <- fmap partitionEithers $ sequence emitters
      callback $ \ msg -> do
        arbitration <- deref $ msg ~> can_message_id
        let msgID = toRep $ arbitration #. can_arbitration_id
        let one (GeneratedHandler baseID f) = (msgID >=? fromIntegral baseID) ==> f (castDefault $ msgID - fromIntegral baseID) msg
        let handle = cond_ . map one . sortBy (flip $ comparing generatedBaseID)
        let ide = bitToBool $ arbitration #. can_arbitration_ide
        ifte_ ide (handle ext) (handle std)
