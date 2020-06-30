{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TipSample.Codec
  ( codecTipSample
  , codecTipSampleId

  , byteLimitsTipSample
  , timeLimitsTipSample
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime

import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero), natToInt, unsafeIntToNat)

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.TipSample.Type
import           Ouroboros.Network.Protocol.Limits

import qualified Data.ByteString.Lazy as LBS
import           Data.Typeable (Typeable, cast)

import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.Serialise as Serialise (Serialise (..))
import           Text.Printf


codecTipSample
  :: forall m tip. MonadST m
  => (tip -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tip)
  -> Codec (TipSample tip)
           CBOR.DeserialiseFailure m LBS.ByteString
codecTipSample encodeTip decodeTip =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr  :: PeerRole)
                     (st  :: TipSample tip)
                     (st' :: TipSample tip).
              PeerHasAgency pr st
           -> Message (TipSample tip) st st'
           -> CBOR.Encoding
    encode (ClientAgency TokInitClient) MsgGetCurrentTip =
      encodeListLen 1 <> encodeWord 0

    encode (ClientAgency TokIdle) (MsgGetTipAfterSlotNo slotNo) =
      encodeListLen 2 <> encodeWord 1 <> Serialise.encode slotNo

    encode (ClientAgency TokIdle) (MsgGetTipAfterTip tip) =
      encodeListLen 2 <> encodeWord 2 <> encodeTip tip

    encode (ClientAgency TokIdle) MsgDone =
      encodeListLen 1 <> encodeWord 3

    encode (ServerAgency TokInitServer) (MsgCurrentTip tip) =
      encodeListLen 2 <> encodeWord 4 <> encodeTip tip

    encode (ServerAgency (TokBusy _)) (MsgTip tip) =
      encodeListLen 2 <> encodeWord 5 <> encodeTip tip

    encode (ClientAgency TokIdle) (MsgFollowTip n) =
      encodeListLen 2 <> encodeWord 6 <> CBOR.encodeInt (natToInt n)

    encode (ServerAgency (TokFollowTip _)) (MsgNextTip tip) =
      encodeListLen 2 <> encodeWord 7 <> encodeTip tip

    encode (ServerAgency (TokFollowTip _)) (MsgNextTipDone tip) =
      encodeListLen 2 <> encodeWord 8 <> encodeTip tip


    decode :: forall (pr :: PeerRole) (st :: TipSample tip) s.
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- decodeListLen
      key <- decodeWord
      case (key, len, stok) of
        (0, 1, ClientAgency TokInitClient) ->
          pure (SomeMessage MsgGetCurrentTip)

        (1, 2, ClientAgency TokIdle) ->
          SomeMessage . MsgGetTipAfterSlotNo <$> Serialise.decode

        (2, 2, ClientAgency TokIdle) ->
          SomeMessage . MsgGetTipAfterTip <$> decodeTip

        (3, 1, ClientAgency TokIdle) ->
          pure (SomeMessage MsgDone)

        (4, 2, ServerAgency TokInitServer) ->
          SomeMessage . MsgCurrentTip <$> decodeTip

        (5, 2, ServerAgency (TokBusy TokBlockUntilSlot)) ->
          SomeMessage . MsgTip <$> decodeTip

        (5, 2, ServerAgency (TokBusy TokBlockUntilTip)) ->
          SomeMessage . MsgTip <$> decodeTip

        (6, 2, ClientAgency TokIdle) ->
          SomeMessage . MsgFollowTip . unsafeIntToNat <$> CBOR.decodeInt

        (7, 2, ServerAgency (TokFollowTip (Succ (Succ _)))) ->
          SomeMessage . MsgNextTip <$> decodeTip

        (8, 2, ServerAgency (TokFollowTip (Succ Zero))) ->
          SomeMessage . MsgNextTipDone <$> decodeTip

        --
        -- failures
        --

        (_, _, _) ->
          fail (printf "codecTipSample (%s) unexpected key (%d, %d)" (show stok) key len)


codecTipSampleId :: forall tip m.
                    ( Monad m
                    , Typeable tip
                    )
                 => Codec (TipSample tip)
                          CodecFailure m (AnyMessage (TipSample tip))
codecTipSampleId = Codec encode decode
  where
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (TipSample tip) st st'
           -> AnyMessage (TipSample tip)
    encode _ = AnyMessage

    decode :: forall (pr :: PeerRole) st.
              PeerHasAgency pr st
           -> m (DecodeStep (AnyMessage (TipSample tip))
                            CodecFailure m (SomeMessage st))
    decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
      (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput

      (ClientAgency TokInitClient, Just (AnyMessage msg@MsgGetCurrentTip{})) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      (ClientAgency TokIdle, Just (AnyMessage msg@MsgGetTipAfterSlotNo{})) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      (ClientAgency TokIdle, Just (AnyMessage msg@MsgGetTipAfterTip{})) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      (ClientAgency TokIdle, Just (AnyMessage msg@MsgDone)) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      (ServerAgency TokInitServer, Just (AnyMessage msg@MsgCurrentTip{})) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      -- We are using 'Typeable' constraint to recover the type of 'AnyMessage'
      (ServerAgency (TokBusy TokBlockUntilSlot), Just (AnyMessage msg@MsgTip{})) ->
        case cast msg :: Maybe (Message (TipSample tip) (StBusy BlockUntilSlot) StIdle) of
          Just msg' ->
            pure (DecodeDone (SomeMessage msg') Nothing)
          Nothing ->
            pure (DecodeFail (CodecFailure "codecTipSample: mismatching TipRequestKind"))

      (ServerAgency (TokBusy TokBlockUntilTip), Just (AnyMessage msg@MsgTip{})) ->
        case cast msg :: Maybe (Message (TipSample tip) (StBusy BlockUntilTip) StIdle) of
          Just msg' ->
            pure (DecodeDone (SomeMessage msg') Nothing)
          Nothing ->
            pure (DecodeFail (CodecFailure "codecTipSample: mismatching TipRequestKind"))

      (_, _) -> pure $ DecodeFail (CodecFailure "codecTipSample: no matching message")


-- | Byte limits for 'TipSample' protocol.
--
byteLimitsTipSample :: forall tip bytes.
                       (bytes -> Word)
                    -> ProtocolSizeLimits (TipSample tip) bytes
byteLimitsTipSample = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: TipSample tip).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokInitClient)               = smallByteLimit
    stateToLimit (ClientAgency TokIdle)                     = smallByteLimit
    stateToLimit (ServerAgency TokInitServer)               = smallByteLimit
    stateToLimit (ServerAgency (TokBusy TokBlockUntilSlot)) = smallByteLimit
    stateToLimit (ServerAgency (TokBusy TokBlockUntilTip))  = smallByteLimit
    stateToLimit (ServerAgency (TokFollowTip _))            = smallByteLimit


-- | Time limits for 'TipSample' protocol.
--
timeLimitsTipSample :: forall tip.
                       ProtocolTimeLimits (TipSample tip)
timeLimitsTipSample = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: TipSample tip).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokInitClient)               = waitForever
    stateToLimit (ClientAgency TokIdle)                     = waitForever
    stateToLimit (ServerAgency TokInitServer)               = shortWait
    stateToLimit (ServerAgency (TokBusy TokBlockUntilSlot)) = waitForever
    stateToLimit (ServerAgency (TokBusy TokBlockUntilTip))  = waitForever
    stateToLimit (ServerAgency (TokFollowTip _))            = longWait
