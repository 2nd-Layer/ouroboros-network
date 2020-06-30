{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures #-}

module Ouroboros.Network.Protocol.TipSample.Server (
    -- * Protocol type for the server
    TipSampleServer (..)
  , ServerStIdle (..)
  , SendTips (..)

    -- * Execution as a typed protocol
  , tipSampleServerPeer
  ) where

import           Cardano.Slotting.Slot (SlotNo)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))

import           Ouroboros.Network.Protocol.TipSample.Type


-- | A 'TipSample' protocol server .
--
newtype TipSampleServer tip m a = TipSampleServer {
    -- | 'RunTipSampleServer' must not block for a long time.
    --
    runTipSampleServer :: m (tip, ServerStIdle tip m a)
  }

-- | In 'StIdle' protocol state the server does not have agency, it is await for
-- one of:
--
-- * 'MsgGetTipAfterSlotNo'
-- * 'MsgGetTipAfterTip'
-- * 'MsgDone'
--
-- The 'ServerStIdle' contains handlers for all these messages.
--
data ServerStIdle tip m a = ServerStIdle {
    -- | The server is requested to return the first tip after 'SlotNo' (at or
    -- after to be precise); 'handleTipAfterSlotNo' might block.
    handleTipAfterSlotNo :: SlotNo ->  m (tip, ServerStIdle tip m a),
    handleTipChange      :: tip    ->  m (tip, ServerStIdle tip m a),
    handleFollowTip      :: forall (n :: N). Nat (S n) -> SendTips (S n) tip m a,
    handleDone           :: m a
  }

data SendTips (n :: N) tip m a where
    SendNextTip :: Nat (S (S n))
                -> m (tip, SendTips (S n) tip m a)
                -> SendTips (S (S n)) tip m a

    SendLastTip :: Nat (S Z)
                -> m (tip, ServerStIdle tip m a)
                -> SendTips (S Z) tip m a


-- | Interpret 'TipSampleServer' action sequence as a 'Peer' on the server side
-- of the 'TipSample' protocol.
--
tipSampleServerPeer
  :: forall tip m a.
     Monad m
  => TipSampleServer tip m a
  -> Peer (TipSample tip) AsServer (StInit StClientRequest) m a
tipSampleServerPeer (TipSampleServer mserver) =
    Await (ClientAgency TokInitClient) $ \MsgGetCurrentTip ->
    Effect $ 
      (\(currentTip, next) ->
        Yield (ServerAgency TokInitServer)
              (MsgCurrentTip currentTip)
              (serverStIdlePeer next))
      <$> mserver
  where
    -- (guarded) recursive step
    serverStIdlePeer :: ServerStIdle tip m a
                     -> Peer (TipSample tip) AsServer StIdle m a

    serverStIdlePeer
      ServerStIdle
        { handleTipAfterSlotNo
        , handleTipChange
        , handleFollowTip
        , handleDone
        } = 
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgGetTipAfterSlotNo slotNo ->
          Effect $
            (\(tip, next) ->
              Yield (ServerAgency (TokBusy TokBlockUntilSlot))
                    (MsgTip tip)
                    (serverStIdlePeer next))
            <$> handleTipAfterSlotNo slotNo

        MsgGetTipAfterTip tip ->
          Effect $
            (\(tip', next) ->
              Yield (ServerAgency (TokBusy TokBlockUntilTip))
                    (MsgTip tip')
                    (serverStIdlePeer next))
            <$> handleTipChange tip

        MsgFollowTip n ->
          serverStFollowTipPeer (handleFollowTip n)

        MsgDone ->
          Effect $ Done TokDone <$> handleDone


    serverStFollowTipPeer :: SendTips (S n) tip m a
                          -> Peer (TipSample tip) AsServer (StFollowTip (S n)) m a

    serverStFollowTipPeer (SendNextTip n@(Succ (Succ _)) mnext) = 
      Effect $
        (\(tip, next) ->
          Yield
            (ServerAgency (TokFollowTip n))
            (MsgNextTip tip)
            (serverStFollowTipPeer next)
          )
        <$> mnext

    serverStFollowTipPeer (SendLastTip n@(Succ Zero) mnext) =
      Effect $
        (\(tip, next) ->
            Yield
              (ServerAgency (TokFollowTip n))
              (MsgNextTipDone tip)
              (serverStIdlePeer next))
        <$> mnext


