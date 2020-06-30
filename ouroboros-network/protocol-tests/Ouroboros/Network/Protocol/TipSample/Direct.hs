{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.TipSample.Direct
  ( direct ) where

import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Server


-- | The 'TipSampleClient' and 'TipSampleServer' are dual to each other in the
-- sense that they can be paired together using 'direct'.
--
direct :: forall tip m a b. Monad m
       => TipSampleClient tip m a
       -> TipSampleServer tip m b
       -> m (a, b)
direct (TipSampleClient mmclient) (TipSampleServer mserver) = do
    mclient <- mmclient
    (tip, server) <- mserver
    client <- mclient tip
    directStIdle client server


directStIdle :: forall tip m a b. Monad m
             => ClientStIdle tip m a
             -> ServerStIdle tip m b
             -> m (a, b)
directStIdle (SendMsgGetTipAfterSlotNo slotNo mclient)
             ServerStIdle { handleTipAfterSlotNo } = do
    (tip, server) <- handleTipAfterSlotNo slotNo
    client <- mclient tip 
    directStIdle client server

directStIdle (SendMsgGetTipAfterTip tip mclient)
             ServerStIdle { handleTipChange } = do
    (tip', server) <- handleTipChange tip
    client <- mclient tip'
    directStIdle client server

directStIdle (SendMsgDone a)
             ServerStIdle { handleDone } =
    (a,) <$> handleDone

directStIdle (SendMsgFollowTip n handleTips) ServerStIdle { handleFollowTip } =
    go handleTips (handleFollowTip n)
  where
    go :: HandleTips n tip m a
       -> SendTips n tip m b
       -> m (a, b)
    go (ReceiveTip k) (SendNextTip _ mFollowTip) = do
      (tip, followTip) <- mFollowTip
      receiveTip <- k tip
      go receiveTip followTip
    go (ReceiveLastTip k) (SendLastTip _ mFollowTip) = do
      (tip, serverIdle) <- mFollowTip
      clientIdle <- k tip
      directStIdle clientIdle serverIdle


