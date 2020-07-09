{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TipSample.Server where

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM

import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero), N (..))

import           Cardano.Slotting.Slot (SlotNo)

import           Ouroboros.Network.Block (StandardHash, Tip (..))
import           Ouroboros.Network.Protocol.TipSample.Server


tipServer :: forall block m.
             ( MonadSTM m
             , StandardHash block
             )
          => STM m (Tip block)
          -- ^ 'ChainDB' 'getCurrentSlot' method.
          -> TipSampleServer (Tip block) m ()
tipServer getCurrentTip = TipSampleServer $ do
    tip <- atomically getCurrentTip
    pure (tip, serverStIdle)
  where
    serverStIdle :: ServerStIdle (Tip block) m ()
    serverStIdle = ServerStIdle {
        handleTipAfterSlotNo,
        handleTipChange,
        handleFollowTip,
        handleDone
      }

    handleTipAfterSlotNo :: SlotNo -> m (Tip block, ServerStIdle (Tip block) m ())
    handleTipAfterSlotNo slotNo = do
      atomically $ do
        tip <- getCurrentTip
        case tip of
          Tip slotNo' _ _ | slotNo' > slotNo -> pure (tip, serverStIdle)
                          | otherwise -> retry
          TipGenesis      -> retry

    handleTipChange :: Tip block -> m (Tip block, ServerStIdle (Tip block) m ())
    handleTipChange tip' = atomically $ do
      tip <- getCurrentTip
      when (tip == tip') retry
      pure (tip, serverStIdle)

    handleFollowTip :: forall (n :: N). Nat (S n) -> SendTips (S n) (Tip block) m ()
    handleFollowTip n@(Succ Zero) =
      SendLastTip n $
        atomically $ do
          tip <- getCurrentTip
          pure (tip, serverStIdle)
    handleFollowTip n@(Succ m@Succ{}) =
      SendNextTip n $
        atomically $ do
          tip <- getCurrentTip
          pure (tip, handleFollowTip m)

    handleDone :: m ()
    handleDone = pure ()
