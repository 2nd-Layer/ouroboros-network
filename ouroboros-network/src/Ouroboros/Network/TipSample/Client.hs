{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TipSample.Client where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception (Exception (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           System.Random

import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero), unsafeIntToNat)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Network.Block (Tip (..), StandardHash)
import           Ouroboros.Network.Mux (ScheduledStop, RunOrStop (..))
import           Ouroboros.Network.TipSample.TipFragment
import           Ouroboros.Network.Protocol.TipSample.Client




data TipSamplePolicy = TipSamplePolicy {
      -- | slot range; The client will keep only that oldest slot that many slots
      -- behind the last received slot.
      --
      tspRange :: Word64,

      tspTipAfterSlotNoWeight :: Rational,

      -- | range from which we randomly choose to a number of slots ahead of the
      -- current slot (for 'MsgGetTipAfterSlotNo').
      --
      tspTipAfterSlotNoRange :: (Word64, Word64),

      tspTipAfterTipWeight :: Rational,

      tspFollowTipWeight :: Rational,

      -- | range from which we randomly choose how many tips to follow.
      -- 
      tspFollowTipRange :: (Word64, Word64)
    }


data TipSampleDecision
    = TipAfterSlotNo
    | TipAfterTip
    | FollowTip


randomTipSampleDecision
    :: RandomGen g
    => Rational
    -- ^ 'tspTipAfterSlotNoWeight'
    -> Rational
    -- ^ 'tspTipAfterTipWeight'
    -> Rational
    -- ^ 'tspFollowTipWeight'
    -> g
    -> (TipSampleDecision, g)
randomTipSampleDecision
    tipAfterSlotNoWeight
    tipAfterTipWeight
    followTipWeight
    g =
      case randomR (0 :: Int, 100) g of
        (a, g') | toRational a <= 100 * tipAfterSlotNoWeight / total
                -> (TipAfterSlotNo, g')

                | toRational a <= 100 * (tipAfterSlotNoWeight + tipAfterTipWeight) / total
                -> (TipAfterTip, g')

                | otherwise
                -> (FollowTip, g')
  where
    total = tipAfterSlotNoWeight + tipAfterTipWeight + followTipWeight


data TipSampleState block m = TipSampleState {
    tssLastTip        :: !(Tip block),
    tssGen            :: !StdGen,
    tssTipFragmentVar :: !(StrictTVar m (TipFragment block))
  }


data TipSampleClientValidationError block =
    TipSampleWrongSlot SlotNo (Tip block)
  | TipSampleWrongTip (Tip block) (Tip block)
  deriving Show

instance (Typeable block, StandardHash block)
    => Exception (TipSampleClientValidationError block)

afterSlotNo :: Tip block
            -> Word64
            -> SlotNo
afterSlotNo (Tip (SlotNo slotNo) _ _) x = SlotNo (slotNo + x)
afterSlotNo TipGenesis                x = SlotNo x


compareTipsBySlotNo :: Tip block
                    -> Tip block
                    -> Ordering
compareTipsBySlotNo (Tip slotNo _ _) (Tip slotNo' _ _) = slotNo `compare` slotNo'
compareTipsBySlotNo Tip{}            TipGenesis        = LT
compareTipsBySlotNo TipGenesis       Tip{}             = GT
compareTipsBySlotNo TipGenesis       TipGenesis        = EQ


tipSampleClient :: forall block m.
                   ( MonadMonotonicTime m
                   , MonadSTM           m
                   , MonadThrow         m
                   , StandardHash block
                   , Typeable     block
                   )
                => TipSamplePolicy
                -> ScheduledStop m
                -> StdGen
                -> StrictTVar m (TipFragment block)
                -> TipSampleClient (Tip block) m ()
tipSampleClient TipSamplePolicy {
                  tspTipAfterSlotNoWeight,
                  tspTipAfterTipWeight,
                  tspFollowTipWeight,
                  tspTipAfterSlotNoRange,
                  tspFollowTipRange
                }
                scheduledStop g tipFragmentVar =
    TipSampleClient $ pure $ \tip -> do
      t <- getMonotonicTime
      ss <-
        atomically $ do
          modifyTVar tipFragmentVar (:> Timed t tip)
          scheduledStop
      case ss of
        Run ->
          pure $ clientStIdle (TipSampleState tip g tipFragmentVar)
        Stop -> 
          pure $ SendMsgDone ()
  where
    makeDecision :: StdGen -> (TipSampleDecision, StdGen)
    makeDecision =
      randomTipSampleDecision
        tspTipAfterSlotNoWeight
        tspTipAfterTipWeight
        tspFollowTipWeight


    -- We only validate 'SlotNo' and 'BlockNo'; at this point we cannot trust
    -- the received hash;  Its validation is deffered until we actually use the
    -- tip.
    --
    -- Note: we only validate the 'Tip' if we expect it to move forward, both
    -- 'MsgGetTipAfterTip' and 'MsgFollowTip' might bring a 'Tip' at which we
    -- need to rollback.
    validateTip :: Either SlotNo (Tip block)
                -> Tip block
                -> Maybe (TipSampleClientValidationError block)
    validateTip (Left requestedSlotNo) tip@(Tip tipSlotNo _ _) =
      if tipSlotNo > requestedSlotNo
        then Nothing
        else Just (TipSampleWrongSlot requestedSlotNo tip)
    validateTip (Left requestedSlotNo) tip@TipGenesis =
      Just (TipSampleWrongSlot requestedSlotNo tip)
    validateTip (Right requestedTip@(Tip requestedSlotNo _ requestedBlockNo))
                                tip@(Tip slotNo          _ blockNo) =
      if (requestedSlotNo < slotNo && requestedBlockNo < blockNo)
        then Nothing
        else Just (TipSampleWrongTip requestedTip tip)
    validateTip (Right TipGenesis) Tip{} =
      Nothing
    validateTip (Right requestedTip) tip =
      Just (TipSampleWrongTip requestedTip tip)


    clientStIdle :: TipSampleState block m
                 -> ClientStIdle (Tip block) m ()
    clientStIdle st@TipSampleState { tssLastTip, tssGen, tssTipFragmentVar } =

      case makeDecision tssGen of
        (TipAfterSlotNo, tssGen') ->
          case randomR tspTipAfterSlotNoRange tssGen' of
            (a, tssGen'') ->
              let requestedSlotNo = afterSlotNo tssLastTip a in
              SendMsgGetTipAfterSlotNo requestedSlotNo $ \tip -> do
                t <- getMonotonicTime

                case validateTip (Left requestedSlotNo) tip of
                  Just err -> throwM err
                  Nothing  -> pure ()

                ss <-
                  atomically $ do
                    modifyTVar tssTipFragmentVar (:> Timed t tip)
                    scheduledStop
                case ss of
                  Run ->
                    pure $ clientStIdle st { tssLastTip = tip, tssGen = tssGen'' }
                  Stop ->
                    pure $ SendMsgDone ()

        (TipAfterTip, tssGen') ->
          SendMsgGetTipAfterTip tssLastTip $ \tip -> do
            t <- getMonotonicTime

            ss <-
              atomically $ do
                rollbackOrRollforward st (Timed t tip)
                scheduledStop
            case ss of
              Run ->
                pure $ clientStIdle st { tssLastTip = tip, tssGen = tssGen' }
              Stop ->
                pure $ SendMsgDone ()

        (FollowTip, tssGen') ->
          case randomR tspFollowTipRange tssGen' of
            (n, tssGen'') ->
              let m = unsafeIntToNat (fromIntegral n)
              in SendMsgFollowTip m (receiveTips m st { tssGen = tssGen'' })


    receiveTips :: Nat (S n)
                -> TipSampleState block m
                -> HandleTips (S n) (Tip block) m ()

    receiveTips (Succ Zero) st =
      ReceiveLastTip $ \tip -> do
        t <- getMonotonicTime

        ss <-
          atomically $ do
            rollbackOrRollforward st (Timed t tip)
            scheduledStop

        case ss of
          Run ->
            pure $ clientStIdle st { tssLastTip = tip }
          Stop ->
            pure $ SendMsgDone ()

    receiveTips (Succ n@Succ {}) st =
      ReceiveTip $ \tip -> do
        t <- getMonotonicTime
        atomically (rollbackOrRollforward st (Timed t tip))
        pure $ receiveTips n st { tssLastTip = tip }

    
    rollbackOrRollforward
        :: TipSampleState block m
        -> Timed (Tip block)
        -> STM m ()
    rollbackOrRollforward TipSampleState { tssLastTip, tssTipFragmentVar } tt =
      modifyTVar tssTipFragmentVar $ \tf -> 
        case tssLastTip `compareTipsBySlotNo` timedData tt of
          LT -> tf :> tt
          -- there was a rollback; Currently we rollback our state, but
          -- in the future we should preserver all the rollbacks on the
          -- `TipFragment`.
          _  -> case timedData tt of
                  Tip slotNo _ _ -> dropNewestUntilSlotNo slotNo tf :> tt
                  TipGenesis     -> Empty
