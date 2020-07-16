{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.Forging (
    hardForkBlockForging
  ) where

import           Data.Functor.Product
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger (Ticked (..))
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

hardForkBlockForging ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> BlockForging m (HardForkBlock xs)
hardForkBlockForging blockForgings =
    BlockForging {
        updateForgeState = hardForkUpdateForgeState             blockForgings
      , canBeLeader      = hmap (WrapCanBeLeader . canBeLeader) blockForgings
      , forgeBlock       = hardForkForgeBlock                   blockForgings
      }

hardForkUpdateForgeState ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> SlotNo
  -> m (HardForkForgeStateInfo xs)
hardForkUpdateForgeState blockForgings curSlot =
    PerEraForgeStateInfo <$> htraverse' updateOne blockForgings
  where
    updateOne :: BlockForging m a -> m (WrapForgeStateInfo a)
    updateOne blockForging =
        WrapForgeStateInfo <$> updateForgeState blockForging curSlot

hardForkForgeBlock ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> BlockNo
  -> SlotNo
  -> TickedLedgerState (HardForkBlock xs)
  -> [GenTx (HardForkBlock xs)]
  -> HardForkIsLeader xs
  -> m (HardForkBlock xs)
hardForkForgeBlock blockForgings
                   cfg
                   bno
                   sno
                   (TickedHardForkLedgerState transition ledgerState)
                   txs
                   isLeader = do
    -- First establish the 'IsLeader' and the 'LedgerState' are from the
    -- same era. As we have passed the ledger view of the ticked ledger to
    -- obtain the 'IsLeader' value, it __must__ be from the same era.
    -- Unfortunately, we cannot avoid this 'error' call: the 'IsLeader'
    -- evidence could conceivably include the ledger /view/, but not the
    -- ledger /state/.
    case State.match (getOneEraIsLeader isLeader) ledgerState of
      Left _mismatch ->
        error "IsLeader from different era than the TickedLedgerState"
      Right matched  ->
        -- Although we get a list with transactions that each could be from
        -- a different era, we know they have been validated against the
        -- 'LedgerState', which means they __must__ be from the same era.
        fmap (HardForkBlock . OneEraBlock) $
        hsequence $
        hpure (fn_4 forgeBlockOne)
          `hap` fromOptNP blockForgings
          `hap` distribTopLevelConfig ei cfg
          `hap` (partition_NS (map (getOneEraGenTx . getHardForkGenTx) txs))
          `hap` (State.tip matched)
  where
    ei = State.epochInfoPrecomputedTransitionInfo
           (hardForkLedgerConfigShape (configLedger cfg))
           transition
           ledgerState

    -- | Unwraps all the layers needed for SOP and call 'forgeBlock'.
    forgeBlockOne ::
         (Maybe :.: BlockForging m) blk
      -> TopLevelConfig blk
      -> ([] :.: GenTx) blk
      -> Product WrapIsLeader (Ticked :.: LedgerState) blk
      -> m blk
    forgeBlockOne (Comp mBlockForging)
                  matchedCfg
                  (Comp matchedTxs)
                  (Pair matchedIsLeader (Comp matchedLedgerState)) =
        forgeBlock
          blockForging
          matchedCfg
          bno
          sno
          matchedLedgerState
          matchedTxs
          (unwrapIsLeader matchedIsLeader)
      where
        blockForging = case mBlockForging of
          Just bf -> bf
          Nothing ->
            error "forging a block in an era in which we cannot lead"
