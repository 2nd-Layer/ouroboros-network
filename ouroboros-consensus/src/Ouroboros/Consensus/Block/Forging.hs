{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Block.Forging (
    BlockForging(..)
  , getLeaderProof
  , hoistBlockForging
  ) where

import           Control.Tracer (Tracer, traceWith)
import           GHC.Stack

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((.....:))

-- | Stateful wrapper around block production
--
-- NOTE: do not refer to the consensus or ledger config in the closure of this
-- record because they might contain an @EpochInfo Identity@, which will be
-- incorrect when used as part of the hard fork combinator.
data BlockForging m blk = BlockForging {
      -- | Update the forge state and return info about its updated state.
      --
      -- When the node can be a leader, this will be called at the start of
      -- each slot, right before calling 'checkIsLeader'.
      --
      -- The returned info is traced.
      updateForgeState :: SlotNo
                       -> m (ForgeStateInfo (BlockProtocol blk))

      -- | Proof that the node can be a leader and produce blocks.
    , canBeLeader :: CanBeLeader (BlockProtocol blk)

      -- | Forge a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      --
      -- NOTE: do not refer to the consensus or ledger config in the closure,
      -- because they might contain an @EpochInfo Identity@, which will be
      -- incorrect when used as part of the hard fork combinator. Use the
      -- given 'TopLevelConfig' instead, as it is guaranteed to be correct
      -- even when used as part of the hard fork combinator.
    , forgeBlock :: TopLevelConfig blk
                 -> BlockNo               -- Current block number
                 -> SlotNo                -- Current slot number
                 -> TickedLedgerState blk -- Current ledger state
                 -> [GenTx blk]           -- Contents of the mempool
                 -> IsLeader (BlockProtocol blk) -- Proof we are leader
                 -> m blk
    }

getLeaderProof ::
     (Monad m, ConsensusProtocol (BlockProtocol blk), HasCallStack)
  => BlockForging m blk
  -> Tracer m (ForgeStateInfo (BlockProtocol blk))
  -> ConsensusConfig (BlockProtocol blk)
  -> SlotNo
  -> Ticked (ChainDepState (BlockProtocol blk))
  -> m (LeaderCheck (BlockProtocol blk))
getLeaderProof BlockForging{..} tracer cfg slot tickedChainDepState = do
    forgeStateInfo <- updateForgeState slot
    traceWith tracer forgeStateInfo
    return $
      checkIsLeader cfg canBeLeader forgeStateInfo slot tickedChainDepState

hoistBlockForging ::
     (forall x. m x -> n x)
  -> BlockForging m blk
  -> BlockForging n blk
hoistBlockForging f BlockForging{..} = BlockForging {
      updateForgeState = f . updateForgeState
    , canBeLeader      = canBeLeader
    , forgeBlock       = f .....: forgeBlock
    }
