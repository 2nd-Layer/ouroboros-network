{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Mock.Ledger.Forge (
    forgeSimple
  ) where

import           Cardano.Binary (toCBOR)
import           Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word

import           Cardano.Crypto.Hash (hashWithSerialiser)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

forgeSimple ::
     forall c ext. RunMockBlock c ext
  => ForgeStateInfo (BlockProtocol (SimpleBlock c ext))
  -> TopLevelConfig (SimpleBlock c ext)
  -> BlockNo                               -- ^ Current block number
  -> SlotNo                                -- ^ Current slot number
  -> TickedLedgerState (SimpleBlock c ext) -- ^ Current ledger
  -> [GenTx (SimpleBlock c ext)]           -- ^ Txs to include
  -> IsLeader (BlockProtocol (SimpleBlock c ext))
  -> SimpleBlock c ext
forgeSimple forgeStateInfo cfg curBlock curSlot tickedLedger txs proof =
    forgeExt cfg forgeStateInfo proof $ SimpleBlock {
        simpleHeader = mkSimpleHeader encode stdHeader ()
      , simpleBody   = body
      }
  where
    body :: SimpleBody
    body = SimpleBody { simpleTxs = map simpleGenTx txs }

    stdHeader :: SimpleStdHeader c ext
    stdHeader = SimpleStdHeader {
          simplePrev      = castHash $ getTipHash tickedLedger
        , simpleSlotNo    = curSlot
        , simpleBlockNo   = curBlock
        , simpleBodyHash  = hashWithSerialiser toCBOR body
        , simpleBlockSize = bodySize
        }

    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    bodySize :: Word64
    bodySize = fromIntegral $ Lazy.length $ serialise body
