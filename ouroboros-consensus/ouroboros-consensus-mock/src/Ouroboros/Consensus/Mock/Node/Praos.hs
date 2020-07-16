{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Praos (
    MockPraosBlock
  , protocolInfoPraos
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

type MockPraosBlock = SimplePraosBlock SimpleMockCrypto PraosMockCrypto

protocolInfoPraos :: IOLike m
                  => NumCoreNodes
                  -> CoreNodeId
                  -> PraosParams
                  -> HardFork.EraParams
                  -> ProtocolInfo m MockPraosBlock
protocolInfoPraos numCoreNodes nid params eraParams =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PraosConfig {
                praosParams       = params
              , praosSignKeyVRF   = signKeyVRF nid
              , praosInitialEta   = 0
              , praosInitialStake = genesisStakeDist addrDist
              , praosVerKeys      = verKeys
              }
          , topLevelConfigBlock = FullBlockConfig {
                blockConfigLedger = SimpleLedgerConfig addrDist eraParams
              , blockConfigBlock  = SimpleBlockConfig (praosSecurityParam params)
              , blockConfigCodec  = SimpleCodecConfig (praosSecurityParam params)
              }
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = genesisSimpleLedgerState addrDist
          , headerState = genesisHeaderState (PraosChainDepState [])
          }
      , pInfoBlockForging = Just (praosBlockForging nid initHotKey)

      }
  where
    signKeyVRF :: CoreNodeId -> SignKeyVRF MockVRF
    signKeyVRF (CoreNodeId n) = SignKeyMockVRF n

    verKeyVRF :: CoreNodeId -> VerKeyVRF MockVRF
    verKeyVRF (CoreNodeId n) = VerKeyMockVRF n

    verKeyKES :: CoreNodeId -> VerKeyKES (MockKES t)
    verKeyKES (CoreNodeId n) = VerKeyMockKES n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES (MockKES t), VerKeyVRF MockVRF)
    verKeys = Map.fromList
      [ (nid', (kesKey, vrfKey))
      | nid' <- enumCoreNodes numCoreNodes
      , let !kesKey = verKeyKES nid'
            !vrfKey = verKeyVRF nid'
      ]

    initHotKey :: HotKey PraosMockCrypto
    initHotKey =
      HotKey $
        SignKeyMockKES
          -- key ID
          (fst $ verKeys Map.! nid)
          -- KES initial slot
          0

praosBlockForging ::
     IOLike m
  => CanBeLeader (BlockProtocol MockPraosBlock)
  -> HotKey PraosMockCrypto
  -> m (BlockForging m MockPraosBlock)
praosBlockForging canBeLeader initHotKey = do
    varHotKey <- newMVar initHotKey
    return $ BlockForging {
        updateForgeState = \curSlot -> do
          modifyMVar varHotKey $ \hotKey -> do
            hotKey' <- evolveKey curSlot hotKey
            return (hotKey', hotKey')
      , canBeLeader = canBeLeader
      , forgeBlock = \cfg curNo curSlotNo tickedLedgerState txs isLeader -> do
          hotKey <- readMVar varHotKey
          return $
            forgeSimple
              hotKey
              cfg
              curNo
              curSlotNo
              tickedLedgerState
              txs
              isLeader
      }
