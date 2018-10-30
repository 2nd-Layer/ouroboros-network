{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS -fno-warn-unused-binds #-}

module Test.Dynamic (
    tests
  ) where

import           Control.Monad
import           Control.Monad.ST.Lazy
import           Control.Monad.State
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Test.QuickCheck

import           Block
import           Chain
import           ChainProducerState
import           MonadClass
import           Node
import           Ouroboros
import           Protocol

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" prop_simple_bft_convergence
    ]

-- Run BFT on the broadcast network, and check that all nodes converge to the
-- same final chain
test_simple_bft_convergence :: forall m n stm.
                               ( MonadSTM m stm
                               , MonadRunProbe m n
                               , MonadSay m
                               , MonadProbe m
                               , Show (Time m)
                               )
                            => n Property
test_simple_bft_convergence =
    fmap isValid $ withProbe $ go
  where
    go :: Probe m (Map NodeId (Chain (Block 'TestLedgerDomain 'OuroborosBFT))) -> m ()
    go p = do
      finalChains <- broadcastNetwork
                       numSlots
                       nodeInit
                       appendBlock
      probeOutput p finalChains

    numNodes :: Int
    numNodes = 3

    numSlots :: Int
    numSlots = 10

    appendBlock :: NodeId
                -> Slot
                -> Chain (Block 'TestLedgerDomain 'OuroborosBFT)
                -> StateT Int stm (Chain (Block 'TestLedgerDomain 'OuroborosBFT))
    appendBlock (RelayId _) _ _ = error "appendBlock: relay cant be leader"
    appendBlock (CoreId us) (Slot slot) c = do
        count <- state $ \n -> (n + 1, n + 1)
        let body :: BlockBody 'TestLedgerDomain
            body = BlockBody $ concat [
                       "Block " ++ show count
                     , " produced in slot " ++ show slot
                     , " by core node " ++ show us
                     ]

            header :: BlockHeader 'OuroborosBFT
            header = BlockHeader {
                headerHash     = hashHeader header -- not actually recursive!
              , headerPrevHash = headHash c
              , headerSlot     = Slot slot
              , headerBlockNo  = succ $ headBlockNo c
              , headerSigner   = BlockSigner (fromIntegral us)
              , headerBodyHash = hashBody body
              }

            block :: Block 'TestLedgerDomain 'OuroborosBFT
            block = Block header body

        return $ c :> block

    nodeInit :: Map NodeId (Int, Chain (Block 'TestLedgerDomain 'OuroborosBFT))
    nodeInit = Map.fromList $ [ (CoreId i, (0, Genesis))
                              | i <- [0 .. numNodes - 1]
                              ]

    isValid :: [(Time m, Map NodeId (Chain (Block 'TestLedgerDomain 'OuroborosBFT)))] -> Property
    isValid trace = counterexample (show trace) $
      case trace of
        [(_, final)] -> Map.keys final == Map.keys nodeInit
                   .&&. allEqual (Map.elems final)
        _otherwise   -> property False

prop_simple_bft_convergence :: Property
prop_simple_bft_convergence = runST test_simple_bft_convergence

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | Setup fully-connected topology, where every node is both a producer
-- and a consumer
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
broadcastNetwork :: forall p m stm (dom :: LedgerDomain) st.
                    ( p   ~ 'OuroborosBFT     -- for now
                    , dom ~ 'TestLedgerDomain -- for now
                    , KnownOuroborosProtocol p
                    , MonadSTM   m stm
                    , MonadTimer m
                    , MonadSay   m
                    , Show     (Block dom p)
                    , HasHeader (Block dom)
                    )
                 => Int
                 -- ^ Number of slots to run for
                 -> Map NodeId (st, Chain (Block dom p))
                 -- ^ Node initial state and initial chain
                 -> (   NodeId
                     -> Slot
                     -> Chain (Block dom p) -> StateT st stm (Chain (Block dom p))
                    )
                 -- ^ Produce a block
                 -> m (Map NodeId (Chain (Block dom p)))
broadcastNetwork numSlots nodeInit mkBlock = do
    chans <- fmap Map.fromList $ forM nodeIds $ \us -> do
               fmap (us, ) $ fmap Map.fromList $ forM (filter (/= us) nodeIds) $ \them ->
                 fmap (them, ) $
                   createCoupledChannels
                     @(MsgProducer (Block dom p))
                     @MsgConsumer
                     0
                     0

    nodes <- forM (Map.toList nodeInit) $ \(us, (initSt, initChain)) -> do
      varRes <- atomically $ newTVar Nothing
      varSt  <- atomically $ newTVar initSt
      varCPS <- relayNode us initChain $ NodeChannels {
          consumerChans = map (\them -> snd (chans Map.! them Map.! us)) (filter (/= us) nodeIds)
        , producerChans = map (\them -> fst (chans Map.! us Map.! them)) (filter (/= us) nodeIds)
        }

      forM_ [1 .. numSlots] $ \slotId ->
        timer (slotDuration (Proxy @m) * fromIntegral slotId) $ do
          let isLeader = case us of
                RelayId _ -> False
                CoreId i  -> slotId `mod` numNodes == i
          when isLeader $ atomically $ do
            cps <- readTVar varCPS
            st  <- readTVar varSt
            (chain', st') <- runStateT (mkBlock us (toEnum slotId) (chainState cps)) st
            writeTVar varCPS cps{ chainState = chain' }
            writeTVar varSt  st'

      timer (slotDuration (Proxy @m) * fromIntegral (numSlots + 10)) $
        atomically $ do
          cps <- readTVar varCPS
          writeTVar varRes $ Just (us, chainState cps)

      return varRes

    atomically $ Map.fromList <$> collectAllJust nodes
  where
    nodeIds :: [NodeId]
    nodeIds = Map.keys nodeInit

    numNodes :: Int
    numNodes = Map.size nodeInit

slotDuration :: MonadTimer m => proxy m -> Duration (Time m)
slotDuration _ = 1000000

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

collectAllJust :: MonadSTM m stm => [TVar m (Maybe a)] -> stm [a]
collectAllJust = mapM collectJust

collectJust :: MonadSTM m stm => TVar m (Maybe a) -> stm a
collectJust var = do
    ma <- readTVar var
    case ma of
      Nothing -> retry
      Just a  -> return a

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)
