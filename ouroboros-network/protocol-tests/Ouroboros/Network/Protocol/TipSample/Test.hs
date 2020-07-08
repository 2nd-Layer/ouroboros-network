{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.TipSample.Test where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import qualified Control.Monad.ST as ST
import           Control.Tracer (nullTracer)

import           Data.ByteString.Lazy (ByteString)
import           Data.List (cycle)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Functor.Identity (Identity (..))
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise (Serialise (..), DeserialiseFailure)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IOSim (runSimOrThrow)
import           Network.TypedProtocol.Pipelined (natToInt)
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver

import           Ouroboros.Network.Protocol.TipSample.Type
import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Server
import           Ouroboros.Network.Protocol.TipSample.Direct
import           Ouroboros.Network.Protocol.TipSample.Examples
import           Ouroboros.Network.Protocol.TipSample.Codec

import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

instance Arbitrary SlotNo where
    arbitrary = SlotNo <$> arbitrary
    shrink (SlotNo a) = SlotNo `map` shrink a


tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.TipSampleProtocol"
  [ testProperty "direct"  propSampleDirect
  , testProperty "connect" propSampleConnect
  , testProperty "codec"   prop_codec_TipSample
  , testProperty "codec 2-splits" prop_codec_splits2_TipSample
  , testProperty "codec 3-splits" $ withMaxSuccess 30 prop_codec_splits3_TipSample
  , testProperty "demo ST" propTipSampleDemoST
  , testProperty "demo IO" propTipSampleDemoIO
  ]

--
-- Pure tests using either 'direct', 'directPipelined', 'connect' or
-- 'connectPipelined'.
--

tipSampleExperiment
  :: ( Eq tip
     , Show tip
     , Monad m
     )
  => (forall a b. TipSampleClient tip m a -> TipSampleServer tip m b -> m (a, b))
  -> [Request tip]
  -> tip
  -> NonEmpty tip
  -> m Property
tipSampleExperiment run reqs tip resps = do
    (resps', reqs') <-
      tipSampleClientExample reqs
      `run`
      tipSampleServerExample tip resps
    pure $
           counterexample "requests"  (reqs'  === reqs)
      .&&. counterexample "responses" (resps' === pureClient reqs tip resps)


pureClient :: [Request tip] -> tip ->  NonEmpty tip -> [tip]
pureClient reqs tip resps = tip : go reqs (cycle (NonEmpty.toList resps))
  where
    go :: [Request tip] -> [tip] -> [tip]
    go [] _ = []
    go (RequestTipAfterSlotNo _ : rs) (a : as) = a : go rs as
    go (RequestTipAfterTip _ : rs) (a : as) = a : go rs as
    go (RequestFollowTips n : rs) as =
      case splitAt (natToInt n) as of
        (bs, as') -> bs ++ go rs as'
    go _ [] = error "tipSampleExperiment: impossible happened"


propSampleDirect :: [Request Int]
                 -> Int
                 -> NonEmptyList Int
                 -> Property
propSampleDirect reqs tip (NonEmpty resps) =
    runIdentity $ tipSampleExperiment direct reqs tip (NonEmpty.fromList resps)


propSampleConnect :: [Request Int]
                  -> Int
                  -> NonEmptyList Int
                  -> Property
propSampleConnect reqs tip (NonEmpty resps) =
    runIdentity $
      tipSampleExperiment
        (\client server -> do
          (a, b, TerminalStates TokDone TokDone) <- 
            tipSampleClientPeer client
            `connect`
            tipSampleServerPeer server
          pure (a, b))
        reqs tip (NonEmpty.fromList resps)

--
-- Codec tests
--

instance Eq tip => Eq (AnyMessage (TipSample tip)) where
    AnyMessage MsgGetCurrentTip
      == AnyMessage MsgGetCurrentTip = True
    AnyMessage (MsgCurrentTip tip)
      == AnyMessage (MsgCurrentTip tip') = tip == tip'
    AnyMessage (MsgGetTipAfterSlotNo slotNo)
      == AnyMessage (MsgGetTipAfterSlotNo slotNo') = slotNo == slotNo'
    AnyMessage (MsgGetTipAfterTip tip)
      == AnyMessage (MsgGetTipAfterTip tip') = tip == tip'
    AnyMessage (MsgTip tip)
      == AnyMessage (MsgTip tip') = tip == tip'
    _ == _ = False

instance Eq tip => Eq (AnyMessageAndAgency (TipSample tip)) where
    AnyMessageAndAgency _ MsgGetCurrentTip
      == AnyMessageAndAgency _ MsgGetCurrentTip = True
    AnyMessageAndAgency _ (MsgCurrentTip tip)
      == AnyMessageAndAgency _ (MsgCurrentTip tip') = tip == tip'
    AnyMessageAndAgency _ (MsgGetTipAfterSlotNo slotNo)
      == AnyMessageAndAgency _ (MsgGetTipAfterSlotNo slotNo') = slotNo == slotNo'
    AnyMessageAndAgency _ (MsgGetTipAfterTip tip)
      == AnyMessageAndAgency _ (MsgGetTipAfterTip tip') = tip == tip'
    AnyMessageAndAgency (ServerAgency (TokBusy TokBlockUntilSlot)) (MsgTip tip)
      == AnyMessageAndAgency (ServerAgency (TokBusy TokBlockUntilSlot)) (MsgTip tip')
        = tip == tip'
    AnyMessageAndAgency (ServerAgency (TokBusy TokBlockUntilTip)) (MsgTip tip)
      == AnyMessageAndAgency (ServerAgency (TokBusy TokBlockUntilTip)) (MsgTip tip')
        = tip == tip'
    _ == _ = False

instance Show tip => Show (AnyMessageAndAgency (TipSample tip)) where
    show (AnyMessageAndAgency agency msg) =
      concat
        ["AnnyMessageAndAgency "
        , show agency
        , " "
        , show msg
        ]

instance Arbitrary tip => Arbitrary (AnyMessageAndAgency (TipSample tip)) where
    arbitrary = oneof
      [ pure $ AnyMessageAndAgency (ClientAgency TokInitClient) MsgGetCurrentTip
      , arbitrary >>= \tip ->
          pure $ AnyMessageAndAgency (ServerAgency TokInitServer) (MsgCurrentTip tip)
      , arbitrary >>= \slotNo ->
          pure $ AnyMessageAndAgency (ClientAgency TokIdle) (MsgGetTipAfterSlotNo slotNo)
      , arbitrary >>= \tip ->
          pure $ AnyMessageAndAgency (ClientAgency TokIdle) (MsgGetTipAfterTip tip)
      , arbitrary >>= \tip ->
          pure $ AnyMessageAndAgency (ServerAgency (TokBusy TokBlockUntilSlot)) (MsgTip tip)
      , arbitrary >>= \tip ->
          pure $ AnyMessageAndAgency (ServerAgency (TokBusy TokBlockUntilTip)) (MsgTip tip)
      ]

codec :: ( MonadST m
         , Serialise tip
         )
      => Codec (TipSample tip)
               Serialise.DeserialiseFailure
               m ByteString
codec = codecTipSample Serialise.encode Serialise.decode

prop_codec_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codec_TipSample msg =
    ST.runST $ prop_codecM codec msg

prop_codec_splits2_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codec_splits2_TipSample msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codec
      msg

prop_codec_splits3_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codec_splits3_TipSample msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codec
      msg

--
-- Network demos
--

tipSampleDemo
  :: forall tip m.
     ( MonadST    m
     , MonadSTM   m
     , MonadAsync m
     , MonadThrow m
     , Serialise tip
     , Eq        tip
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> [Request tip]
  -> tip
  -> NonEmpty tip
  -> m Bool
tipSampleDemo clientChan serverChan reqs tip resps = do
  let client :: TipSampleClient tip m [tip]
      client = tipSampleClientExample reqs

      server :: TipSampleServer tip m [Request tip]
      server = tipSampleServerExample tip resps

  ((reqs', serBS), (resps', cliBS)) <-
    runPeer nullTracer codec serverChan (tipSampleServerPeer server)
    `concurrently`
    runPeer nullTracer codec clientChan (tipSampleClientPeer client)
  
  pure $ reqs   == reqs'
      && resps' == pureClient reqs tip resps
      && serBS  == Nothing
      && cliBS  == Nothing


propTipSampleDemoST
  :: [Request Int]
  -> Int
  -> NonEmptyList Int
  -> Bool
propTipSampleDemoST reqs tip (NonEmpty resps) =
  runSimOrThrow $ do
    (clientChan, serverChan) <- createConnectedChannels
    tipSampleDemo clientChan serverChan reqs tip (NonEmpty.fromList resps)


propTipSampleDemoIO
  :: [Request Int]
  -> Int
  -> NonEmptyList Int
  -> Property
propTipSampleDemoIO reqs tip (NonEmpty resps) =
  ioProperty $ do
    (clientChan, serverChan) <- createConnectedChannels
    tipSampleDemo clientChan serverChan reqs tip (NonEmpty.fromList resps)
