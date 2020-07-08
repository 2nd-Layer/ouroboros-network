{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TipSample.Examples where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero),
                   natToInt, unsafeIntToNat)

import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Server

import           Test.QuickCheck


data Request tip
    = RequestTipAfterSlotNo SlotNo
    | RequestTipAfterTip tip
    | forall (n :: N). RequestFollowTips (Nat (S n))


instance Show tip => Show (Request tip) where
    show (RequestTipAfterSlotNo slotNo) = "RequestTipAfterSlotNo " ++ show slotNo
    show (RequestTipAfterTip tip) = "RequestTipAfterTip " ++ show tip
    show (RequestFollowTips n) = "RequestFollowTips " ++ show (natToInt n)

instance Eq tip => Eq (Request tip) where
    RequestTipAfterSlotNo slotNo == RequestTipAfterSlotNo slotNo' = slotNo == slotNo'
    RequestTipAfterTip tip == RequestTipAfterTip tip' = tip == tip'
    RequestFollowTips n == RequestFollowTips n' = natToInt n == natToInt n'
    _ == _ = False

instance Arbitrary tip => Arbitrary (Request tip) where
    arbitrary = oneof
      [ RequestTipAfterSlotNo . SlotNo <$> arbitrary
      , RequestTipAfterTip <$> arbitrary
      , RequestFollowTips . unsafeIntToNat . getPositive <$> arbitrary
      ]

-- | Given a list of requests record all the responses.
--
tipSampleClientExample :: forall tip m. Applicative m
                       => [Request tip]
                       -> TipSampleClient tip m [tip]
tipSampleClientExample reqs =
    TipSampleClient $ pure (\tip -> pure $ goIdle [tip] reqs)
  where
    goIdle
      :: [tip]
      -> [Request tip]
      -> ClientStIdle tip m [tip]
    goIdle !acc [] =
      SendMsgDone (reverse acc)
    goIdle !acc (RequestTipAfterSlotNo slotNo : as) =
      SendMsgGetTipAfterSlotNo slotNo $ \tip -> pure (goIdle (tip : acc) as)
    goIdle !acc (RequestTipAfterTip a : as) =
      SendMsgGetTipAfterTip a $ \tip -> pure (goIdle (tip : acc) as)
    goIdle !acc (RequestFollowTips n : as) = SendMsgFollowTip n (goFollowTips acc as n)

    goFollowTips
      :: [tip]
      -> [Request tip]
      -> Nat (S n)
      -> HandleTips (S n) tip m [tip]
    goFollowTips !acc as (Succ p@(Succ _)) =
      (ReceiveTip $ \tip -> pure $ goFollowTips (tip : acc) as p)
    goFollowTips !acc as (Succ Zero) =
      (ReceiveLastTip $ \tip -> pure $ goIdle (tip : acc) as)



-- | A server which sends replies from a list (used cyclicly) and returns all
-- requests.
--
tipSampleServerExample :: forall tip m. Applicative m
                       => tip
                       -> NonEmpty tip
                       -> TipSampleServer tip m [Request tip]
tipSampleServerExample tip tips =
    TipSampleServer $ pure (tip, go [] tiplist)
  where
    tiplist = cycle $ NonEmpty.toList tips

    go :: [Request tip]
       -> [tip]
       -> ServerStIdle tip m [Request tip]
    go _acc [] = error "tipSampleServerExample: impossible happened"
    go !acc as@(a : as') =
      ServerStIdle {
          handleTipAfterSlotNo = \req -> pure (a, go (RequestTipAfterSlotNo  req : acc) as'),
          handleTipChange      = \req -> pure (a, go (RequestTipAfterTip req : acc) as'),
          handleFollowTip      = \n   -> goFollowTip n (RequestFollowTips n : acc) as,
          handleDone           = pure $ reverse acc
        }

    goFollowTip :: Nat (S n)
                -> [Request tip]
                -> [tip]
                -> SendTips (S n) tip m [Request tip]
    goFollowTip n@(Succ Zero)       !acc (a : as) =
      SendLastTip n (pure (a, go acc as))
    goFollowTip n@(Succ p@(Succ _)) !acc (a : as) =
      SendNextTip n (pure (a, goFollowTip p acc as))
    goFollowTip _ _ [] =
      error "tipSampleServerExample: impossible happened"
