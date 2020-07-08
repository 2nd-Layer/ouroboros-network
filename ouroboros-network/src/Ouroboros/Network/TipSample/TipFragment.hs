{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

-- @Measured TipMeasure (Tip block)@ is an orphaned instance.
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.TipSample.TipFragment where

import           Control.Exception (assert)
import           Control.Monad.Class.MonadTime (Time)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import           Data.FingerTree.Strict (Measured (..), StrictFingerTree)
import qualified Data.FingerTree.Strict as FT
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Network.Block (HasHeader (..), Tip (..))
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

--
-- This is like `ChainFragment` but it does not do some of its assertions.
--

data TipMeasure = TipMeasure {
    tmMinSlot :: !(WithOrigin SlotNo),
    tmMaxSlot :: !(WithOrigin SlotNo),
    tmSize    :: !Int
  }

instance Semigroup TipMeasure where
  vl <> vr =
    TipMeasure (min (tmMinSlot vl) (tmMinSlot vr))
               (max (tmMaxSlot vl) (tmMaxSlot vr))
               (tmSize vl + tmSize vr)

instance Monoid TipMeasure where
  mempty = TipMeasure (At maxBound) Origin 0
  mappend = (<>)

instance Measured TipMeasure (Tip block) where
  measure (Tip slotNo _ _blockNo) = TipMeasure (At slotNo) (At slotNo) 1
  measure TipGenesis = TipMeasure Origin Origin 1


data Timed a = Timed {
    timedAt   :: !Time,
    timedData :: !a
  }

-- TODO: these `Eq` and `Ord` instances are not quite right.
instance Eq (Timed a) where
    a == b = timedAt a == timedAt b

instance Ord (Timed a) where
    a `compare` b = timedAt a `compare` timedAt b

instance Measured TipMeasure a => Measured TipMeasure (Timed a) where
    measure = measure . timedData


newtype TipFragment' tip = TipFragment (StrictFingerTree TipMeasure tip)

type TipFragment block = TipFragment' (Timed (Tip block))

viewRight :: Measured TipMeasure tip => TipFragment' tip -> FT.ViewR TipFragment' tip
viewRight (TipFragment c) = case FT.viewr c of
  FT.EmptyR  -> FT.EmptyR
  c' FT.:> tip -> TipFragment c' FT.:> tip

viewLeft :: Measured TipMeasure tip => TipFragment' tip -> FT.ViewL TipFragment' tip
viewLeft (TipFragment c) = case FT.viewl c of
  FT.EmptyL  -> FT.EmptyL
  tip FT.:< c' -> tip FT.:< TipFragment c'

pattern Empty :: Measured TipMeasure tip => TipFragment' tip
pattern Empty <- (viewRight -> FT.EmptyR) where
  Empty = TipFragment FT.empty

-- | \( O(1) \). Add a tip to the right of the chain fragment.
pattern (:>) :: Measured TipMeasure tip
             => Measured TipMeasure tip
             => TipFragment' tip -> tip -> TipFragment' tip
pattern c :> tip <- (viewRight -> (c FT.:> tip)) where
  TipFragment c :> tip = TipFragment (c FT.|> tip)

-- | \( O(1) \). Add a tip to the left of the chain fragment.
pattern (:<) :: Measured TipMeasure tip
             => Measured TipMeasure tip
             => tip -> TipFragment' tip -> TipFragment' tip
pattern b :< c <- (viewLeft -> (b FT.:< c)) where
  b :< TipFragment c = TipFragment (b FT.<| c)

infixl 5 :>, :<

{-# COMPLETE Empty, (:>) #-}
{-# COMPLETE Empty, (:<) #-}

lookupBySlotFT :: WithOrigin SlotNo
               -> TipFragment block
               -> FT.SearchResult TipMeasure (Timed (Tip block))
lookupBySlotFT slotNo (TipFragment c) =
    FT.search (\vl vr -> tmMaxSlot vl >= slotNo && tmMinSlot vr > slotNo) c


-- | \( O(n) \). Drop the newest blocks that satisfy the predicate, keeping
-- the remainder.
--
dropWhileNewest :: Measured TipMeasure tip
                => (tip -> Bool)
                -> TipFragment' tip
                -> TipFragment' tip
dropWhileNewest _ Empty       = Empty
dropWhileNewest p c@(c' :> b)
                  | p b       = dropWhileNewest p c'
                  | otherwise = c


-- | Drop all newest @'Tip' block@ which 'SlotNo' is greater or equal to the
-- given one.
--
dropNewestUntilSlotNo :: SlotNo
                      -> TipFragment block
                      -> TipFragment block
dropNewestUntilSlotNo slotNo = dropWhileNewest $ \Timed { timedData } ->
    case timedData of
      Tip slotNo' _ _ -> slotNo' >= slotNo
      TipGenesis      -> False


-- | Drop all @'Tip' block@ which 'SlotNo' is smaller than the given one.
--
dropUntilSlotNo :: SlotNo
                -> TipFragment block
                -> TipFragment block
dropUntilSlotNo slotNo (TipFragment c) =
    TipFragment $ FT.dropUntil (\v -> tmMinSlot v >= At slotNo) c


--
-- Operations needed by the client application.
-- TODO: move to appropriate palce
--


-- | Takes an 'AnchoredFragment' and a 'TipFragment' which is validated against
-- it.
--
validateTipFragment
  :: forall block.
     HasHeader block
  => AnchoredFragment block
  -> TipFragment block
  -> TipFragment block
validateTipFragment af tf0 =
      case AF.anchor af of
        AF.Anchor slotNo _ _ ->
          go (AF.unanchorFragment af) (dropUntilSlotNo (succ slotNo) tf0)
        AF.AnchorGenesis ->
          go (AF.unanchorFragment af) tf0
    where
      -- a structural fold over both fragments at the same time
      go :: ChainFragment block
         -> TipFragment block
         -> TipFragment block

      go cf@(b CF.:< cf') tf@(tt@(Timed _ (Tip slotNo headerHash blockNo_)) :< tf')

        | blockSlot b == slotNo && blockHash b == headerHash && blockNo b == blockNo_
        -- 'tt' is valid
        = tt :< go cf' tf'

        | blockSlot b == slotNo
        -- 'tt' is not valid but it was at the right slot
        = go cf' tf'

        | blockSlot b < slotNo
        -- 'tt' might be valid; 'tf' is ahead of 'cf'
        = go cf' tf

        | otherwise -- blockSlot b > slotNo
        -- 'cf' is ahead of 'tf'; this should never happen
        = assert False (go cf tf')

      go cf (tt@(Timed _ TipGenesis) :< tf')
        = tt :< go cf tf'

      go _ Empty
        = Empty

      go CF.Empty _
        = Empty



-- | Find winning peers; Traverse each 'TipFragment', for every slot find
-- a winning peer. Accumulate results for all slots (higher than the given
-- 'SlotNo').
--
winingPeers
    :: forall block peerAddr.
       Ord peerAddr
    => SlotNo
    -- ^ initial slot from which we should start analyzgin 'TipFragment's.
    -> [(TipFragment block, peerAddr)]
    -- ^ peer 'TipFragment'; it should only contain 'Tip block' which were
    -- included in our chain (and thus are valid).
    -> Map peerAddr Int
winingPeers = go Map.empty
  where
    -- TODO: maybe use a heap, instead of `Map`
    go :: Map peerAddr Int
       -> SlotNo
       -> [(TipFragment block, peerAddr)]
       -> Map peerAddr Int
    go !acc _curSlotNo [] = acc
    go !acc !curSlotNo row =
      case takeRow curSlotNo row of
        ( Just peerAddr, row' ) ->
          go (Map.alter (Just . maybe 1 succ) peerAddr acc)
             (succ curSlotNo)
             row'
        ( Nothing, row' ) ->
          go acc
             (succ curSlotNo)
             row'

    -- Find the winning peer in a given slot; return the winner and truncated
    -- 'TipFragment's.
    takeRow :: SlotNo
            -> [(TipFragment block, peerAddr)]
            -> ( Maybe peerAddr
               , [(TipFragment block, peerAddr)] )
    takeRow curSlotNo = goTakeRow Nothing []
      where
        goTakeRow :: Maybe (Timed (Tip block), peerAddr)
                  -- ^ accumulator of found 'Tip's
                  -> [(TipFragment block, peerAddr)]
                  -- ^ accumulator of 'TipFragment's
                  -> [(TipFragment block, peerAddr)]
                  -> ( Maybe peerAddr
                     , [(TipFragment block, peerAddr)] )
        goTakeRow !mtt !acc [] = (snd <$> mtt, acc)
        goTakeRow !mtt !acc ((tf, peerAddr) : as) =
          case lookupBySlotFT (At curSlotNo) tf of
            FT.Position _ a r ->
              goTakeRow
                (case mtt of
                  Just !tt -> Just (tt `min` (a, peerAddr))
                  Nothing  -> Just (a, peerAddr))
                ((TipFragment r, peerAddr) : acc)
                as
            FT.OnLeft  -> goTakeRow mtt acc as
            FT.OnRight -> goTakeRow mtt acc as
            FT.Nowhere -> goTakeRow mtt acc as
