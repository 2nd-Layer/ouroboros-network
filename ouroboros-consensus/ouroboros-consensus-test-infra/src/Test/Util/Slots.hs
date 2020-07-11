{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Test.Util.Slots (
  NumSlots (..),
  maxNumSlots,
  ) where

import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))
import           Test.QuickCheck (Arbitrary (..))
import qualified Test.QuickCheck as QC

-- | Number of slots
newtype NumSlots = NumSlots {unNumSlots :: Word64}
  deriving (Eq, Generic)
  deriving (Show) via (Quiet NumSlots)

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

-- TODO: We shouldn't really pick the number of slots independent from k
instance Arbitrary NumSlots where
  arbitrary = NumSlots <$> QC.choose (minNumSlots, maxNumSlots)
  shrink (NumSlots n) = NumSlots <$> (filter (>= minNumSlots) $ shrink n)

minNumSlots :: Word64
minNumSlots = 1

maxNumSlots :: Word64
maxNumSlots = 100
