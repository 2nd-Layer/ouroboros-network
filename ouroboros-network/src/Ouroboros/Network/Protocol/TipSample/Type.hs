{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Network.Protocol.TipSample.Type where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined (N (..), Nat)
import Cardano.Slotting.Slot (SlotNo)
import Data.Typeable (Typeable)

import Ouroboros.Network.Util.ShowProxy


-- | There are three of blocking requests: awiat until slot, await until
-- a tip will change; the third one has a dedicated type:
-- @'StFollowTip' :: 'TipSample' tip@.
--
data TipRequestKind where
    BlockUntilSlot    :: TipRequestKind
    BlockUntilTip     :: TipRequestKind


data TokTipRequest (tipRequestKind :: TipRequestKind) where
    TokBlockUntilSlot    :: TokTipRequest BlockUntilSlot
    TokBlockUntilTip     :: TokTipRequest BlockUntilTip

deriving instance Show (TokTipRequest tipRequestKind)


-- | The protocol starts with an initial request for the current tip.
--
data InitTipSample where
    StClientRequest  :: InitTipSample
    StServerResponse :: InitTipSample


-- | `tip-sample` protocol, desigined for established peers to sample tip from
-- upstream peers.
--
data TipSample tip where
    StInit      :: InitTipSample -> TipSample tip
    StIdle      :: TipSample tip
    StBusy      :: TipRequestKind -> TipSample tip
    StFollowTip :: N -> TipSample tip
    StDone      :: TipSample tip

instance ShowProxy (TipSample tip) where
    showProxy _ = "TipSample"

instance Protocol (TipSample tip) where
    data Message (TipSample tip) from to where

      -- | Request current tip.  It must not block.
      --
      MsgGetCurrentTip :: Message (TipSample tip)
                                  (StInit StClientRequest)
                                  (StInit StServerResponse)

      -- | Server response with its current tip.
      --
      MsgCurrentTip :: tip -> Message (TipSample tip)
                                      (StInit StServerResponse)
                                      StIdle


      -- | Get first tip after given slot.  It can block.
      --
      MsgGetTipAfterSlotNo :: SlotNo -> Message (TipSample tip)
                                                StIdle
                                                (StBusy BlockUntilSlot)

      -- | Get next tip afetr the given one.  It can block.
      --
      MsgGetTipAfterTip :: tip -> Message (TipSample tip)
                                          StIdle
                                          (StBusy BlockUntilTip)

      -- | Response to 'MsgGetTipAfterSlotNo' or 'MsgGetTipChange'.
      --
      -- Note: 'Typeable' constraint is only needed by 'codecTipSampleId'.  We
      -- are using the same message for two state transitions and we use
      -- 'Typeable' to recover the 'TipRequestKind' from an existential type.
      MsgTip :: forall (any :: TipRequestKind) tip. Typeable any
             => tip
             -> Message (TipSample tip)
                        (StBusy any)
                        StIdle

      -- | Send next tip; The server must send record last send tip and if
      -- received this message must reply as soon as its tip has changed.  The
      -- next send tip might not be the next block on the blockchain, especially
      -- when the node switched to another fork.  One can only ask for
      -- a positive number of tips (someimtes it would be more elegant if
      -- natural numbers started from `1`).
      --
      MsgFollowTip :: Nat (S n)
                   -> Message (TipSample tip)
                               StIdle
                               (StFollowTip (S n))

      -- | Send a tip back to the client, hold on the agency.
      --
      MsgNextTip :: tip
                 -> Message (TipSample tip)
                            (StFollowTip (S (S n)))
                            (StFollowTip (S n))

      -- | Send last tip and pass the agency to the client.
      --
      MsgNextTipDone :: tip
                     -> Message (TipSample tip)
                                (StFollowTip (S Z))
                                StIdle

      -- | Terminating message (client side).
      --
      MsgDone :: Message (TipSample tip)
                         StIdle
                         StDone

    data ClientHasAgency st where
      TokInitClient :: ClientHasAgency (StInit StClientRequest)
      TokIdle       :: ClientHasAgency StIdle

    data ServerHasAgency st where
      TokInitServer  :: ServerHasAgency (StInit StServerResponse)

      TokBusy        :: TokTipRequest tipRequestKind
                     -> ServerHasAgency (StBusy tipRequestKind)

      TokFollowTip   :: Nat (S n) -> ServerHasAgency (StFollowTip (S n))

    data NobodyHasAgency st where
      TokDone :: NobodyHasAgency StDone

    exclusionLemma_ClientAndServerHaveAgency TokInitClient tok =
      case tok of {}
    exclusionLemma_ClientAndServerHaveAgency TokIdle tok =
      case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone tok =
      case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone tok =
      case tok of {}


--
-- Show instances
--

deriving instance Show tip => Show (Message (TipSample tip) from to)
deriving instance Show (ClientHasAgency (st :: TipSample tip))
deriving instance Show (ServerHasAgency (st :: TipSample tip))
deriving instance Show (NobodyHasAgency (st :: TipSample tip))
