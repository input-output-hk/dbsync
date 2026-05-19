-- | Monotonic ID counter for pre-assigning database primary keys.
--
-- During 'IngestChainHistory', all IDs are assigned in-process using
-- monotonic counters. Since we are the sole writer, every ID is deterministic.
-- PostgreSQL sequences are not used until 'FollowingChainTip'.
module DbSync.Phase.Ingest.Counter
  ( -- * Types
    IdCounter (..)
  , IdCounters (..)

    -- * Construction
  , mkIdCounter
  , freshIdCounters

    -- * Operations
  , nextId
  , currentId
  , resetTo
  ) where

import Cardano.Prelude

-- * Types

-- | Monotonic counter for assigning database IDs.
-- Pure value — no IO, no STM. Threaded through extraction functions.
data IdCounter = IdCounter
  { icNext :: !Int64  -- ^ The next ID to assign
  }
  deriving stock (Eq, Show)

-- | All ID counters used during 'IngestChainHistory'.
-- Each counter tracks the next ID for its table/entity.
data IdCounters = IdCounters
  { icBlockId            :: !IdCounter
  , icTxId               :: !IdCounter
  , icTxOutId            :: !IdCounter
  , icTxInId             :: !IdCounter
  , icCollateralTxInId   :: !IdCounter
  , icReferenceTxInId    :: !IdCounter
  , icTxMetadataId       :: !IdCounter
  , icMaTxMintId         :: !IdCounter
  , icMaTxOutId          :: !IdCounter
  , icSlotLeaderId       :: !IdCounter
  , icAddressId          :: !IdCounter
  , icStakeAddressId     :: !IdCounter
  , icPoolHashId         :: !IdCounter
  , icMultiAssetId       :: !IdCounter
  , icScriptId              :: !IdCounter
  , icStakeRegistrationId   :: !IdCounter
  , icStakeDeregistrationId :: !IdCounter
  , icDelegationId          :: !IdCounter
  , icWithdrawalId          :: !IdCounter
  , icPoolUpdateId          :: !IdCounter
  , icPoolMetadataRefId     :: !IdCounter
  , icPoolOwnerId           :: !IdCounter
  , icPoolRetireId          :: !IdCounter
  , icPoolRelayId           :: !IdCounter
  , icTxCborId              :: !IdCounter
  , icEpochSyncStatsId      :: !IdCounter
  , icAdaPotsId             :: !IdCounter
  , icCollateralTxOutId     :: !IdCounter
  }
  deriving stock (Eq, Show)

-- * Construction

-- | Create a new counter starting from a given value.
mkIdCounter :: Int64 -> IdCounter
mkIdCounter = IdCounter

-- | Every counter seeded at 1 — the production startup state, also
-- the fixture used by tests that don't resume from a checkpoint.
freshIdCounters :: IdCounters
freshIdCounters = IdCounters
  { icBlockId               = mkIdCounter 1
  , icTxId                  = mkIdCounter 1
  , icTxOutId               = mkIdCounter 1
  , icTxInId                = mkIdCounter 1
  , icCollateralTxInId      = mkIdCounter 1
  , icReferenceTxInId       = mkIdCounter 1
  , icTxMetadataId          = mkIdCounter 1
  , icMaTxMintId            = mkIdCounter 1
  , icMaTxOutId             = mkIdCounter 1
  , icSlotLeaderId          = mkIdCounter 1
  , icAddressId             = mkIdCounter 1
  , icStakeAddressId        = mkIdCounter 1
  , icPoolHashId            = mkIdCounter 1
  , icMultiAssetId          = mkIdCounter 1
  , icScriptId              = mkIdCounter 1
  , icStakeRegistrationId   = mkIdCounter 1
  , icStakeDeregistrationId = mkIdCounter 1
  , icDelegationId          = mkIdCounter 1
  , icWithdrawalId          = mkIdCounter 1
  , icPoolUpdateId          = mkIdCounter 1
  , icPoolMetadataRefId     = mkIdCounter 1
  , icPoolOwnerId           = mkIdCounter 1
  , icPoolRetireId          = mkIdCounter 1
  , icPoolRelayId           = mkIdCounter 1
  , icTxCborId              = mkIdCounter 1
  , icEpochSyncStatsId      = mkIdCounter 1
  , icAdaPotsId             = mkIdCounter 1
  , icCollateralTxOutId     = mkIdCounter 1
  }

-- * Operations

-- | Assign the next ID and return the updated counter.
nextId :: IdCounter -> (Int64, IdCounter)
nextId (IdCounter n) = (n, IdCounter (n + 1))

-- | Return the current (next-to-be-assigned) ID without consuming it.
currentId :: IdCounter -> Int64
currentId = icNext

-- | Reset the counter to start from a specific value.
-- Used when restoring from a checkpoint.
resetTo :: Int64 -> IdCounter
resetTo = IdCounter
