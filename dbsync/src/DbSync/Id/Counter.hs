-- | Monotonic ID counter for pre-assigning database primary keys.
--
-- During 'IngestChainHistory', all IDs are assigned in-process using
-- monotonic counters. Since we are the sole writer, every ID is deterministic.
-- PostgreSQL sequences are not used until 'FollowingChainTip'.
module DbSync.Id.Counter
  ( -- * Types
    IdCounter (..)
  , IdCounters (..)

    -- * Construction
  , mkIdCounter

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
  }
  deriving stock (Eq, Show)

-- * Construction

-- | Create a new counter starting from a given value.
mkIdCounter :: Int64 -> IdCounter
mkIdCounter = IdCounter

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
