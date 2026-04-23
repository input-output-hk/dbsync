{-# LANGUAGE OverloadedStrings #-}

-- | ID resolution interface for the unified extraction pipeline.
--
-- An 'IdResolver' provides the mechanism for obtaining database IDs
-- during block processing. Two implementations exist:
--
-- * 'DbSync.Resolver.Ingest' — DedupMap\/Counter-based for 'IngestChainHistory'
-- * (future) @DbSync.Resolver.Follow@ — SELECT->INSERT for 'FollowingChainTip'
--
-- Extractors are parameterised by 'IdResolver' so the same extraction
-- logic works in both phases.
module DbSync.Resolver
  ( -- * Types
    IdResolver (..)
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import DbSync.Db.Schema.Core (SlotLeader)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Db.Schema.StakeDelegation (StakeAddress)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | How to obtain database IDs during block processing.
--
-- The @m@ parameter is the effect monad — 'IO' in production,
-- potentially a test monad in tests.
data IdResolver m = IdResolver
  { -- ---------------------------------------------------------------
    -- Core (shared IDs — used by processBlock centrally)
    -- ---------------------------------------------------------------

    -- | Assign the next block ID.
    assignBlockId     :: !(m BlockId)

    -- | Assign the next transaction ID.
  , assignTxId        :: !(m TxId)

    -- | Assign the next transaction output ID.
  , assignTxOutId     :: !(m TxOutId)

    -- | Resolve a slot leader by its hash.
    -- Returns @(SlotLeaderId, isNew)@. When @isNew = True@, the caller
    -- should also write the 'SlotLeader' row via the 'Writer'.
  , resolveSlotLeader :: !(ByteString -> SlotLeader -> m (SlotLeaderId, Bool))

    -- | Look up the previous block's ID by its hash.
  , resolvePrevBlock  :: !(ByteString -> m (Maybe BlockId))

    -- ---------------------------------------------------------------
    -- UTxO extractor IDs
    -- ---------------------------------------------------------------

    -- | Assign the next tx_in ID.
  , assignTxInId           :: !(m TxInId)

    -- | Assign the next collateral_tx_in ID.
  , assignCollateralTxInId :: !(m CollateralTxInId)

    -- | Assign the next reference_tx_in ID.
  , assignReferenceTxInId  :: !(m ReferenceTxInId)

    -- ---------------------------------------------------------------
    -- Metadata extractor IDs
    -- ---------------------------------------------------------------

    -- | Assign the next tx_metadata ID.
  , assignTxMetadataId :: !(m TxMetadataId)

    -- ---------------------------------------------------------------
    -- MultiAsset extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a multi-asset by its (policy ++ name) key.
    -- Key is 'ShortByteString' (unpinned) to avoid pinned ByteString
    -- concatenation in the hot multi-asset lookup path.
    -- Returns @(MultiAssetId, isNew)@.
  , resolveMultiAsset :: !(ShortByteString -> MultiAsset -> m (MultiAssetId, Bool))

    -- | Assign the next ma_tx_mint ID.
  , assignMaTxMintId :: !(m MaTxMintId)

    -- | Assign the next ma_tx_out ID.
  , assignMaTxOutId  :: !(m MaTxOutId)

    -- ---------------------------------------------------------------
    -- StakeDelegation extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a stake address by its credential hash.
    -- Returns @(StakeAddressId, isNew)@.
  , resolveStakeAddress :: !(ByteString -> StakeAddress -> m (StakeAddressId, Bool))

    -- | Assign the next stake_registration ID.
  , assignStakeRegistrationId :: !(m StakeRegistrationId)

    -- | Assign the next stake_deregistration ID.
  , assignStakeDeregistrationId :: !(m StakeDeregistrationId)

    -- | Assign the next delegation ID.
  , assignDelegationId :: !(m DelegationId)

    -- | Assign the next withdrawal ID.
  , assignWithdrawalId :: !(m WithdrawalId)

    -- ---------------------------------------------------------------
    -- Pool extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a pool hash by its key hash.
    -- Returns @(PoolHashId, isNew)@.
  , resolvePoolHash :: !(ByteString -> PoolHash -> m (PoolHashId, Bool))

    -- | Assign the next pool_update ID.
  , assignPoolUpdateId :: !(m PoolUpdateId)

    -- | Assign the next pool_metadata_ref ID.
  , assignPoolMetadataRefId :: !(m PoolMetadataRefId)

    -- | Assign the next pool_owner ID.
  , assignPoolOwnerId :: !(m PoolOwnerId)

    -- | Assign the next pool_retire ID.
  , assignPoolRetireId :: !(m PoolRetireId)

    -- | Assign the next pool_relay ID.
  , assignPoolRelayId :: !(m PoolRelayId)

    -- ---------------------------------------------------------------
    -- CBOR extractor IDs
    -- ---------------------------------------------------------------

    -- | Assign the next tx_cbor ID.
  , assignTxCborId :: !(m TxCborId)

    -- ---------------------------------------------------------------
    -- EpochSyncStats IDs
    -- ---------------------------------------------------------------

    -- | Assign the next epoch_sync_stats ID.
  , assignEpochSyncStatsId :: !(m EpochSyncStatsId)
  }
