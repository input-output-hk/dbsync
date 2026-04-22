-- | Newtype wrappers for database primary keys.
--
-- Ported from @Cardano.Db.Schema.Ids@ in the original cardano-db-sync.
-- Each table's primary key gets its own newtype around 'Int64', providing
-- type safety so that a 'BlockId' cannot accidentally be used where a
-- 'TxId' is expected.
module DbSync.Db.Schema.Ids
  ( -- * Core table IDs
    BlockId (..)
  , TxId (..)
  , SlotLeaderId (..)

    -- * UTxO table IDs
  , TxOutId (..)
  , TxInId (..)
  , CollateralTxInId (..)
  , ReferenceTxInId (..)

    -- * Metadata table IDs
  , TxMetadataId (..)

    -- * MultiAsset table IDs
  , MultiAssetId (..)
  , MaTxMintId (..)
  , MaTxOutId (..)

    -- * StakeDelegation table IDs
  , StakeRegistrationId (..)
  , StakeDeregistrationId (..)
  , DelegationId (..)
  , WithdrawalId (..)

    -- * Pool table IDs
  , PoolUpdateId (..)
  , PoolMetadataRefId (..)
  , PoolOwnerId (..)
  , PoolRetireId (..)
  , PoolRelayId (..)

    -- * CBOR table IDs
  , TxCborId (..)

    -- * EpochSyncStats table IDs
  , EpochSyncStatsId (..)

    -- * Referenced by other tables
  , PoolHashId (..)
  , StakeAddressId (..)
  , DatumId (..)
  , ScriptId (..)
  , RedeemerId (..)
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Core table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @block@ table.
newtype BlockId = BlockId { getBlockId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @tx@ table.
newtype TxId = TxId { getTxId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @slot_leader@ table.
newtype SlotLeaderId = SlotLeaderId { getSlotLeaderId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * UTxO table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @tx_out@ table.
newtype TxOutId = TxOutId { getTxOutId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @tx_in@ table.
newtype TxInId = TxInId { getTxInId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @collateral_tx_in@ table.
newtype CollateralTxInId = CollateralTxInId { getCollateralTxInId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @reference_tx_in@ table.
newtype ReferenceTxInId = ReferenceTxInId { getReferenceTxInId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Metadata table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @tx_metadata@ table.
newtype TxMetadataId = TxMetadataId { getTxMetadataId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * MultiAsset table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @multi_asset@ table.
newtype MultiAssetId = MultiAssetId { getMultiAssetId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @ma_tx_mint@ table.
newtype MaTxMintId = MaTxMintId { getMaTxMintId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @ma_tx_out@ table.
newtype MaTxOutId = MaTxOutId { getMaTxOutId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * StakeDelegation table IDs
-- ---------------------------------------------------------------------------

newtype StakeRegistrationId = StakeRegistrationId { getStakeRegistrationId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype StakeDeregistrationId = StakeDeregistrationId { getStakeDeregistrationId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype DelegationId = DelegationId { getDelegationId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype WithdrawalId = WithdrawalId { getWithdrawalId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Pool table IDs
-- ---------------------------------------------------------------------------

newtype PoolUpdateId = PoolUpdateId { getPoolUpdateId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype PoolMetadataRefId = PoolMetadataRefId { getPoolMetadataRefId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype PoolOwnerId = PoolOwnerId { getPoolOwnerId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype PoolRetireId = PoolRetireId { getPoolRetireId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype PoolRelayId = PoolRelayId { getPoolRelayId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * CBOR table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @tx_cbor@ table.
newtype TxCborId = TxCborId { getTxCborId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * EpochSyncStats table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @epoch_sync_stats@ table.
newtype EpochSyncStatsId = EpochSyncStatsId { getEpochSyncStatsId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Referenced by other tables (owned by future extractors)
-- ---------------------------------------------------------------------------

-- | Primary key for the @pool_hash@ table.
-- Referenced by 'SlotLeader.slotLeaderPoolHashId'.
newtype PoolHashId = PoolHashId { getPoolHashId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @stake_address@ table.
-- Referenced by @tx_out.stake_address_id@.
newtype StakeAddressId = StakeAddressId { getStakeAddressId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @datum@ table.
-- Referenced by @tx_out.inline_datum_id@.
newtype DatumId = DatumId { getDatumId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @script@ table.
-- Referenced by @tx_out.reference_script_id@.
newtype ScriptId = ScriptId { getScriptId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @redeemer@ table.
-- Referenced by @tx_in.redeemer_id@.
newtype RedeemerId = RedeemerId { getRedeemerId :: Int64 }
  deriving stock (Eq, Ord, Show)
