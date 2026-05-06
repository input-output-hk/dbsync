{-# LANGUAGE OverloadedStrings #-}

-- | Newtype wrappers for database primary keys.
--
-- Each table's primary key gets its own newtype around 'Int64', providing
-- type safety so that a 'BlockId' cannot accidentally be used where a
-- 'TxId' is expected.
module DbSync.Db.Schema.Ids
  ( -- * Hasql encoder \/ decoder helpers
    idDecoder
  , maybeIdDecoder
  , idEncoder
  , maybeIdEncoder

    -- * Core table IDs
  , BlockId (..)
  , TxId (..)
  , SlotLeaderId (..)
  , MetaId (..)
  , EpochSyncTimeId (..)
  , ReverseIndexId (..)

    -- * UTxO table IDs
  , TxOutId (..)
  , TxInId (..)
  , CollateralTxInId (..)
  , ReferenceTxInId (..)
  , CollateralTxOutId (..)

    -- * Address table IDs
  , AddressId (..)

    -- * Metadata table IDs
  , TxMetadataId (..)

    -- * MultiAsset table IDs
  , MultiAssetId (..)
  , MaTxMintId (..)
  , MaTxOutId (..)

    -- * ScriptsDatums table IDs
  , ExtraKeyWitnessId (..)
  , RedeemerDataId (..)

    -- * StakeDelegation table IDs
  , StakeRegistrationId (..)
  , StakeDeregistrationId (..)
  , DelegationId (..)
  , WithdrawalId (..)
  , RewardId (..)
  , RewardRestId (..)
  , EpochStakeId (..)
  , EpochStakeProgressId (..)

    -- * Pool table IDs
  , PoolUpdateId (..)
  , PoolMetadataRefId (..)
  , PoolOwnerId (..)
  , PoolRetireId (..)
  , PoolRelayId (..)
  , PoolStatId (..)
  , DelistedPoolId (..)
  , ReservedPoolTickerId (..)

    -- * CBOR table IDs
  , TxCborId (..)

    -- * Governance table IDs
  , DrepHashId (..)
  , DrepRegistrationId (..)
  , DrepDistrId (..)
  , DelegationVoteId (..)
  , GovActionProposalId (..)
  , VotingProcedureId (..)
  , VotingAnchorId (..)
  , ConstitutionId (..)
  , CommitteeId (..)
  , CommitteeHashId (..)
  , CommitteeMemberId (..)
  , CommitteeRegistrationId (..)
  , CommitteeDeRegistrationId (..)
  , ParamProposalId (..)
  , TreasuryWithdrawalId (..)
  , EventInfoId (..)

    -- * EpochSyncStats table IDs
  , EpochSyncStatsId (..)

    -- * EpochBoundary table IDs
  , AdaPotsId (..)
  , EpochId (..)
  , EpochParamId (..)
  , EpochStateId (..)
  , CostModelId (..)
  , PotTransferId (..)
  , TreasuryId (..)
  , ReserveId (..)

    -- * OffChain table IDs
  , OffChainPoolDataId (..)
  , OffChainPoolFetchErrorId (..)
  , OffChainVoteDataId (..)
  , OffChainVoteGovActionDataId (..)
  , OffChainVoteDrepDataId (..)
  , OffChainVoteAuthorId (..)
  , OffChainVoteReferenceId (..)
  , OffChainVoteExternalUpdateId (..)
  , OffChainVoteFetchErrorId (..)

    -- * Referenced by other tables
  , PoolHashId (..)
  , StakeAddressId (..)
  , DatumId (..)
  , ScriptId (..)
  , RedeemerId (..)
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

-- ---------------------------------------------------------------------------
-- * Encoder \/ decoder helpers
-- ---------------------------------------------------------------------------

-- | Build a row decoder for an @id@ column, given the constructor.
idDecoder :: (Int64 -> a) -> D.Row a
idDecoder f = D.column (D.nonNullable $ f <$> D.int8)

-- | Build a row decoder for a nullable foreign-key column.
maybeIdDecoder :: (Int64 -> a) -> D.Row (Maybe a)
maybeIdDecoder f = D.column (D.nullable $ f <$> D.int8)

-- | Build a parameter encoder for an @id@ column, given the accessor.
idEncoder :: (a -> Int64) -> E.Params a
idEncoder f = E.param $ E.nonNullable $ f >$< E.int8

-- | Build a parameter encoder for a nullable foreign-key column.
maybeIdEncoder :: (a -> Int64) -> E.Params (Maybe a)
maybeIdEncoder f = E.param $ E.nullable $ f >$< E.int8

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

-- | Primary key for the @meta@ table.
newtype MetaId = MetaId { getMetaId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @epoch_sync_time@ table.
newtype EpochSyncTimeId = EpochSyncTimeId { getEpochSyncTimeId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @reverse_index@ table.
newtype ReverseIndexId = ReverseIndexId { getReverseIndexId :: Int64 }
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

-- | Primary key for the @collateral_tx_out@ table.
newtype CollateralTxOutId = CollateralTxOutId { getCollateralTxOutId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Address table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @address@ table.
-- Referenced by @tx_out.address_id@.
newtype AddressId = AddressId { getAddressId :: Int64 }
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
-- * ScriptsDatums table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @extra_key_witness@ table.
newtype ExtraKeyWitnessId = ExtraKeyWitnessId { getExtraKeyWitnessId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @redeemer_data@ table.
newtype RedeemerDataId = RedeemerDataId { getRedeemerDataId :: Int64 }
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
-- * EpochBoundary table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @ada_pots@ table.
-- One row per epoch boundary; written by the EpochBoundary extractor.
newtype AdaPotsId = AdaPotsId { getAdaPotsId :: Int64 }
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
