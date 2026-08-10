{-# LANGUAGE OverloadedStrings #-}

-- | Newtype wrappers for database primary keys.
--
-- Each key is a newtype over 'Int64', so a 'BlockId' cannot stand in for
-- a 'TxId'.
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
  , EpochSyncTimeId (..)

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
  , PotRewardId (..)
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

idDecoder :: (Int64 -> a) -> D.Row a
idDecoder f = D.column (D.nonNullable $ f <$> D.int8)

maybeIdDecoder :: (Int64 -> a) -> D.Row (Maybe a)
maybeIdDecoder f = D.column (D.nullable $ f <$> D.int8)

idEncoder :: (a -> Int64) -> E.Params a
idEncoder f = E.param $ E.nonNullable $ f >$< E.int8

maybeIdEncoder :: (a -> Int64) -> E.Params (Maybe a)
maybeIdEncoder f = E.param $ E.nullable $ f >$< E.int8

-- ---------------------------------------------------------------------------
-- * Core table IDs
-- ---------------------------------------------------------------------------

newtype BlockId = BlockId { getBlockId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype TxId = TxId { getTxId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype SlotLeaderId = SlotLeaderId { getSlotLeaderId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EpochSyncTimeId = EpochSyncTimeId { getEpochSyncTimeId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * UTxO table IDs
-- ---------------------------------------------------------------------------

newtype TxOutId = TxOutId { getTxOutId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype TxInId = TxInId { getTxInId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CollateralTxInId = CollateralTxInId { getCollateralTxInId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype ReferenceTxInId = ReferenceTxInId { getReferenceTxInId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CollateralTxOutId = CollateralTxOutId { getCollateralTxOutId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Address table IDs
-- ---------------------------------------------------------------------------

-- | Referenced by @tx_out.address_id@.
newtype AddressId = AddressId { getAddressId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Metadata table IDs
-- ---------------------------------------------------------------------------

newtype TxMetadataId = TxMetadataId { getTxMetadataId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * MultiAsset table IDs
-- ---------------------------------------------------------------------------

newtype MultiAssetId = MultiAssetId { getMultiAssetId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype MaTxMintId = MaTxMintId { getMaTxMintId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype MaTxOutId = MaTxOutId { getMaTxOutId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * ScriptsDatums table IDs
-- ---------------------------------------------------------------------------

newtype ExtraKeyWitnessId = ExtraKeyWitnessId { getExtraKeyWitnessId :: Int64 }
  deriving stock (Eq, Ord, Show)

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

newtype RewardId = RewardId { getRewardId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype PotRewardId = PotRewardId { getPotRewardId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EpochStakeId = EpochStakeId { getEpochStakeId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EpochStakeProgressId = EpochStakeProgressId { getEpochStakeProgressId :: Int64 }
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

newtype PoolStatId = PoolStatId { getPoolStatId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype DelistedPoolId = DelistedPoolId { getDelistedPoolId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype ReservedPoolTickerId = ReservedPoolTickerId { getReservedPoolTickerId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * CBOR table IDs
-- ---------------------------------------------------------------------------

newtype TxCborId = TxCborId { getTxCborId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Governance table IDs
-- ---------------------------------------------------------------------------

newtype DrepHashId = DrepHashId { getDrepHashId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype DrepRegistrationId = DrepRegistrationId { getDrepRegistrationId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype DrepDistrId = DrepDistrId { getDrepDistrId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype DelegationVoteId = DelegationVoteId { getDelegationVoteId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype GovActionProposalId = GovActionProposalId { getGovActionProposalId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype VotingProcedureId = VotingProcedureId { getVotingProcedureId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype VotingAnchorId = VotingAnchorId { getVotingAnchorId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype ConstitutionId = ConstitutionId { getConstitutionId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CommitteeId = CommitteeId { getCommitteeId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CommitteeHashId = CommitteeHashId { getCommitteeHashId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CommitteeMemberId = CommitteeMemberId { getCommitteeMemberId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CommitteeRegistrationId = CommitteeRegistrationId { getCommitteeRegistrationId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CommitteeDeRegistrationId = CommitteeDeRegistrationId { getCommitteeDeRegistrationId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype ParamProposalId = ParamProposalId { getParamProposalId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype TreasuryWithdrawalId = TreasuryWithdrawalId { getTreasuryWithdrawalId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EventInfoId = EventInfoId { getEventInfoId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * EpochSyncStats table IDs
-- ---------------------------------------------------------------------------

newtype EpochSyncStatsId = EpochSyncStatsId { getEpochSyncStatsId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * EpochBoundary table IDs
-- ---------------------------------------------------------------------------

-- | One row per epoch boundary; written by the EpochBoundary extractor.
newtype AdaPotsId = AdaPotsId { getAdaPotsId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EpochId = EpochId { getEpochId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EpochParamId = EpochParamId { getEpochParamId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype EpochStateId = EpochStateId { getEpochStateId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype CostModelId = CostModelId { getCostModelId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype PotTransferId = PotTransferId { getPotTransferId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype TreasuryId = TreasuryId { getTreasuryId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype ReserveId = ReserveId { getReserveId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * OffChain table IDs
-- ---------------------------------------------------------------------------

newtype OffChainPoolDataId = OffChainPoolDataId { getOffChainPoolDataId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainPoolFetchErrorId = OffChainPoolFetchErrorId { getOffChainPoolFetchErrorId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteDataId = OffChainVoteDataId { getOffChainVoteDataId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteGovActionDataId = OffChainVoteGovActionDataId { getOffChainVoteGovActionDataId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteDrepDataId = OffChainVoteDrepDataId { getOffChainVoteDrepDataId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteAuthorId = OffChainVoteAuthorId { getOffChainVoteAuthorId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteReferenceId = OffChainVoteReferenceId { getOffChainVoteReferenceId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteExternalUpdateId = OffChainVoteExternalUpdateId { getOffChainVoteExternalUpdateId :: Int64 }
  deriving stock (Eq, Ord, Show)

newtype OffChainVoteFetchErrorId = OffChainVoteFetchErrorId { getOffChainVoteFetchErrorId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Referenced by other tables
-- ---------------------------------------------------------------------------

-- | Referenced by 'SlotLeader.slotLeaderPoolHashId'.
newtype PoolHashId = PoolHashId { getPoolHashId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Referenced by @tx_out.stake_address_id@.
newtype StakeAddressId = StakeAddressId { getStakeAddressId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Referenced by @tx_out.inline_datum_id@.
newtype DatumId = DatumId { getDatumId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Referenced by @tx_out.reference_script_id@.
newtype ScriptId = ScriptId { getScriptId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Referenced by the @redeemer_id@ column of @tx_in@, @delegation@,
-- @stake_deregistration@, @withdrawal@ and @delegation_vote@.
newtype RedeemerId = RedeemerId { getRedeemerId :: Int64 }
  deriving stock (Eq, Ord, Show)
