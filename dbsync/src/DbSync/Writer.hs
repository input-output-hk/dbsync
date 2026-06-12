{-# LANGUAGE OverloadedStrings #-}

-- | Writer interface for the unified extraction pipeline.
--
-- Two implementations exist, one per persisting phase:
--
-- * __IngestChainHistory__: COPY encoding via @putCopyData@ on
--   per-table @libpq@ connections, epoch-aligned commits.
-- * __FollowingChainTip__: per-record @INSERT@ via @hasql@,
--   per-block commits with rollback support.
--
-- Leaf-table writers take just the typed row — PostgreSQL allocates
-- the id on the backing identity sequence. FK-referenced writers
-- take an explicit @XId@ so sibling rows in the same block can use
-- it as a foreign key.
module DbSync.Writer
  ( -- * Types
    Writer (..)

    -- * Accessor class
  , HasWriter (..)
  ) where

import Cardano.Prelude (IO)

import DbSync.Db.Schema.AdaPots (AdaPots)
import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.CBOR (TxCbor)
import DbSync.Db.Schema.Core (Block, SlotLeader, Tx)
import DbSync.Db.Schema.EpochBoundary
  ( CostModel, EpochParam, EpochState, PotTransfer, Reserve, Treasury )
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats)
import DbSync.Db.Schema.Governance
  ( Committee
  , CommitteeDeRegistration
  , CommitteeHash
  , CommitteeMember
  , CommitteeRegistration
  , Constitution
  , DelegationVote
  , DrepDistr
  , DrepHash
  , DrepRegistration
  , EventInfo
  , GovActionProposal
  , ParamProposal
  , TreasuryWithdrawal
  , VotingAnchor
  , VotingProcedure
  )
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Metadata (TxMetadata)
import DbSync.Db.Schema.MultiAsset (MultiAsset, MaTxMint, MaTxOut)
import DbSync.Db.Schema.OffChainPool (OffChainPoolData, OffChainPoolFetchError)
import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor
  , OffChainVoteData
  , OffChainVoteDrepData
  , OffChainVoteExternalUpdate
  , OffChainVoteFetchError
  , OffChainVoteGovActionData
  , OffChainVoteReference
  )
import DbSync.Db.Schema.Pool
  ( DelistedPool
  , PoolHash
  , PoolMetadataRef
  , PoolOwner
  , PoolRelay
  , PoolRetire
  , PoolStat
  , PoolUpdate
  , ReservedPoolTicker
  )
import DbSync.Db.Schema.ScriptsDatums
  ( Datum, ExtraKeyWitness, Redeemer, RedeemerData, Script )
import DbSync.Db.Schema.StakeDelegation
  ( Delegation
  , EpochStake
  , EpochStakeProgress
  , PotReward
  , Reward
  , StakeAddress
  , StakeDeregistration
  , StakeRegistration
  , Withdrawal
  )
import DbSync.Db.Schema.UTxO (TxOut, TxIn, CollateralTxIn, CollateralTxOut, ReferenceTxIn)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | How to persist extracted rows. Parameterised by effect monad @m@.
data Writer m = Writer
  { -- ---------------------------------------------------------------
    -- Core tables
    -- ---------------------------------------------------------------
    writeBlock      :: !(BlockId -> Block -> m ())
  , writeTx         :: !(TxId -> Tx -> m ())
  , writeSlotLeader :: !(SlotLeaderId -> SlotLeader -> m ())

    -- ---------------------------------------------------------------
    -- UTxO tables
    -- ---------------------------------------------------------------
  , writeAddress         :: !(AddressId -> Address -> m ())
  , writeTxOut           :: !(TxOutId -> TxOut -> m ())
  , writeTxIn            :: !(TxIn -> m ())
  , writeCollateralTxIn  :: !(CollateralTxIn -> m ())
  , writeCollateralTxOut :: !(CollateralTxOutId -> CollateralTxOut -> m ())
  , writeReferenceTxIn   :: !(ReferenceTxIn -> m ())

    -- ---------------------------------------------------------------
    -- Metadata tables
    -- ---------------------------------------------------------------
  , writeTxMetadata :: !(TxMetadata -> m ())

    -- ---------------------------------------------------------------
    -- MultiAsset tables
    -- ---------------------------------------------------------------
  , writeMultiAsset :: !(MultiAssetId -> MultiAsset -> m ())
  , writeMaTxMint   :: !(MaTxMint -> m ())
  , writeMaTxOut    :: !(MaTxOut -> m ())

    -- ---------------------------------------------------------------
    -- Stake delegation tables
    -- ---------------------------------------------------------------
  , writeStakeAddress        :: !(StakeAddressId -> StakeAddress -> m ())
  , writeStakeRegistration   :: !(StakeRegistration -> m ())
  , writeStakeDeregistration :: !(StakeDeregistration -> m ())
  , writeDelegation          :: !(Delegation -> m ())
  , writeWithdrawal          :: !(Withdrawal -> m ())

    -- ---------------------------------------------------------------
    -- Stake delegation (ledger-derived, IDENTITY leaves)
    -- ---------------------------------------------------------------
  , writeReward             :: !(Reward -> m ())
  , writePotReward          :: !(PotReward -> m ())
  , writeEpochStake         :: !(EpochStake -> m ())
  , writeEpochStakeProgress :: !(EpochStakeProgress -> m ())

    -- ---------------------------------------------------------------
    -- Pool tables
    -- ---------------------------------------------------------------
  , writePoolHash        :: !(PoolHashId -> PoolHash -> m ())
  , writePoolUpdate      :: !(PoolUpdateId -> PoolUpdate -> m ())
  , writePoolMetadataRef :: !(PoolMetadataRefId -> PoolMetadataRef -> m ())
  , writePoolOwner       :: !(PoolOwner -> m ())
  , writePoolRetire      :: !(PoolRetire -> m ())
  , writePoolRelay       :: !(PoolRelay -> m ())
  , writePoolStat        :: !(PoolStat -> m ())

    -- ---------------------------------------------------------------
    -- Off-chain pools (IDENTITY leaves)
    -- ---------------------------------------------------------------
  , writeOffChainPoolData       :: !(OffChainPoolData -> m ())
  , writeOffChainPoolFetchError :: !(OffChainPoolFetchError -> m ())
  , writeDelistedPool           :: !(DelistedPool -> m ())
  , writeReservedPoolTicker     :: !(ReservedPoolTicker -> m ())

    -- ---------------------------------------------------------------
    -- Off-chain votes (IDENTITY leaves)
    -- ---------------------------------------------------------------
  , writeOffChainVoteData           :: !(OffChainVoteData -> m ())
  , writeOffChainVoteGovActionData  :: !(OffChainVoteGovActionData -> m ())
  , writeOffChainVoteDrepData       :: !(OffChainVoteDrepData -> m ())
  , writeOffChainVoteAuthor         :: !(OffChainVoteAuthor -> m ())
  , writeOffChainVoteReference      :: !(OffChainVoteReference -> m ())
  , writeOffChainVoteExternalUpdate :: !(OffChainVoteExternalUpdate -> m ())
  , writeOffChainVoteFetchError     :: !(OffChainVoteFetchError -> m ())

    -- ---------------------------------------------------------------
    -- CBOR tables
    -- ---------------------------------------------------------------
  , writeTxCbor :: !(TxCbor -> m ())

    -- ---------------------------------------------------------------
    -- Epoch sync stats
    -- ---------------------------------------------------------------
  , writeEpochSyncStats :: !(EpochSyncStatsId -> EpochSyncStats -> m ())

    -- ---------------------------------------------------------------
    -- Epoch boundary tables
    -- ---------------------------------------------------------------
  , writeAdaPots     :: !(AdaPots -> m ())
  , writeEpochParam  :: !(EpochParam -> m ())
  , writeEpochState  :: !(EpochState -> m ())
  , writeCostModel   :: !(CostModelId -> CostModel -> m ())
  , writePotTransfer :: !(PotTransfer -> m ())
  , writeTreasury    :: !(Treasury -> m ())
  , writeReserve     :: !(Reserve -> m ())

    -- ---------------------------------------------------------------
    -- Scripts / datums tables
    -- ---------------------------------------------------------------
  , writeDatum           :: !(DatumId -> Datum -> m ())
  , writeScript          :: !(ScriptId -> Script -> m ())
  , writeRedeemer        :: !(RedeemerId -> Redeemer -> m ())
  , writeRedeemerData    :: !(RedeemerDataId -> RedeemerData -> m ())
  , writeExtraKeyWitness :: !(ExtraKeyWitness -> m ())

    -- ---------------------------------------------------------------
    -- Governance — FK-referenced (counter-managed)
    -- ---------------------------------------------------------------
  , writeGovActionProposal :: !(GovActionProposalId -> GovActionProposal -> m ())
  , writeParamProposal     :: !(ParamProposalId -> ParamProposal -> m ())
  , writeCommittee         :: !(CommitteeId -> Committee -> m ())
  , writeConstitution      :: !(ConstitutionId -> Constitution -> m ())
  , writeEventInfo         :: !(EventInfoId -> EventInfo -> m ())

    -- ---------------------------------------------------------------
    -- Governance — dedup-backed
    -- ---------------------------------------------------------------
  , writeDrepHash          :: !(DrepHashId -> DrepHash -> m ())
  , writeCommitteeHash     :: !(CommitteeHashId -> CommitteeHash -> m ())
  , writeVotingAnchor      :: !(VotingAnchorId -> VotingAnchor -> m ())

    -- ---------------------------------------------------------------
    -- Governance — IDENTITY leaves
    -- ---------------------------------------------------------------
  , writeDrepRegistration       :: !(DrepRegistration -> m ())
  , writeDrepDistr              :: !(DrepDistr -> m ())
  , writeDelegationVote         :: !(DelegationVote -> m ())
  , writeVotingProcedure        :: !(VotingProcedure -> m ())
  , writeTreasuryWithdrawal     :: !(TreasuryWithdrawal -> m ())
  , writeCommitteeMember        :: !(CommitteeMember -> m ())
  , writeCommitteeRegistration  :: !(CommitteeRegistration -> m ())
  , writeCommitteeDeRegistration :: !(CommitteeDeRegistration -> m ())

    -- ---------------------------------------------------------------
    -- Transaction control
    -- ---------------------------------------------------------------
  , commit :: !(m ())
  }

-- ---------------------------------------------------------------------------
-- * Accessor class
-- ---------------------------------------------------------------------------

-- | Access the (IO-effecting) writer from any environment.
--
-- See 'DbSync.Resolver.HasResolver' for the rationale on fixing the effect
-- monad to 'IO' at the env layer.
class HasWriter env where
  getWriter :: env -> Writer IO
