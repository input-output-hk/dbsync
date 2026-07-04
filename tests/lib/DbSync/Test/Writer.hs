{-# LANGUAGE OverloadedStrings #-}

-- | Test writer for the unified extraction pipeline.
--
-- Captures written records in 'IORef's for test assertions.
-- No database, no COPY encoding — just accumulates typed records.
module DbSync.Test.Writer
  ( -- * Construction
    mkTestWriter
  , TestWriterState (..)
  , emptyTestWriterState
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef')

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Core (Block, PoolHash, SlotLeader, StakeAddress, Tx)
import DbSync.Db.Schema.EpochBoundary
  ( CostModel, EpochParam, EpochState, PotTransfer, Reserve, Treasury )
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
  , GovActionProposal
  , ParamProposal
  , TreasuryWithdrawal
  , VotingAnchor
  , VotingProcedure
  )
import DbSync.Db.Schema.Ids
  ( AddressId
  , BlockId
  , CollateralTxOutId
  , CommitteeHashId
  , CommitteeId
  , ConstitutionId
  , CostModelId
  , DatumId
  , DrepHashId
  , GovActionProposalId
  , ParamProposalId
  , PoolHashId
  , PoolUpdateId
  , RedeemerDataId
  , RedeemerId
  , ScriptId
  , SlotLeaderId
  , StakeAddressId
  , TxId
  , TxOutId
  , VotingAnchorId
  )
import DbSync.Db.Schema.Metadata (TxMetadata)
import DbSync.Db.Schema.MultiAsset (MaTxMint, MaTxOut)
import DbSync.Db.Schema.Pool (PoolStat, PoolUpdate)
import DbSync.Db.Schema.ScriptsDatums
  ( Datum, ExtraKeyWitness, Redeemer, RedeemerData, Script )
import DbSync.Db.Schema.StakeDelegation
  ( EpochStake
  , EpochStakeProgress
  , PotReward
  , Reward
  , StakeRegistration
  )
import DbSync.Db.Schema.UTxO (CollateralTxIn, CollateralTxOut, ReferenceTxIn, TxIn, TxOut)
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Accumulated state from a test writer. Leaf-table fields drop the
-- @XId@ from the tuple — PostgreSQL allocates the id at insert time
-- so the in-memory test buffer never sees one.
data TestWriterState = TestWriterState
  { twBlocks            :: ![(BlockId, Block)]
  , twTxs               :: ![(TxId, Tx)]
  , twSlotLeaders       :: ![(SlotLeaderId, SlotLeader)]
  , twAddresses         :: ![(AddressId, Address)]
  , twTxOuts            :: ![(TxOutId, TxOut)]
  , twTxIns             :: ![TxIn]
  , twCollateralTxIns   :: ![CollateralTxIn]
  , twCollateralTxOuts  :: ![(CollateralTxOutId, CollateralTxOut)]
  , twReferenceTxIns    :: ![ReferenceTxIn]
  , twStakeAddresses    :: ![(StakeAddressId, StakeAddress)]
  , twStakeRegistrations :: ![StakeRegistration]
  , twRewards           :: ![Reward]
  , twPotRewards        :: ![PotReward]
  , twEpochStakes       :: ![EpochStake]
  , twEpochStakeProgresses :: ![EpochStakeProgress]
  , twPoolHashes        :: ![(PoolHashId, PoolHash)]
  , twPoolUpdates       :: ![(PoolUpdateId, PoolUpdate)]
  , twPoolStats         :: ![PoolStat]
  , twTxMetadata        :: ![TxMetadata]
  , twMaTxMints         :: ![MaTxMint]
  , twMaTxOuts          :: ![MaTxOut]
  , twPotTransfers      :: ![PotTransfer]
  , twTreasuries        :: ![Treasury]
  , twReserves          :: ![Reserve]
  , twEpochParams       :: ![EpochParam]
  , twEpochStates       :: ![EpochState]
  , twCostModels        :: ![(CostModelId, CostModel)]
  , twDatums            :: ![(DatumId, Datum)]
  , twScripts           :: ![(ScriptId, Script)]
  , twRedeemers         :: ![(RedeemerId, Redeemer)]
  , twRedeemerData      :: ![(RedeemerDataId, RedeemerData)]
  , twExtraKeyWitnesses :: ![ExtraKeyWitness]

    -- Governance
  , twGovActionProposals       :: ![(GovActionProposalId, GovActionProposal)]
  , twParamProposals           :: ![(ParamProposalId, ParamProposal)]
  , twCommittees               :: ![(CommitteeId, Committee)]
  , twConstitutions            :: ![(ConstitutionId, Constitution)]
  , twDrepHashes               :: ![(DrepHashId, DrepHash)]
  , twCommitteeHashes          :: ![(CommitteeHashId, CommitteeHash)]
  , twVotingAnchors            :: ![(VotingAnchorId, VotingAnchor)]
  , twDrepRegistrations        :: ![DrepRegistration]
  , twDelegationVotes          :: ![DelegationVote]
  , twVotingProcedures         :: ![VotingProcedure]
  , twTreasuryWithdrawals      :: ![TreasuryWithdrawal]
  , twCommitteeMembers         :: ![CommitteeMember]
  , twCommitteeRegistrations   :: ![CommitteeRegistration]
  , twCommitteeDeRegistrations :: ![CommitteeDeRegistration]
  , twDrepDistrs               :: ![DrepDistr]

  , twCommits           :: !Int
  }
  deriving stock (Show)

-- | Empty test writer state.
emptyTestWriterState :: TestWriterState
emptyTestWriterState = TestWriterState
  { twBlocks            = []
  , twTxs               = []
  , twSlotLeaders       = []
  , twAddresses         = []
  , twTxOuts            = []
  , twTxIns             = []
  , twCollateralTxIns   = []
  , twCollateralTxOuts  = []
  , twReferenceTxIns    = []
  , twStakeAddresses    = []
  , twStakeRegistrations = []
  , twRewards           = []
  , twPotRewards        = []
  , twEpochStakes       = []
  , twEpochStakeProgresses = []
  , twPoolHashes        = []
  , twPoolUpdates       = []
  , twPoolStats         = []
  , twTxMetadata        = []
  , twMaTxMints         = []
  , twMaTxOuts          = []
  , twPotTransfers     = []
  , twTreasuries       = []
  , twReserves         = []
  , twEpochParams      = []
  , twEpochStates      = []
  , twCostModels       = []
  , twDatums           = []
  , twScripts          = []
  , twRedeemers        = []
  , twRedeemerData     = []
  , twExtraKeyWitnesses = []
  , twGovActionProposals       = []
  , twParamProposals           = []
  , twCommittees               = []
  , twConstitutions            = []
  , twDrepHashes               = []
  , twCommitteeHashes          = []
  , twVotingAnchors            = []
  , twDrepRegistrations        = []
  , twDelegationVotes          = []
  , twVotingProcedures         = []
  , twTreasuryWithdrawals      = []
  , twCommitteeMembers         = []
  , twCommitteeRegistrations   = []
  , twCommitteeDeRegistrations = []
  , twDrepDistrs               = []
  , twCommits           = 0
  }

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build a 'Writer' that captures the row-producing calls used by
-- the unit tests. Calls without a corresponding field on
-- 'TestWriterState' are no-ops.
mkTestWriter :: IORef TestWriterState -> Writer IO
mkTestWriter ref = Writer
  { -- Core
    writeBlock = \bid blk ->
      atomicModifyIORef' ref $ \s ->
        (s { twBlocks = twBlocks s ++ [(bid, blk)] }, ())
  , writeTx = \tid tx ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxs = twTxs s ++ [(tid, tx)] }, ())
  , writeSlotLeader = \slid sl ->
      atomicModifyIORef' ref $ \s ->
        (s { twSlotLeaders = twSlotLeaders s ++ [(slid, sl)] }, ())

    -- UTxO
  , writeAddress = \aid addr ->
      atomicModifyIORef' ref $ \s ->
        (s { twAddresses = twAddresses s ++ [(aid, addr)] }, ())
  , writeTxOut = \oid txOut ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxOuts = twTxOuts s ++ [(oid, txOut)] }, ())
  , writeTxIn = \ti ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxIns = twTxIns s ++ [ti] }, ())
  , writeCollateralTxIn = \ci ->
      atomicModifyIORef' ref $ \s ->
        (s { twCollateralTxIns = twCollateralTxIns s ++ [ci] }, ())
  , writeCollateralTxOut = \oid co ->
      atomicModifyIORef' ref $ \s ->
        (s { twCollateralTxOuts = twCollateralTxOuts s ++ [(oid, co)] }, ())
  , writeReferenceTxIn = \ri ->
      atomicModifyIORef' ref $ \s ->
        (s { twReferenceTxIns = twReferenceTxIns s ++ [ri] }, ())

    -- Metadata
  , writeTxMetadata = \md ->
      atomicModifyIORef' ref $ \s ->
        (s { twTxMetadata = twTxMetadata s ++ [md] }, ())

    -- MultiAsset
  , writeMultiAsset = \_ _ -> pure ()
  , writeMaTxMint = \m ->
      atomicModifyIORef' ref $ \s ->
        (s { twMaTxMints = twMaTxMints s ++ [m] }, ())
  , writeMaTxOut = \m ->
      atomicModifyIORef' ref $ \s ->
        (s { twMaTxOuts = twMaTxOuts s ++ [m] }, ())

    -- StakeDelegation
  , writeStakeAddress = \said sa ->
      atomicModifyIORef' ref $ \s ->
        (s { twStakeAddresses = twStakeAddresses s ++ [(said, sa)] }, ())
  , writeStakeRegistration   = \sr ->
      atomicModifyIORef' ref $ \s ->
        (s { twStakeRegistrations = twStakeRegistrations s ++ [sr] }, ())
  , writeStakeDeregistration = \_ -> pure ()
  , writeDelegation          = \_ -> pure ()
  , writeWithdrawal          = \_ -> pure ()
  , writeReward = \r ->
      atomicModifyIORef' ref $ \s ->
        (s { twRewards = twRewards s ++ [r] }, ())
  , writePotReward = \pr ->
      atomicModifyIORef' ref $ \s ->
        (s { twPotRewards = twPotRewards s ++ [pr] }, ())
  , writeEpochStake = \es ->
      atomicModifyIORef' ref $ \s ->
        (s { twEpochStakes = twEpochStakes s ++ [es] }, ())
  , writeEpochStakeProgress = \esp ->
      atomicModifyIORef' ref $ \s ->
        (s { twEpochStakeProgresses = twEpochStakeProgresses s ++ [esp] }, ())

    -- Pool
  , writePoolHash = \phid ph ->
      atomicModifyIORef' ref $ \s ->
        (s { twPoolHashes = twPoolHashes s ++ [(phid, ph)] }, ())
  , writePoolUpdate = \puid pu ->
      atomicModifyIORef' ref $ \s ->
        (s { twPoolUpdates = twPoolUpdates s ++ [(puid, pu)] }, ())
  , writePoolMetadataRef = \_ _ -> pure ()
  , writePoolOwner       = \_ -> pure ()
  , writePoolRetire      = \_ -> pure ()
  , writePoolRelay       = \_ -> pure ()
  , writePoolStat        = \ps ->
      atomicModifyIORef' ref $ \s ->
        (s { twPoolStats = twPoolStats s ++ [ps] }, ())

    -- OffChainPools — written by the off-chain pool worker, not by
    -- the extractor under test. No-op in unit tests.
  , writeOffChainPoolData       = \_ -> pure ()
  , writeOffChainPoolFetchError = \_ -> pure ()
  , writeDelistedPool           = \_ -> pure ()
  , writeReservedPoolTicker     = \_ -> pure ()

    -- OffChainVotes — written by the off-chain vote worker, not by
    -- the extractor under test. No-op in unit tests.
  , writeOffChainVoteData           = \_ -> pure ()
  , writeOffChainVoteGovActionData  = \_ -> pure ()
  , writeOffChainVoteDrepData       = \_ -> pure ()
  , writeOffChainVoteAuthor         = \_ -> pure ()
  , writeOffChainVoteReference      = \_ -> pure ()
  , writeOffChainVoteExternalUpdate = \_ -> pure ()
  , writeOffChainVoteFetchError     = \_ -> pure ()

    -- CBOR (no-op)
  , writeTxCbor = \_ -> pure ()

    -- EpochSyncStats (no-op)
  , writeEpochSyncStats = \_ _ -> pure ()

    -- EpochBoundary
  , writeAdaPots     = \_ -> pure ()
  , writeEpochParam  = \ep ->
      atomicModifyIORef' ref $ \s ->
        (s { twEpochParams = twEpochParams s ++ [ep] }, ())
  , writeEpochState  = \es ->
      atomicModifyIORef' ref $ \s ->
        (s { twEpochStates = twEpochStates s ++ [es] }, ())
  , writeCostModel   = \cmid cm ->
      atomicModifyIORef' ref $ \s ->
        (s { twCostModels = twCostModels s ++ [(cmid, cm)] }, ())
  , writePotTransfer = \pt ->
      atomicModifyIORef' ref $ \s ->
        (s { twPotTransfers = twPotTransfers s ++ [pt] }, ())
  , writeTreasury    = \t ->
      atomicModifyIORef' ref $ \s ->
        (s { twTreasuries = twTreasuries s ++ [t] }, ())
  , writeReserve     = \r ->
      atomicModifyIORef' ref $ \s ->
        (s { twReserves = twReserves s ++ [r] }, ())

    -- ScriptsDatums
  , writeDatum           = \did d ->
      atomicModifyIORef' ref $ \s ->
        (s { twDatums = twDatums s ++ [(did, d)] }, ())
  , writeScript          = \sid sc ->
      atomicModifyIORef' ref $ \s ->
        (s { twScripts = twScripts s ++ [(sid, sc)] }, ())
  , writeRedeemer        = \rid r ->
      atomicModifyIORef' ref $ \s ->
        (s { twRedeemers = twRedeemers s ++ [(rid, r)] }, ())
  , writeRedeemerData    = \rdid rd ->
      atomicModifyIORef' ref $ \s ->
        (s { twRedeemerData = twRedeemerData s ++ [(rdid, rd)] }, ())
  , writeExtraKeyWitness = \ek ->
      atomicModifyIORef' ref $ \s ->
        (s { twExtraKeyWitnesses = twExtraKeyWitnesses s ++ [ek] }, ())

    -- Governance — see 'TestWriterState' for the captured fields;
    -- only the slice-2-relevant tables are stored, the rest are no-ops.
  , writeGovActionProposal       = \gid g ->
      atomicModifyIORef' ref $ \s ->
        (s { twGovActionProposals = twGovActionProposals s ++ [(gid, g)] }, ())
  , writeParamProposal           = \pid p ->
      atomicModifyIORef' ref $ \s ->
        (s { twParamProposals = twParamProposals s ++ [(pid, p)] }, ())
  , writeCommittee               = \cid c ->
      atomicModifyIORef' ref $ \s ->
        (s { twCommittees = twCommittees s ++ [(cid, c)] }, ())
  , writeConstitution            = \cid c ->
      atomicModifyIORef' ref $ \s ->
        (s { twConstitutions = twConstitutions s ++ [(cid, c)] }, ())
  , writeEventInfo               = \_ _ -> pure ()
  , writeDrepHash                = \did d ->
      atomicModifyIORef' ref $ \s ->
        (s { twDrepHashes = twDrepHashes s ++ [(did, d)] }, ())
  , writeCommitteeHash           = \cid c ->
      atomicModifyIORef' ref $ \s ->
        (s { twCommitteeHashes = twCommitteeHashes s ++ [(cid, c)] }, ())
  , writeVotingAnchor            = \vid v ->
      atomicModifyIORef' ref $ \s ->
        (s { twVotingAnchors = twVotingAnchors s ++ [(vid, v)] }, ())
  , writeDrepRegistration        = \dr ->
      atomicModifyIORef' ref $ \s ->
        (s { twDrepRegistrations = twDrepRegistrations s ++ [dr] }, ())
  , writeDrepDistr               = \dd ->
      atomicModifyIORef' ref $ \s ->
        (s { twDrepDistrs = twDrepDistrs s ++ [dd] }, ())
  , writeDelegationVote          = \dv ->
      atomicModifyIORef' ref $ \s ->
        (s { twDelegationVotes = twDelegationVotes s ++ [dv] }, ())
  , writeVotingProcedure         = \vp ->
      atomicModifyIORef' ref $ \s ->
        (s { twVotingProcedures = twVotingProcedures s ++ [vp] }, ())
  , writeTreasuryWithdrawal      = \tw ->
      atomicModifyIORef' ref $ \s ->
        (s { twTreasuryWithdrawals = twTreasuryWithdrawals s ++ [tw] }, ())
  , writeCommitteeMember         = \cm ->
      atomicModifyIORef' ref $ \s ->
        (s { twCommitteeMembers = twCommitteeMembers s ++ [cm] }, ())
  , writeCommitteeRegistration   = \cr ->
      atomicModifyIORef' ref $ \s ->
        (s { twCommitteeRegistrations = twCommitteeRegistrations s ++ [cr] }, ())
  , writeCommitteeDeRegistration = \cdr ->
      atomicModifyIORef' ref $ \s ->
        (s { twCommitteeDeRegistrations = twCommitteeDeRegistrations s ++ [cdr] }, ())

    -- Transaction control
  , commit =
      atomicModifyIORef' ref $ \s ->
        (s { twCommits = twCommits s + 1 }, ())
  }
