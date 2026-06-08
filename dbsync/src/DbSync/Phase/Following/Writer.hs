-- | FollowingChainTip writer: typed records to hasql INSERTs against
-- a single connection. IDs come from the resolver; this layer only
-- writes.
--
-- Two implementations:
--
-- * 'mkWriter' — every @write*@ goes straight to PG via the connection.
--   One round-trip per row; used by the integration test suite where
--   deterministic per-row behaviour makes assertions easier to author.
--
-- * 'mkBufferedWriter' — every @write*@ appends a
--   'Hasql.Pipeline.statement' to a per-block 'WriteBuffer'. The
--   orchestrator drains the buffer in one round-trip at end of block.
--   Used in production; the per-row cost drops from one round-trip to
--   one append.
--
-- The within-block dedup pattern (a SELECT seeing a just-inserted
-- row) is preserved because the buffered resolver maintains an
-- in-process map for the duration of the block.
--
-- 'commit' is a no-op in both implementations; the per-block
-- @BEGIN@\/@COMMIT@ envelope owned by 'DbSync.Phase.Following.Run'
-- is what actually closes the transaction.
--
-- Per-table write functions live in @Writer\/\<extractor\>.hs@.
module DbSync.Phase.Following.Writer
  ( mkWriter
  , mkBufferedWriter
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import qualified DbSync.Phase.Following.Writer.Cbor as Cbor
import qualified DbSync.Phase.Following.Writer.Core as Core
import qualified DbSync.Phase.Following.Writer.Epoch as Epoch
import qualified DbSync.Phase.Following.Writer.EpochBoundary as EpochBoundary
import qualified DbSync.Phase.Following.Writer.Governance as Governance
import qualified DbSync.Phase.Following.Writer.Metadata as Metadata
import qualified DbSync.Phase.Following.Writer.MultiAsset as MultiAsset
import qualified DbSync.Phase.Following.Writer.OffChainPools as OffChainPools
import qualified DbSync.Phase.Following.Writer.Pool as Pool
import qualified DbSync.Phase.Following.Writer.PoolStats as PoolStats
import qualified DbSync.Phase.Following.Writer.ScriptsDatums as ScriptsDatums
import qualified DbSync.Phase.Following.Writer.StakeDelegation as Stake
import qualified DbSync.Phase.Following.Writer.StakeDelegationLedger as StakeLedger
import qualified DbSync.Phase.Following.Writer.UTxO as UTxO
import DbSync.Writer (Writer (..))

-- | Build a 'Writer IO' that runs each insert immediately against
-- the supplied connection.
mkWriter :: Conn.Connection -> Writer IO
mkWriter conn = Writer
  { -- Core
    writeBlock      = Core.writeBlockConn conn
  , writeTx         = Core.writeTxConn conn
  , writeSlotLeader = Core.writeSlotLeaderConn conn

    -- UTxO
  , writeAddress         = UTxO.writeAddressConn conn
  , writeTxOut           = UTxO.writeTxOutConn conn
  , writeTxIn            = UTxO.writeTxInConn conn
  , writeCollateralTxIn  = UTxO.writeCollateralTxInConn conn
  , writeCollateralTxOut = UTxO.writeCollateralTxOutConn conn
  , writeReferenceTxIn   = UTxO.writeReferenceTxInConn conn

    -- Metadata
  , writeTxMetadata = Metadata.writeTxMetadataConn conn

    -- MultiAsset
  , writeMultiAsset = MultiAsset.writeMultiAssetConn conn
  , writeMaTxMint   = MultiAsset.writeMaTxMintConn conn
  , writeMaTxOut    = MultiAsset.writeMaTxOutConn conn

    -- StakeDelegation (incl. pot rebalancing)
  , writeStakeAddress        = Stake.writeStakeAddressConn conn
  , writeStakeRegistration   = Stake.writeStakeRegistrationConn conn
  , writeStakeDeregistration = Stake.writeStakeDeregistrationConn conn
  , writeDelegation          = Stake.writeDelegationConn conn
  , writeWithdrawal          = Stake.writeWithdrawalConn conn
  , writePotTransfer         = Stake.writePotTransferConn conn
  , writeTreasury            = Stake.writeTreasuryConn conn
  , writeReserve             = Stake.writeReserveConn conn

    -- StakeDelegation (ledger-derived)
  , writeReward             = StakeLedger.writeRewardConn conn
  , writePotReward          = StakeLedger.writePotRewardConn conn
  , writeEpochStake         = StakeLedger.writeEpochStakeConn conn
  , writeEpochStakeProgress = StakeLedger.writeEpochStakeProgressConn conn

    -- Pool
  , writePoolHash        = Pool.writePoolHashConn conn
  , writePoolUpdate      = Pool.writePoolUpdateConn conn
  , writePoolMetadataRef = Pool.writePoolMetadataRefConn conn
  , writePoolOwner       = Pool.writePoolOwnerConn conn
  , writePoolRetire      = Pool.writePoolRetireConn conn
  , writePoolRelay       = Pool.writePoolRelayConn conn

    -- PoolStats
  , writePoolStat        = PoolStats.writePoolStatConn conn

    -- OffChainPools
  , writeOffChainPoolData       = OffChainPools.writeOffChainPoolDataConn conn
  , writeOffChainPoolFetchError = OffChainPools.writeOffChainPoolFetchErrorConn conn
  , writeDelistedPool           = OffChainPools.writeDelistedPoolConn conn
  , writeReservedPoolTicker     = OffChainPools.writeReservedPoolTickerConn conn

    -- CBOR
  , writeTxCbor = Cbor.writeTxCborConn conn

    -- EpochSyncStats
  , writeEpochSyncStats = Epoch.writeEpochSyncStatsConn conn

    -- EpochBoundary
  , writeAdaPots    = EpochBoundary.writeAdaPotsConn conn
  , writeEpochParam = EpochBoundary.writeEpochParamConn conn
  , writeEpochState = EpochBoundary.writeEpochStateConn conn
  , writeCostModel  = EpochBoundary.writeCostModelConn conn

    -- ScriptsDatums
  , writeDatum           = ScriptsDatums.writeDatumConn conn
  , writeScript          = ScriptsDatums.writeScriptConn conn
  , writeRedeemer        = ScriptsDatums.writeRedeemerConn conn
  , writeRedeemerData    = ScriptsDatums.writeRedeemerDataConn conn
  , writeExtraKeyWitness = ScriptsDatums.writeExtraKeyWitnessConn conn

    -- Governance
  , writeGovActionProposal       = Governance.writeGovActionProposalConn conn
  , writeParamProposal           = Governance.writeParamProposalConn conn
  , writeCommittee               = Governance.writeCommitteeConn conn
  , writeConstitution            = Governance.writeConstitutionConn conn
  , writeEventInfo               = Governance.writeEventInfoConn conn
  , writeDrepHash                = Governance.writeDrepHashConn conn
  , writeCommitteeHash           = Governance.writeCommitteeHashConn conn
  , writeVotingAnchor            = Governance.writeVotingAnchorConn conn
  , writeDrepRegistration        = Governance.writeDrepRegistrationConn conn
  , writeDrepDistr               = Governance.writeDrepDistrConn conn
  , writeDelegationVote          = Governance.writeDelegationVoteConn conn
  , writeVotingProcedure         = Governance.writeVotingProcedureConn conn
  , writeTreasuryWithdrawal      = Governance.writeTreasuryWithdrawalConn conn
  , writeCommitteeMember         = Governance.writeCommitteeMemberConn conn
  , writeCommitteeRegistration   = Governance.writeCommitteeRegistrationConn conn
  , writeCommitteeDeRegistration = Governance.writeCommitteeDeRegistrationConn conn

    -- No-op: the per-block transaction envelope is owned by
    -- 'DbSync.Phase.Following.Run', not the Writer.
  , commit = pure ()
  }

-- | Buffered writer: every @write*@ appends to the supplied
-- 'WriteBuffer' instead of running the statement immediately. The
-- caller flushes the buffer once at end of block.
--
-- Same row shapes, same encoders, same SQL — only the network timing
-- differs. Each append is a 'modifyIORef' (microseconds); each
-- immediate-mode call would be a libpq round-trip
-- (microseconds-to-milliseconds, depending on PG distance).
mkBufferedWriter :: WriteBuffer -> Writer IO
mkBufferedWriter buf = Writer
  { -- Core
    writeBlock      = Core.writeBlockBuf buf
  , writeTx         = Core.writeTxBuf buf
  , writeSlotLeader = Core.writeSlotLeaderBuf buf

    -- UTxO
  , writeAddress         = UTxO.writeAddressBuf buf
  , writeTxOut           = UTxO.writeTxOutBuf buf
  , writeTxIn            = UTxO.writeTxInBuf buf
  , writeCollateralTxIn  = UTxO.writeCollateralTxInBuf buf
  , writeCollateralTxOut = UTxO.writeCollateralTxOutBuf buf
  , writeReferenceTxIn   = UTxO.writeReferenceTxInBuf buf

    -- Metadata
  , writeTxMetadata = Metadata.writeTxMetadataBuf buf

    -- MultiAsset
  , writeMultiAsset = MultiAsset.writeMultiAssetBuf buf
  , writeMaTxMint   = MultiAsset.writeMaTxMintBuf buf
  , writeMaTxOut    = MultiAsset.writeMaTxOutBuf buf

    -- StakeDelegation (incl. pot rebalancing)
  , writeStakeAddress        = Stake.writeStakeAddressBuf buf
  , writeStakeRegistration   = Stake.writeStakeRegistrationBuf buf
  , writeStakeDeregistration = Stake.writeStakeDeregistrationBuf buf
  , writeDelegation          = Stake.writeDelegationBuf buf
  , writeWithdrawal          = Stake.writeWithdrawalBuf buf
  , writePotTransfer         = Stake.writePotTransferBuf buf
  , writeTreasury            = Stake.writeTreasuryBuf buf
  , writeReserve             = Stake.writeReserveBuf buf

    -- StakeDelegation (ledger-derived)
  , writeReward             = StakeLedger.writeRewardBuf buf
  , writePotReward          = StakeLedger.writePotRewardBuf buf
  , writeEpochStake         = StakeLedger.writeEpochStakeBuf buf
  , writeEpochStakeProgress = StakeLedger.writeEpochStakeProgressBuf buf

    -- Pool
  , writePoolHash        = Pool.writePoolHashBuf buf
  , writePoolUpdate      = Pool.writePoolUpdateBuf buf
  , writePoolMetadataRef = Pool.writePoolMetadataRefBuf buf
  , writePoolOwner       = Pool.writePoolOwnerBuf buf
  , writePoolRetire      = Pool.writePoolRetireBuf buf
  , writePoolRelay       = Pool.writePoolRelayBuf buf

    -- PoolStats
  , writePoolStat        = PoolStats.writePoolStatBuf buf

    -- OffChainPools
  , writeOffChainPoolData       = OffChainPools.writeOffChainPoolDataBuf buf
  , writeOffChainPoolFetchError = OffChainPools.writeOffChainPoolFetchErrorBuf buf
  , writeDelistedPool           = OffChainPools.writeDelistedPoolBuf buf
  , writeReservedPoolTicker     = OffChainPools.writeReservedPoolTickerBuf buf

    -- CBOR
  , writeTxCbor = Cbor.writeTxCborBuf buf

    -- EpochSyncStats
  , writeEpochSyncStats = Epoch.writeEpochSyncStatsBuf buf

    -- EpochBoundary
  , writeAdaPots    = EpochBoundary.writeAdaPotsBuf buf
  , writeEpochParam = EpochBoundary.writeEpochParamBuf buf
  , writeEpochState = EpochBoundary.writeEpochStateBuf buf
  , writeCostModel  = EpochBoundary.writeCostModelBuf buf

    -- ScriptsDatums
  , writeDatum           = ScriptsDatums.writeDatumBuf buf
  , writeScript          = ScriptsDatums.writeScriptBuf buf
  , writeRedeemer        = ScriptsDatums.writeRedeemerBuf buf
  , writeRedeemerData    = ScriptsDatums.writeRedeemerDataBuf buf
  , writeExtraKeyWitness = ScriptsDatums.writeExtraKeyWitnessBuf buf

    -- Governance
  , writeGovActionProposal       = Governance.writeGovActionProposalBuf buf
  , writeParamProposal           = Governance.writeParamProposalBuf buf
  , writeCommittee               = Governance.writeCommitteeBuf buf
  , writeConstitution            = Governance.writeConstitutionBuf buf
  , writeEventInfo               = Governance.writeEventInfoBuf buf
  , writeDrepHash                = Governance.writeDrepHashBuf buf
  , writeCommitteeHash           = Governance.writeCommitteeHashBuf buf
  , writeVotingAnchor            = Governance.writeVotingAnchorBuf buf
  , writeDrepRegistration        = Governance.writeDrepRegistrationBuf buf
  , writeDrepDistr               = Governance.writeDrepDistrBuf buf
  , writeDelegationVote          = Governance.writeDelegationVoteBuf buf
  , writeVotingProcedure         = Governance.writeVotingProcedureBuf buf
  , writeTreasuryWithdrawal      = Governance.writeTreasuryWithdrawalBuf buf
  , writeCommitteeMember         = Governance.writeCommitteeMemberBuf buf
  , writeCommitteeRegistration   = Governance.writeCommitteeRegistrationBuf buf
  , writeCommitteeDeRegistration = Governance.writeCommitteeDeRegistrationBuf buf

  , commit = pure ()
  }
