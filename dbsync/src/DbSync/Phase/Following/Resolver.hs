-- | FollowingChainTip id resolver. Per-extractor method bodies live
-- in @Resolver\/\<extractor\>.hs@; this module composes them.
--
-- 'mkFollowResolver' makes one round-trip per id, which the
-- integration tests want. Production uses
-- 'mkBufferedFollowResolver'. Both keep the same dedup contracts and
-- FK invariants, and the diff test confirms identical rows in PG.
module DbSync.Phase.Following.Resolver
  ( ConsumedTracking (..)
  , mkFollowResolver
  , mkBufferedFollowResolver
  ) where

import Cardano.Prelude

import Data.IORef (newIORef, readIORef)

import qualified Hasql.Connection as Conn

import qualified DbSync.Phase.Following.Resolver.Core as Core
import qualified DbSync.Phase.Following.Resolver.Epoch as Epoch
import qualified DbSync.Phase.Following.Resolver.EpochBoundary as EpochBoundary
import qualified DbSync.Phase.Following.Resolver.Governance as Governance
import DbSync.Phase.Following.Resolver.Internal (newBlockDedupCache)
import qualified DbSync.Phase.Following.Resolver.MultiAsset as MultiAsset
import qualified DbSync.Phase.Following.Resolver.OffChainPools as OffChainPools
import qualified DbSync.Phase.Following.Resolver.OffChainVotes as OffChainVotes
import qualified DbSync.Phase.Following.Resolver.Pool as Pool
import qualified DbSync.Phase.Following.Resolver.ScriptsDatums as ScriptsDatums
import qualified DbSync.Phase.Following.Resolver.StakeDelegation as Stake
import qualified DbSync.Phase.Following.Resolver.UTxO as UTxO
import DbSync.Phase.Following.Resolver.UTxO (ConsumedTracking (..))
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Resolver (IdResolver (..))

-- ---------------------------------------------------------------------------
-- * Direct resolver
-- ---------------------------------------------------------------------------

-- | Every assigner makes a @nextval@ round-trip. Every dedup
-- resolver runs a @SELECT@, then @nextval@ on a miss.
mkFollowResolver :: Conn.Connection -> ConsumedTracking -> IO (IdResolver IO)
mkFollowResolver conn consumedTracking = do
  lastBlock <- newIORef Nothing
  gov       <- Governance.newGovScratchpad
  pure IdResolver
    { -- Core
      assignBlockId     = Core.assignBlockIdFollow    conn lastBlock
    , assignTxId        = Core.assignTxIdConn         conn
    , resolveSlotLeader = Core.resolveSlotLeaderConn  conn
    , resolvePrevBlock  = Core.resolvePrevBlockFollow lastBlock
    , lookupLastBlockId = readIORef                    lastBlock

      -- UTxO
    , recordTxOutAddress           = UTxO.recordTxOutAddressFollow
    , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressFollow
    , resolveAddressId             = UTxO.resolveAddressIdConn         conn
    , assignTxOutId                = UTxO.assignTxOutIdConn             conn
    , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdConn   conn
    , resolveInputValues           = UTxO.resolveInputValuesConn        conn
    , resolveInputUtxo             = UTxO.resolveInputUtxoConn          conn
    , recordTxOutputs              = UTxO.recordTxOutputsConn
    , recordConsumed               = UTxO.recordConsumedConn            conn consumedTracking
    , deleteCachedUtxo             = UTxO.deleteCachedUtxoFollow

      -- MultiAsset
    , resolveMultiAsset = MultiAsset.resolveMultiAssetConn conn

      -- StakeDelegation
    , resolveStakeAddress         = Stake.resolveStakeAddressConn         conn

      -- Pool
    , resolvePoolHash         = Pool.resolvePoolHashConn         conn
    , resolvePoolHashQuery    = Pool.resolvePoolHashQueryConn    conn
    , assignPoolUpdateId      = Pool.assignPoolUpdateIdConn      conn
    , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdConn conn

      -- OffChainPools
    , enqueuePoolMetaFetch    = OffChainPools.enqueuePoolMetaFetchFollow

      -- OffChainVotes
    , enqueueVoteMetaFetch    = OffChainVotes.enqueueVoteMetaFetchFollow

      -- EpochSyncStats
    , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdConn conn

      -- EpochBoundary
    , resolveCostModel   = EpochBoundary.resolveCostModelConn conn

      -- ScriptsDatums
    , resolveDatum            = ScriptsDatums.resolveDatumConn         conn
    , resolveScript           = ScriptsDatums.resolveScriptConn        conn
    , resolveRedeemerData     = ScriptsDatums.resolveRedeemerDataConn  conn
    , assignRedeemerId        = ScriptsDatums.assignRedeemerIdConn     conn
    , fillSpendScriptHashes   = ScriptsDatums.fillSpendScriptHashesConn conn

      -- Governance
    , resolveDrepHash             = Governance.resolveDrepHashConn        conn
    , resolveCommitteeHash        = Governance.resolveCommitteeHashConn   conn
    , resolveVotingAnchor         = Governance.resolveVotingAnchorConn    conn
    , assignGovActionProposalId   = Governance.assignGovActionProposalIdConn conn
    , assignParamProposalId       = Governance.assignParamProposalIdConn  conn
    , assignCommitteeId           = Governance.assignCommitteeIdConn      conn
    , assignConstitutionId        = Governance.assignConstitutionIdConn   conn
    , assignEventInfoId           = Governance.assignEventInfoIdConn      conn
    , lookupGovActionProposalId   = Governance.lookupGovActionProposalIdConn conn
    , recordGovActionProposalId   = Governance.recordGovActionProposalIdConn
    , readEnactedEpochStateIds    = Governance.readEnactedEpochStateIdsRef  gov
    , writeEnactedEpochStateIds   = Governance.writeEnactedEpochStateIdsRef gov
    , readGovExpiresAfter         = Governance.readGovExpiresAfterRef       gov
    , writeGovExpiresAfter        = Governance.writeGovExpiresAfterRef      gov
    }

-- ---------------------------------------------------------------------------
-- * Buffered resolver
-- ---------------------------------------------------------------------------

-- | Produces the same rows as 'mkFollowResolver'; only the placement
-- of the work differs:
--
--   * @assign*Id@ pops a 'PreAllocatedIds' queue, with no round-trip.
--   * Dedup @resolve*@ checks the per-block cache first, then falls
--     back to @SELECT@ and @nextval@. The 'Writer' queues the INSERT,
--     and the cache shadows the unflushed row.
--   * @resolveAddressId@ returns the id at once and queues the
--     @address@ INSERT on the 'WriteBuffer', so the extractor writes
--     the tx_out row with @address_id@ already filled in.
--   * @recordTxOutputs@ fills a block-local outputs map, so a
--     same-block spend resolves its producer while the tx_out INSERT
--     is still unflushed. @recordConsumed@ queues the consumed-by
--     UPDATE behind it on the same pipeline.
mkBufferedFollowResolver
  :: Conn.Connection
  -> PreAllocatedIds
  -> WriteBuffer
  -> ConsumedTracking
  -> IO (IdResolver IO)
mkBufferedFollowResolver conn preAlloc buf consumedTracking = do
  lastBlock <- newIORef Nothing
  cache     <- newBlockDedupCache
  gov       <- Governance.newGovScratchpad
  pure IdResolver
    { -- Core. The block id stays synchronous, because resolvePrevBlock
      -- needs the materialised value.
      assignBlockId     = Core.assignBlockIdFollow    conn lastBlock
    , assignTxId        = Core.assignTxIdBuf          preAlloc
    , resolveSlotLeader = Core.resolveSlotLeaderBuf   conn cache
    , resolvePrevBlock  = Core.resolvePrevBlockFollow lastBlock
    , lookupLastBlockId = readIORef                    lastBlock

      -- UTxO
    , recordTxOutAddress           = UTxO.recordTxOutAddressFollow
    , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressFollow
    , resolveAddressId             = UTxO.resolveAddressIdBuf            conn buf cache
    , assignTxOutId                = UTxO.assignTxOutIdBuf                preAlloc
    , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdBuf      preAlloc
    , resolveInputValues           = UTxO.resolveInputValuesBuf           conn cache
    , resolveInputUtxo             = UTxO.resolveInputUtxoBuf             conn cache
    , recordTxOutputs              = UTxO.recordTxOutputsBuf              cache
    , recordConsumed               = UTxO.recordConsumedBuf               buf consumedTracking
    , deleteCachedUtxo             = UTxO.deleteCachedUtxoFollow

      -- MultiAsset
    , resolveMultiAsset = MultiAsset.resolveMultiAssetBuf conn cache

      -- StakeDelegation
    , resolveStakeAddress         = Stake.resolveStakeAddressBuf         conn cache

      -- Pool
    , resolvePoolHash         = Pool.resolvePoolHashBuf         conn cache
    , resolvePoolHashQuery    = Pool.resolvePoolHashQueryBuf    conn cache
    , assignPoolUpdateId      = Pool.assignPoolUpdateIdBuf      preAlloc
    , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdBuf preAlloc

      -- OffChainPools
    , enqueuePoolMetaFetch    = OffChainPools.enqueuePoolMetaFetchFollow

      -- OffChainVotes
    , enqueueVoteMetaFetch    = OffChainVotes.enqueueVoteMetaFetchFollow

      -- EpochSyncStats
    , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdBuf conn

      -- EpochBoundary
    , resolveCostModel   = EpochBoundary.resolveCostModelBuf conn cache

      -- ScriptsDatums
    , resolveDatum            = ScriptsDatums.resolveDatumBuf         conn cache
    , resolveScript           = ScriptsDatums.resolveScriptBuf        conn cache
    , resolveRedeemerData     = ScriptsDatums.resolveRedeemerDataBuf  conn cache
    , assignRedeemerId        = ScriptsDatums.assignRedeemerIdBuf     preAlloc
    , fillSpendScriptHashes   = ScriptsDatums.fillSpendScriptHashesBuf buf

      -- Governance
    , resolveDrepHash             = Governance.resolveDrepHashBuf      conn cache
    , resolveCommitteeHash        = Governance.resolveCommitteeHashBuf conn cache
    , resolveVotingAnchor         = Governance.resolveVotingAnchorBuf  conn cache
    , assignGovActionProposalId   = Governance.assignGovActionProposalIdBuf preAlloc
    , assignParamProposalId       = Governance.assignParamProposalIdBuf     preAlloc
    , assignCommitteeId           = Governance.assignCommitteeIdBuf         preAlloc
    , assignConstitutionId        = Governance.assignConstitutionIdBuf      preAlloc
    , assignEventInfoId           = Governance.assignEventInfoIdBuf         conn
    , lookupGovActionProposalId   = Governance.lookupGovActionProposalIdBuf conn cache
    , recordGovActionProposalId   = Governance.recordGovActionProposalIdBuf cache
    , readEnactedEpochStateIds    = Governance.readEnactedEpochStateIdsRef  gov
    , writeEnactedEpochStateIds   = Governance.writeEnactedEpochStateIdsRef gov
    , readGovExpiresAfter         = Governance.readGovExpiresAfterRef       gov
    , writeGovExpiresAfter        = Governance.writeGovExpiresAfterRef      gov
    }
