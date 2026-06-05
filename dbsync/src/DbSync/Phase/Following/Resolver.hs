-- | FollowingChainTip ID resolver.
--
-- Two implementations:
--
-- * 'mkFollowResolver' — every @assign*Id@ does a @nextval@
--   round-trip; every @resolve*@ does a @SELECT@ then a @nextval@
--   on miss. Used by the integration test suite.
--
-- * 'mkBufferedFollowResolver' — @assign*Id@ pops from a queue of
--   IDs pre-allocated in one pipeline at start of block;
--   @resolve*@ still SELECTs synchronously but checks a per-block
--   in-process map first (so a SELECT seeing a sibling's
--   not-yet-flushed INSERT still finds it). @resolveAddressId@
--   returns the id synchronously and queues the @address@ INSERT
--   (when new) on the shared 'WriteBuffer'; the caller then writes
--   the tx_out row with @address_id@ already populated. Used in
--   production.
--
-- Both share the same dedup contracts and the same FK invariants;
-- the diff test confirms identical rows in PG.
--
-- Per-extractor method bodies live in @Resolver\/\<extractor\>.hs@;
-- this module composes them.
module DbSync.Phase.Following.Resolver
  ( mkFollowResolver
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
import qualified DbSync.Phase.Following.Resolver.Pool as Pool
import qualified DbSync.Phase.Following.Resolver.ScriptsDatums as ScriptsDatums
import qualified DbSync.Phase.Following.Resolver.StakeDelegation as Stake
import qualified DbSync.Phase.Following.Resolver.UTxO as UTxO
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Resolver (IdResolver (..))

-- ---------------------------------------------------------------------------
-- * Direct resolver
-- ---------------------------------------------------------------------------

-- | Build a direct (un-buffered) Follow resolver. Every assigner
-- does a @nextval@ round-trip; every dedup resolver does
-- @SELECT@ then @nextval@ on miss.
mkFollowResolver :: Conn.Connection -> IO (IdResolver IO)
mkFollowResolver conn = do
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
    , resolveInputValues           = UTxO.resolveInputValuesFollow      conn
    , resolveInputUtxo             = UTxO.resolveInputUtxoFollow        conn
    , recordTxOutputs              = UTxO.recordTxOutputsFollow
    , recordConsumed               = UTxO.recordConsumedFollow
    , deleteCachedUtxo             = UTxO.deleteCachedUtxoFollow

      -- MultiAsset
    , resolveMultiAsset = MultiAsset.resolveMultiAssetConn conn

      -- StakeDelegation
    , resolveStakeAddress         = Stake.resolveStakeAddressConn         conn

      -- Pool
    , resolvePoolHash         = Pool.resolvePoolHashConn         conn
    , assignPoolUpdateId      = Pool.assignPoolUpdateIdConn      conn
    , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdConn conn

      -- EpochSyncStats
    , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdConn conn

      -- EpochBoundary
    , resolveCostModel   = EpochBoundary.resolveCostModelConn conn

      -- ScriptsDatums
    , resolveDatum            = ScriptsDatums.resolveDatumConn         conn
    , resolveScript           = ScriptsDatums.resolveScriptConn        conn
    , resolveRedeemerData     = ScriptsDatums.resolveRedeemerDataConn  conn
    , assignRedeemerId        = ScriptsDatums.assignRedeemerIdConn     conn

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

-- | Buffered Follow resolver. Same observable rows as
-- 'mkFollowResolver'; the difference is where the work lands:
--
--   * @assign*Id@ pops from per-sequence queues in 'PreAllocatedIds'
--     (zero round-trips).
--   * Dedup @resolve*@ checks the per-block cache first; on miss
--     does @SELECT@ then @nextval@. The corresponding INSERT is
--     queued via the 'Writer' as today; the per-block cache shadows
--     the not-yet-flushed row.
--   * @resolveAddressId@ resolves synchronously, queuing the
--     @address@ INSERT (when new) on the shared 'WriteBuffer'. The
--     extractor writes the tx_out row with @address_id@ filled in.
mkBufferedFollowResolver
  :: Conn.Connection
  -> PreAllocatedIds
  -> WriteBuffer
  -> IO (IdResolver IO)
mkBufferedFollowResolver conn preAlloc buf = do
  lastBlock <- newIORef Nothing
  cache     <- newBlockDedupCache
  gov       <- Governance.newGovScratchpad
  pure IdResolver
    { -- Core (block ID stays synchronous because resolvePrevBlock needs
      -- the materialised value)
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
    , resolveInputValues           = UTxO.resolveInputValuesFollow         conn
    , resolveInputUtxo             = UTxO.resolveInputUtxoFollow           conn
    , recordTxOutputs              = UTxO.recordTxOutputsFollow
    , recordConsumed               = UTxO.recordConsumedFollow
    , deleteCachedUtxo             = UTxO.deleteCachedUtxoFollow

      -- MultiAsset
    , resolveMultiAsset = MultiAsset.resolveMultiAssetBuf conn cache

      -- StakeDelegation
    , resolveStakeAddress         = Stake.resolveStakeAddressBuf         conn cache

      -- Pool
    , resolvePoolHash         = Pool.resolvePoolHashBuf         conn cache
    , assignPoolUpdateId      = Pool.assignPoolUpdateIdBuf      preAlloc
    , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdBuf preAlloc

      -- EpochSyncStats
    , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdBuf conn

      -- EpochBoundary
    , resolveCostModel   = EpochBoundary.resolveCostModelBuf conn cache

      -- ScriptsDatums
    , resolveDatum            = ScriptsDatums.resolveDatumBuf         conn cache
    , resolveScript           = ScriptsDatums.resolveScriptBuf        conn cache
    , resolveRedeemerData     = ScriptsDatums.resolveRedeemerDataBuf  conn cache
    , assignRedeemerId        = ScriptsDatums.assignRedeemerIdBuf     preAlloc

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
