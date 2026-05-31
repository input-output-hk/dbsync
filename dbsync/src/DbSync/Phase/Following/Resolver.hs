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

import Data.IORef (newIORef)

import qualified Hasql.Connection as Conn

import qualified DbSync.Phase.Following.Resolver.Cbor as Cbor
import qualified DbSync.Phase.Following.Resolver.Core as Core
import qualified DbSync.Phase.Following.Resolver.Epoch as Epoch
import qualified DbSync.Phase.Following.Resolver.EpochBoundary as EpochBoundary
import DbSync.Phase.Following.Resolver.Internal (newBlockDedupCache)
import qualified DbSync.Phase.Following.Resolver.Metadata as Metadata
import qualified DbSync.Phase.Following.Resolver.MultiAsset as MultiAsset
import qualified DbSync.Phase.Following.Resolver.Pool as Pool
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
  pure IdResolver
    { -- Core
      assignBlockId     = Core.assignBlockIdFollow    conn lastBlock
    , assignTxId        = Core.assignTxIdConn         conn
    , resolveSlotLeader = Core.resolveSlotLeaderConn  conn
    , resolvePrevBlock  = Core.resolvePrevBlockFollow lastBlock

      -- UTxO
    , recordTxOutAddress           = UTxO.recordTxOutAddressFollow
    , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressFollow
    , resolveAddressId             = UTxO.resolveAddressIdConn         conn
    , assignTxOutId                = UTxO.assignTxOutIdConn             conn
    , assignTxInId                 = UTxO.assignTxInIdConn              conn
    , assignCollateralTxInId       = UTxO.assignCollateralTxInIdConn    conn
    , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdConn   conn
    , assignReferenceTxInId        = UTxO.assignReferenceTxInIdConn     conn
    , resolveInputValues           = UTxO.resolveInputValuesFollow      conn
    , resolveInputUtxo             = UTxO.resolveInputUtxoFollow        conn
    , recordTxOutputs              = UTxO.recordTxOutputsFollow
    , recordConsumed               = UTxO.recordConsumedFollow
    , deleteCachedUtxo             = UTxO.deleteCachedUtxoFollow

      -- Metadata
    , assignTxMetadataId = Metadata.assignTxMetadataIdConn conn

      -- MultiAsset
    , resolveMultiAsset = MultiAsset.resolveMultiAssetConn conn
    , assignMaTxMintId  = MultiAsset.assignMaTxMintIdConn  conn
    , assignMaTxOutId   = MultiAsset.assignMaTxOutIdConn   conn

      -- StakeDelegation
    , resolveStakeAddress         = Stake.resolveStakeAddressConn         conn
    , assignStakeRegistrationId   = Stake.assignStakeRegistrationIdConn   conn
    , assignStakeDeregistrationId = Stake.assignStakeDeregistrationIdConn conn
    , assignDelegationId          = Stake.assignDelegationIdConn          conn
    , assignWithdrawalId          = Stake.assignWithdrawalIdConn          conn
    , assignPotTransferId         = Stake.assignPotTransferIdStub
    , assignTreasuryId            = Stake.assignTreasuryIdStub
    , assignReserveId             = Stake.assignReserveIdStub

      -- Pool
    , resolvePoolHash         = Pool.resolvePoolHashConn         conn
    , assignPoolUpdateId      = Pool.assignPoolUpdateIdConn      conn
    , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdConn conn
    , assignPoolOwnerId       = Pool.assignPoolOwnerIdConn       conn
    , assignPoolRetireId      = Pool.assignPoolRetireIdConn      conn
    , assignPoolRelayId       = Pool.assignPoolRelayIdConn       conn

      -- CBOR
    , assignTxCborId = Cbor.assignTxCborIdConn conn

      -- EpochSyncStats (stub)
    , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdStub

      -- EpochBoundary (stubs)
    , assignAdaPotsId    = EpochBoundary.assignAdaPotsIdStub
    , assignEpochParamId = EpochBoundary.assignEpochParamIdStub
    , assignEpochStateId = EpochBoundary.assignEpochStateIdStub
    , resolveCostModel   = EpochBoundary.resolveCostModelStub
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
  pure IdResolver
    { -- Core (block ID stays synchronous because resolvePrevBlock needs
      -- the materialised value)
      assignBlockId     = Core.assignBlockIdFollow    conn lastBlock
    , assignTxId        = Core.assignTxIdBuf          preAlloc
    , resolveSlotLeader = Core.resolveSlotLeaderBuf   conn cache
    , resolvePrevBlock  = Core.resolvePrevBlockFollow lastBlock

      -- UTxO
    , recordTxOutAddress           = UTxO.recordTxOutAddressFollow
    , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressFollow
    , resolveAddressId             = UTxO.resolveAddressIdBuf            conn buf cache
    , assignTxOutId                = UTxO.assignTxOutIdBuf                preAlloc
    , assignTxInId                 = UTxO.assignTxInIdBuf                 preAlloc
    , assignCollateralTxInId       = UTxO.assignCollateralTxInIdBuf       preAlloc
    , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdBuf      preAlloc
    , assignReferenceTxInId        = UTxO.assignReferenceTxInIdBuf        preAlloc
    , resolveInputValues           = UTxO.resolveInputValuesFollow         conn
    , resolveInputUtxo             = UTxO.resolveInputUtxoFollow           conn
    , recordTxOutputs              = UTxO.recordTxOutputsFollow
    , recordConsumed               = UTxO.recordConsumedFollow
    , deleteCachedUtxo             = UTxO.deleteCachedUtxoFollow

      -- Metadata
    , assignTxMetadataId = Metadata.assignTxMetadataIdBuf preAlloc

      -- MultiAsset
    , resolveMultiAsset = MultiAsset.resolveMultiAssetBuf conn cache
    , assignMaTxMintId  = MultiAsset.assignMaTxMintIdBuf  preAlloc
    , assignMaTxOutId   = MultiAsset.assignMaTxOutIdBuf   preAlloc

      -- StakeDelegation
    , resolveStakeAddress         = Stake.resolveStakeAddressBuf         conn cache
    , assignStakeRegistrationId   = Stake.assignStakeRegistrationIdBuf   preAlloc
    , assignStakeDeregistrationId = Stake.assignStakeDeregistrationIdBuf preAlloc
    , assignDelegationId          = Stake.assignDelegationIdBuf          preAlloc
    , assignWithdrawalId          = Stake.assignWithdrawalIdBuf          preAlloc
    , assignPotTransferId         = Stake.assignPotTransferIdStub
    , assignTreasuryId            = Stake.assignTreasuryIdStub
    , assignReserveId             = Stake.assignReserveIdStub

      -- Pool
    , resolvePoolHash         = Pool.resolvePoolHashBuf         conn cache
    , assignPoolUpdateId      = Pool.assignPoolUpdateIdBuf      preAlloc
    , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdBuf preAlloc
    , assignPoolOwnerId       = Pool.assignPoolOwnerIdBuf       preAlloc
    , assignPoolRetireId      = Pool.assignPoolRetireIdBuf      preAlloc
    , assignPoolRelayId       = Pool.assignPoolRelayIdBuf       preAlloc

      -- CBOR
    , assignTxCborId = Cbor.assignTxCborIdBuf preAlloc

      -- EpochSyncStats (stub)
    , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdStub

      -- EpochBoundary (stubs)
    , assignAdaPotsId    = EpochBoundary.assignAdaPotsIdStub
    , assignEpochParamId = EpochBoundary.assignEpochParamIdStub
    , assignEpochStateId = EpochBoundary.assignEpochStateIdStub
    , resolveCostModel   = EpochBoundary.resolveCostModelStub
    }
