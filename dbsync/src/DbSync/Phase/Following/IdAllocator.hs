{-# LANGUAGE OverloadedStrings #-}

-- | Pre-allocate every assignable ID a block will need in a single
-- libpq pipeline round-trip.
--
-- Replaces the per-row @SELECT nextval(seq)@ call site by:
--
--   1. Walking the block once via 'IdCounts.countAssignableIds'.
--   2. Issuing one @SELECT nextval(seq) FROM generate_series(1, N)@
--      per non-zero sequence, batched as a single 'Hasql.Pipeline'.
--   3. Stashing the returned IDs in per-table 'IORef' queues that
--      the Follow 'IdResolver' pops from at zero round-trips.
--
-- For a block that touches ten sequences (block, tx, tx_out, …),
-- the whole allocation costs one network round-trip instead of
-- one per row.
module DbSync.Phase.Following.IdAllocator
  ( PreAllocatedIds (..)
  , allocateAllIds
  , popHead
  , unused
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess

import DbSync.Db.Schema.Ids
  ( CollateralTxInId (..)
  , CollateralTxOutId (..)
  , DelegationId (..)
  , MaTxMintId (..)
  , MaTxOutId (..)
  , PoolMetadataRefId (..)
  , PoolOwnerId (..)
  , PoolRelayId (..)
  , PoolRetireId (..)
  , PoolUpdateId (..)
  , ReferenceTxInId (..)
  , StakeDeregistrationId (..)
  , StakeRegistrationId (..)
  , TxCborId (..)
  , TxId (..)
  , TxInId (..)
  , TxMetadataId (..)
  , TxOutId (..)
  , WithdrawalId (..)
  )
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Statement.IdAllocator (bulkNextvalStmt)
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Schema.Core (txTableDef)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Metadata (txMetadataTableDef)
import DbSync.Db.Schema.MultiAsset (maTxMintTableDef, maTxOutTableDef)
import DbSync.Db.Schema.Pool
  ( poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( delegationTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Phase.Following.IdCounts (IdCounts (..))

-- | Per-sequence FIFO of IDs allocated for the current block.
--
-- One 'IORef' per sequence so the resolver can pop without
-- contending across tables. Each queue is filled exactly once by
-- 'allocateAllIds' and drained exactly once by the extractor pass;
-- 'popHead' panics on empty so a miscounted block fails loudly.
data PreAllocatedIds = PreAllocatedIds
  { paiTxIds                 :: !(IORef [TxId])
  , paiTxOutIds              :: !(IORef [TxOutId])
  , paiTxInIds               :: !(IORef [TxInId])
  , paiCollateralTxInIds     :: !(IORef [CollateralTxInId])
  , paiCollateralTxOutIds    :: !(IORef [CollateralTxOutId])
  , paiReferenceTxInIds      :: !(IORef [ReferenceTxInId])
  , paiTxMetadataIds         :: !(IORef [TxMetadataId])
  , paiMaTxMintIds           :: !(IORef [MaTxMintId])
  , paiMaTxOutIds            :: !(IORef [MaTxOutId])
  , paiTxCborIds             :: !(IORef [TxCborId])
  , paiStakeRegistrationIds  :: !(IORef [StakeRegistrationId])
  , paiStakeDeregistrationIds :: !(IORef [StakeDeregistrationId])
  , paiDelegationIds         :: !(IORef [DelegationId])
  , paiWithdrawalIds         :: !(IORef [WithdrawalId])
  , paiPoolUpdateIds         :: !(IORef [PoolUpdateId])
  , paiPoolMetadataRefIds    :: !(IORef [PoolMetadataRefId])
  , paiPoolOwnerIds          :: !(IORef [PoolOwnerId])
  , paiPoolRetireIds         :: !(IORef [PoolRetireId])
  , paiPoolRelayIds          :: !(IORef [PoolRelayId])
  }

-- | Pop the next ID from a per-sequence queue. Panics if the
-- extractor pass requests more IDs than 'allocateAllIds'
-- pre-fetched — that's a counting bug in 'IdCounts' and we want
-- the test suite to catch it loudly.
popHead :: Text -> IORef [a] -> IO a
popHead label ref =
  atomicModifyIORef' ref $ \case
    (x : xs) -> (xs, x)
    []       -> panic $
      "IdAllocator.popHead: queue exhausted for " <> label
        <> ". IdCounts undercount; investigate the count walker."

-- | Number of unconsumed IDs left in a queue. Tests use this to
-- assert the extractor consumed exactly what was pre-allocated.
unused :: IORef [a] -> IO Int
unused ref = length <$> readIORef ref

-- | Allocate every ID the block will need in one pipeline batch.
--
-- The pipeline composes ~19 statements as one Applicative chain so
-- libpq sends them all in a single network round-trip. Each
-- non-zero count yields a @SELECT nextval(seq) FROM
-- generate_series(1, $1)@ that the pipeline batches alongside the
-- others. Zero-count sequences are skipped (no statement issued).
allocateAllIds :: Conn.Connection -> IdCounts -> IO PreAllocatedIds
allocateAllIds conn counts = do
  let pipeline =
        AllocatedIdsRaw
          <$> allocFor txTableDef                  (icTxIds counts)                  TxId
          <*> allocFor txOutTableDef               (icTxOutIds counts)               TxOutId
          <*> allocFor txInTableDef                (icTxInIds counts)                TxInId
          <*> allocFor collateralTxInTableDef      (icCollateralTxInIds counts)      CollateralTxInId
          <*> allocFor collateralTxOutTableDef     (icCollateralTxOutIds counts)     CollateralTxOutId
          <*> allocFor referenceTxInTableDef       (icReferenceTxInIds counts)       ReferenceTxInId
          <*> allocFor txMetadataTableDef          (icTxMetadataIds counts)          TxMetadataId
          <*> allocFor maTxMintTableDef            (icMaTxMintIds counts)            MaTxMintId
          <*> allocFor maTxOutTableDef             (icMaTxOutIds counts)             MaTxOutId
          <*> allocFor txCborTableDef              (icTxCborIds counts)              TxCborId
          <*> allocFor stakeRegistrationTableDef   (icStakeRegistrationIds counts)   StakeRegistrationId
          <*> allocFor stakeDeregistrationTableDef (icStakeDeregistrationIds counts) StakeDeregistrationId
          <*> allocFor delegationTableDef          (icDelegationIds counts)          DelegationId
          <*> allocFor withdrawalTableDef          (icWithdrawalIds counts)          WithdrawalId
          <*> allocFor poolUpdateTableDef          (icPoolUpdateIds counts)          PoolUpdateId
          <*> allocFor poolMetadataRefTableDef     (icPoolMetadataRefIds counts)     PoolMetadataRefId
          <*> allocFor poolOwnerTableDef           (icPoolOwnerIds counts)           PoolOwnerId
          <*> allocFor poolRetireTableDef          (icPoolRetireIds counts)          PoolRetireId
          <*> allocFor poolRelayTableDef           (icPoolRelayIds counts)           PoolRelayId

  raw <- runOrPanic =<< Conn.use conn (Sess.pipeline pipeline)
  wrapInRefs raw

-- | Intermediate record carrying the raw lists returned by the
-- pipeline. Converted into the 'IORef'-of-queue shape by
-- 'wrapInRefs' so the extractor can pop in-place.
data AllocatedIdsRaw = AllocatedIdsRaw
  { rTxIds                 :: ![TxId]
  , rTxOutIds              :: ![TxOutId]
  , rTxInIds               :: ![TxInId]
  , rCollateralTxInIds     :: ![CollateralTxInId]
  , rCollateralTxOutIds    :: ![CollateralTxOutId]
  , rReferenceTxInIds      :: ![ReferenceTxInId]
  , rTxMetadataIds         :: ![TxMetadataId]
  , rMaTxMintIds           :: ![MaTxMintId]
  , rMaTxOutIds            :: ![MaTxOutId]
  , rTxCborIds             :: ![TxCborId]
  , rStakeRegistrationIds  :: ![StakeRegistrationId]
  , rStakeDeregistrationIds :: ![StakeDeregistrationId]
  , rDelegationIds         :: ![DelegationId]
  , rWithdrawalIds         :: ![WithdrawalId]
  , rPoolUpdateIds         :: ![PoolUpdateId]
  , rPoolMetadataRefIds    :: ![PoolMetadataRefId]
  , rPoolOwnerIds          :: ![PoolOwnerId]
  , rPoolRetireIds         :: ![PoolRetireId]
  , rPoolRelayIds          :: ![PoolRelayId]
  }

wrapInRefs :: AllocatedIdsRaw -> IO PreAllocatedIds
wrapInRefs r =
  PreAllocatedIds
    <$> newIORef (rTxIds r)
    <*> newIORef (rTxOutIds r)
    <*> newIORef (rTxInIds r)
    <*> newIORef (rCollateralTxInIds r)
    <*> newIORef (rCollateralTxOutIds r)
    <*> newIORef (rReferenceTxInIds r)
    <*> newIORef (rTxMetadataIds r)
    <*> newIORef (rMaTxMintIds r)
    <*> newIORef (rMaTxOutIds r)
    <*> newIORef (rTxCborIds r)
    <*> newIORef (rStakeRegistrationIds r)
    <*> newIORef (rStakeDeregistrationIds r)
    <*> newIORef (rDelegationIds r)
    <*> newIORef (rWithdrawalIds r)
    <*> newIORef (rPoolUpdateIds r)
    <*> newIORef (rPoolMetadataRefIds r)
    <*> newIORef (rPoolOwnerIds r)
    <*> newIORef (rPoolRetireIds r)
    <*> newIORef (rPoolRelayIds r)

runOrPanic :: Show e => Either e a -> IO a
runOrPanic = \case
  Right x  -> pure x
  Left err -> panic $ "IdAllocator.allocateAllIds: " <> show err

-- | Issue one bulk @nextval@ call for the given table's sequence.
--
-- Zero-count case is a no-op: no pipeline statement issued, an empty
-- list returned immediately via 'pure'.
--
-- Non-zero case: @SELECT nextval(seq) FROM generate_series(1, $1)@
-- returns N rows in one statement; the pipeline batches it with
-- the other tables' allocations. The SQL itself lives in
-- 'DbSync.Db.Statement.IdAllocator'.
allocFor
  :: TableDef
  -> Int                          -- ^ How many IDs to allocate (may be 0).
  -> (Int64 -> a)                 -- ^ ID constructor.
  -> Pipeline.Pipeline [a]
allocFor _  0 _    = pure []
allocFor td n ctor =
  Pipeline.statement (fromIntegral n) (bulkNextvalStmt td ctor)
