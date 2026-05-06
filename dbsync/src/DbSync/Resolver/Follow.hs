{-# LANGUAGE OverloadedStrings #-}

-- | FollowingChainTip ID resolver.
--
-- IDs come from PG sequences via @nextval@; dedup tables are
-- resolved with a SELECT-then-nextval. Same shape as the
-- 'IngestChainHistory' resolver, different id source.
--
-- Tables outside the slice-A surface (block, slot_leader) panic;
-- they're filled in as their extractors land in tests.
module DbSync.Resolver.Follow
  ( mkFollowResolver
  ) where

import Cardano.Prelude

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids
import DbSync.Db.Statement.Block (nextBlockIdStmt)
import DbSync.Db.Statement.CollateralTxIn (nextCollateralTxInIdStmt)
import DbSync.Db.Statement.Delegation (nextDelegationIdStmt)
import DbSync.Db.Statement.MaTxMint (nextMaTxMintIdStmt)
import DbSync.Db.Statement.MaTxOut (nextMaTxOutIdStmt)
import DbSync.Db.Statement.MultiAsset
  ( nextMultiAssetIdStmt
  , queryMultiAssetIdStmt
  )
import DbSync.Db.Schema.MultiAsset (multiAssetName, multiAssetPolicy)
import DbSync.Db.Statement.PoolHash (nextPoolHashIdStmt, queryPoolHashIdStmt)
import DbSync.Db.Statement.PoolMetadataRef (nextPoolMetadataRefIdStmt)
import DbSync.Db.Statement.PoolOwner (nextPoolOwnerIdStmt)
import DbSync.Db.Statement.PoolRelay (nextPoolRelayIdStmt)
import DbSync.Db.Statement.PoolRetire (nextPoolRetireIdStmt)
import DbSync.Db.Statement.PoolUpdate (nextPoolUpdateIdStmt)
import DbSync.Db.Statement.ReferenceTxIn (nextReferenceTxInIdStmt)
import DbSync.Db.Statement.SlotLeader
  ( nextSlotLeaderIdStmt
  , querySlotLeaderIdStmt
  )
import DbSync.Db.Statement.StakeAddress
  ( nextStakeAddressIdStmt
  , queryStakeAddressIdStmt
  )
import DbSync.Db.Statement.StakeDeregistration (nextStakeDeregistrationIdStmt)
import DbSync.Db.Statement.StakeRegistration (nextStakeRegistrationIdStmt)
import DbSync.Db.Statement.Tx (nextTxIdStmt)
import DbSync.Db.Statement.TxCbor (nextTxCborIdStmt)
import DbSync.Db.Statement.TxIn (nextTxInIdStmt)
import DbSync.Db.Statement.TxMetadata (nextTxMetadataIdStmt)
import DbSync.Db.Statement.TxOut (nextTxOutIdStmt)
import DbSync.Db.Statement.Withdrawal (nextWithdrawalIdStmt)
import DbSync.Resolver (IdResolver (..))

mkFollowResolver :: Conn.Connection -> IO (IdResolver IO)
mkFollowResolver conn = do
  lastBlock <- newIORef Nothing
  pure $ resolver conn lastBlock

resolver :: Conn.Connection -> IORef (Maybe BlockId) -> IdResolver IO
resolver conn lastBlock = IdResolver
  { assignBlockId = do
      bid <- run conn () nextBlockIdStmt
      writeIORef lastBlock (Just bid)
      pure bid

  , resolveSlotLeader = \hash _leader -> do
      mId <- run conn hash querySlotLeaderIdStmt
      case mId of
        Just sid -> pure (sid, False)
        Nothing  -> do
          sid <- run conn () nextSlotLeaderIdStmt
          pure (sid, True)

  , resolvePrevBlock = \_hash -> readIORef lastBlock

  , assignTxId = run conn () nextTxIdStmt

    -- UTxO IDs (no resolver-side dedup — straight nextval per row)
  , assignTxOutId            = run conn () nextTxOutIdStmt
  , assignTxInId             = run conn () nextTxInIdStmt
  , assignCollateralTxInId   = run conn () nextCollateralTxInIdStmt
  , assignReferenceTxInId    = run conn () nextReferenceTxInIdStmt

    -- Metadata IDs (no resolver-side dedup)
  , assignTxMetadataId       = run conn () nextTxMetadataIdStmt

    -- MultiAsset IDs.
    -- 'multi_asset' is dedup-keyed by (policy, name) — SELECT first,
    -- nextval on miss. The dedup key handed in by the extractor (a
    -- 'ShortByteString' formed from policy ++ name) is ignored here;
    -- we use the structured policy / name fields for the SELECT.
  , resolveMultiAsset = \_key ma -> do
      mId <- run conn (multiAssetPolicy ma, multiAssetName ma)
                     queryMultiAssetIdStmt
      case mId of
        Just maId -> pure (maId, False)
        Nothing   -> do
          maId <- run conn () nextMultiAssetIdStmt
          pure (maId, True)
  , assignMaTxMintId         = run conn () nextMaTxMintIdStmt
  , assignMaTxOutId          = run conn () nextMaTxOutIdStmt

    -- StakeDelegation IDs.
    -- 'stake_address' deduplicates by 28-byte credential hash. The
    -- resolver mirrors the slot_leader / multi_asset pattern: SELECT
    -- by hash, allocate from the sequence on miss.
  , resolveStakeAddress = \hash _sa -> do
      mId <- run conn hash queryStakeAddressIdStmt
      case mId of
        Just saId -> pure (saId, False)
        Nothing   -> do
          saId <- run conn () nextStakeAddressIdStmt
          pure (saId, True)
  , assignStakeRegistrationId   = run conn () nextStakeRegistrationIdStmt
  , assignStakeDeregistrationId = run conn () nextStakeDeregistrationIdStmt
  , assignDelegationId          = run conn () nextDelegationIdStmt
  , assignWithdrawalId          = run conn () nextWithdrawalIdStmt

    -- Pool IDs.
    -- 'pool_hash' deduplicates by 28-byte pool key hash. SELECT first,
    -- nextval on miss; same shape as 'stake_address' / 'multi_asset'.
  , resolvePoolHash = \hash _ph -> do
      mId <- run conn hash queryPoolHashIdStmt
      case mId of
        Just phId -> pure (phId, False)
        Nothing   -> do
          phId <- run conn () nextPoolHashIdStmt
          pure (phId, True)
  , assignPoolUpdateId       = run conn () nextPoolUpdateIdStmt
  , assignPoolMetadataRefId  = run conn () nextPoolMetadataRefIdStmt
  , assignPoolOwnerId        = run conn () nextPoolOwnerIdStmt
  , assignPoolRetireId       = run conn () nextPoolRetireIdStmt
  , assignPoolRelayId        = run conn () nextPoolRelayIdStmt

    -- CBOR IDs (no resolver-side dedup)
  , assignTxCborId           = run conn () nextTxCborIdStmt

  -- Filled in once their extractors gain test coverage. The IO
  -- actions defer evaluation so unused fields don't crash record
  -- construction.
  , assignEpochSyncStatsId   = todo "assignEpochSyncStatsId"
  , assignAdaPotsId          = todo "assignAdaPotsId"
  }

run :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
run conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left e  -> panic $ "Follow resolver session failed: " <> show e

todo :: Text -> IO a
todo name = pure $ panic $ "Resolver.Follow." <> name <> " not yet implemented"
