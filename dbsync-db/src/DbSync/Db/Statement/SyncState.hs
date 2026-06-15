{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @dbsync_sync_state@ singleton
-- row. The schema type 'SyncStateRow' and its encoder/decoder live in
-- 'DbSync.Db.Schema.SyncState'.
module DbSync.Db.Statement.SyncState
  ( seedSyncStateStmt
  , readSyncStateStmt
  , writeSyncStateStmt
  , writeSyncStateSlotStmt
  , markSnapshotCompleteStmt
  , markSyncCompleteStmt
  , readPendingRollbackSlotStmt
  , writePendingRollbackSlotStmt
  , clearPendingRollbackSlotStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.SyncState
  ( SyncStateCols (..)
  , SyncStateRow
  , syncStateCols
  , syncStateRowDecoder
  , syncStateRowEncoder
  , syncStateTableDef
  )
import DbSync.Db.Sql.Refs (col, table)
import DbSync.Db.Statement.Common (arrayParam)

-- | Idempotent seed @INSERT@. Only @schema_version_applied@,
-- @schema_fingerprint@, @ledger_enabled@ and the @extractors@ set come
-- from the caller; other columns use their @DEFAULT@.
seedSyncStateStmt :: Stmt.Statement (Int32, Text, Bool, [Text]) ()
seedSyncStateStmt =
  Stmt.preparable sql encoder D.noResult
  where
    sql = mconcat
      [ "INSERT INTO ", table syncStateTableDef
      , " (", col syncStateCols.sscSchemaVersionApplied
      , ", ", col syncStateCols.sscSchemaFingerprint
      , ", ", col syncStateCols.sscLedgerEnabled
      , ", ", col syncStateCols.sscExtractors, ")"
      , " VALUES ($1, $2, $3, $4)"
      , " ON CONFLICT (", col syncStateCols.sscId, ") DO NOTHING"
      ]
    encoder =
         ((\(v, _, _, _)  -> v)  >$< E.param (E.nonNullable E.int4))
      <> ((\(_, f, _, _)  -> f)  >$< E.param (E.nonNullable E.text))
      <> ((\(_, _, l, _)  -> l)  >$< E.param (E.nonNullable E.bool))
      <> ((\(_, _, _, es) -> es) >$< arrayParam E.text)

-- | 'Nothing' if the singleton has never been seeded.
readSyncStateStmt :: Stmt.Statement () (Maybe SyncStateRow)
readSyncStateStmt =
  Stmt.preparable
    ( "SELECT * FROM " <> table syncStateTableDef
        <> " WHERE " <> col syncStateCols.sscId <> " = 1"
    )
    E.noParams
    (D.rowMaybe syncStateRowDecoder)

-- | Write the consumer-owned columns. Placeholder order matches
-- 'syncStateRowEncoder'. Does __not__ touch @last_snapshot_slot@ or
-- @sync_complete@ — those are owned by 'markSnapshotCompleteStmt' and
-- 'markSyncCompleteStmt'.
writeSyncStateStmt :: Stmt.Statement SyncStateRow Int64
writeSyncStateStmt =
  Stmt.preparable sql syncStateRowEncoder D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table syncStateTableDef, " SET "
      ,    col syncStateCols.sscLastCommittedSlot,           " = $1"
      , ", ", col syncStateCols.sscLastCommittedBlockNo,     " = $2"
      , ", ", col syncStateCols.sscLastCommittedBlockHash,   " = $3"
      , ", ", col syncStateCols.sscBlockIdCounter,           " = $4"
      , ", ", col syncStateCols.sscTxIdCounter,              " = $5"
      , ", ", col syncStateCols.sscTxOutIdCounter,           " = $6"
      , ", ", col syncStateCols.sscSlotLeaderIdCounter,      " = $7"
      , ", ", col syncStateCols.sscAddressIdCounter,         " = $8"
      , ", ", col syncStateCols.sscStakeAddressIdCounter,    " = $9"
      , ", ", col syncStateCols.sscPoolHashIdCounter,        " = $10"
      , ", ", col syncStateCols.sscMultiAssetIdCounter,      " = $11"
      , ", ", col syncStateCols.sscScriptIdCounter,          " = $12"
      , ", ", col syncStateCols.sscPoolUpdateIdCounter,      " = $13"
      , ", ", col syncStateCols.sscPoolMetadataRefIdCounter, " = $14"
      , ", ", col syncStateCols.sscCostModelIdCounter,       " = $15"
      , ", ", col syncStateCols.sscRedeemerIdCounter,        " = $16"
      , ", ", col syncStateCols.sscCollateralTxOutIdCounter, " = $17"
      , ", ", col syncStateCols.sscEpochSyncStatsIdCounter,  " = $18"
      , ", ", col syncStateCols.sscGovActionProposalIdCounter, " = $19"
      , ", ", col syncStateCols.sscParamProposalIdCounter,   " = $20"
      , ", ", col syncStateCols.sscCommitteeIdCounter,       " = $21"
      , ", ", col syncStateCols.sscConstitutionIdCounter,    " = $22"
      , ", ", col syncStateCols.sscEventInfoIdCounter,       " = $23"
      , ", ", col syncStateCols.sscSchemaVersionApplied,     " = $24"
      , ", ", col syncStateCols.sscLedgerEnabled,            " = $25"
      , ", ", col syncStateCols.sscUpdatedAt,                " = now()"
      , " WHERE ", col syncStateCols.sscId, " = 1"
      ]

-- | Advance only the @last_committed_slot/block_no/block_hash@ trio.
-- Used by 'FollowingChainTip' inside each per-block transaction;
-- counter columns aren't touched because Follow allocates IDs via PG
-- sequences (@nextval@) rather than IORef counters.
writeSyncStateSlotStmt :: Stmt.Statement (Word64, Word64, ByteString) Int64
writeSyncStateSlotStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table syncStateTableDef, " SET "
      ,    col syncStateCols.sscLastCommittedSlot,         " = $1"
      , ", ", col syncStateCols.sscLastCommittedBlockNo,   " = $2"
      , ", ", col syncStateCols.sscLastCommittedBlockHash, " = $3"
      , ", ", col syncStateCols.sscUpdatedAt,              " = now()"
      , " WHERE ", col syncStateCols.sscId, " = 1"
      ]
    encoder =
         ((\(s, _, _) -> fromIntegral s) >$< E.param (E.nonNullable E.int8))
      <> ((\(_, b, _) -> fromIntegral b) >$< E.param (E.nonNullable E.int8))
      <> ((\(_, _, h) -> h)              >$< E.param (E.nonNullable E.bytea))

-- | Record a successful ledger-snapshot write. Owned by the
-- snapshot-writer thread.
markSnapshotCompleteStmt :: Stmt.Statement Word64 Int64
markSnapshotCompleteStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table syncStateTableDef, " SET "
      ,    col syncStateCols.sscLastSnapshotSlot, " = $1"
      , ", ", col syncStateCols.sscUpdatedAt,     " = now()"
      , " WHERE ", col syncStateCols.sscId, " = 1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Flip @sync_complete@ true at the Ingest → Follow transition.
markSyncCompleteStmt :: Stmt.Statement () Int64
markSyncCompleteStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table syncStateTableDef, " SET "
      ,    col syncStateCols.sscSyncComplete, " = true"
      , ", ", col syncStateCols.sscUpdatedAt, " = now()"
      , " WHERE ", col syncStateCols.sscId, " = 1"
      ]

-- | 'Nothing' = no rollback pending; 'Just' = boot must run a
-- rollback to that slot before normal resume.
readPendingRollbackSlotStmt :: Stmt.Statement () (Maybe Word64)
readPendingRollbackSlotStmt =
  Stmt.preparable
    ( "SELECT " <> col syncStateCols.sscPendingRollbackSlot
        <> " FROM " <> table syncStateTableDef
        <> " WHERE " <> col syncStateCols.sscId <> " = 1"
    )
    E.noParams
    (D.singleRow (fmap fromIntegral <$> D.column (D.nullable D.int8)))

-- | Persist a pending rollback target so recovery survives a process
-- restart. Written by the ledger worker on deep rollback, and by the
-- @--rollback-to-slot@ CLI before the cascade runs (so a mid-rollback
-- crash resumes cleanly).
writePendingRollbackSlotStmt :: Stmt.Statement Word64 Int64
writePendingRollbackSlotStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table syncStateTableDef, " SET "
      ,    col syncStateCols.sscPendingRollbackSlot, " = $1"
      , ", ", col syncStateCols.sscUpdatedAt,        " = now()"
      , " WHERE ", col syncStateCols.sscId, " = 1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

clearPendingRollbackSlotStmt :: Stmt.Statement () Int64
clearPendingRollbackSlotStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table syncStateTableDef, " SET "
      ,    col syncStateCols.sscPendingRollbackSlot, " = NULL"
      , ", ", col syncStateCols.sscUpdatedAt,        " = now()"
      , " WHERE ", col syncStateCols.sscId, " = 1"
      ]
