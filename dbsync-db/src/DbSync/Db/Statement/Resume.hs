{-# LANGUAGE OverloadedStrings #-}

-- | Parameterised hasql 'Statement's used at boot to clean up rows
-- past the resume point.
--
-- Each statement is built per-table (table name interpolated into
-- the SQL); the @WHERE@ predicate is parameterised. The @$1@
-- placeholder carries the slot number or counter as appropriate.
--
-- Table names come from 'tdName' on the static 'TableDef's defined
-- by this package, so identifier interpolation is safe — no
-- user-controlled input flows into the SQL.
module DbSync.Db.Statement.Resume
  ( -- * Cleanup deletes
    deleteBySlotStmt
  , deleteByBlockSlotStmt
  , deleteByIdCounterStmt

    -- * Dedup-map rebuild selects
  , selectDedupSingleStmt
  , selectMultiAssetDedupStmt

    -- * Boot-time canonicalisation
  , selectBlockHashAtSlotStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Sql (quoteIdent)

-- | @DELETE FROM <table> WHERE slot_no > $1@. Returns rows affected.
-- For tables that carry @slot_no@ directly.
deleteBySlotStmt :: Text -> Stmt.Statement Word64 Int64
deleteBySlotStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE slot_no > $1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | @DELETE FROM <table> WHERE block_id IN (SELECT id FROM block
-- WHERE slot_no > $1)@. Returns rows affected. For tables that
-- reference @block@ via @block_id@ but don't carry @slot_no@.
deleteByBlockSlotStmt :: Text -> Stmt.Statement Word64 Int64
deleteByBlockSlotStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE block_id IN (SELECT id FROM \"block\" WHERE slot_no > $1)"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | @DELETE FROM <table> WHERE id >= $1@. Returns rows affected.
-- The @$1@ parameter is the corresponding @*_id_counter@ from
-- @dbsync_sync_state@ (\"next id to assign\"); the resume cleanup
-- uses this to prune rows the previous run wrote past the
-- last-recorded counter — covers both dedup tables and any other
-- counter-tracked data table that lacks a slot or block reference.
deleteByIdCounterStmt :: Text -> Stmt.Statement Int64 Int64
deleteByIdCounterStmt tableName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE id >= $1"
      ]
    encoder = E.param (E.nonNullable E.int8)

-- | @SELECT id, <keyCol> FROM <table>@. Returns @(id, key)@ pairs
-- as a list. For dedup tables whose natural key is a single column
-- (slot_leader.hash, stake_address.hash_raw, pool_hash.hash_raw).
selectDedupSingleStmt :: Text -> Text -> Stmt.Statement () [(Int64, ByteString)]
selectDedupSingleStmt tableName keyCol =
  Stmt.unpreparable sql E.noParams decoder
  where
    sql = T.concat
      [ "SELECT id, ", quoteIdent keyCol
      , " FROM ", quoteIdent tableName
      ]
    decoder = D.rowList $
      (,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)

-- | @SELECT id, policy, name FROM multi_asset@. The dedup key is
-- @policy <> name@; the wrapper concatenates after decoding.
selectMultiAssetDedupStmt :: Stmt.Statement () [(Int64, ByteString, ByteString)]
selectMultiAssetDedupStmt =
  Stmt.unpreparable
    "SELECT id, policy, name FROM \"multi_asset\""
    E.noParams
    decoder
  where
    decoder = D.rowList $
      (,,)
        <$> D.column (D.nonNullable D.int8)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable D.bytea)

-- | @SELECT hash FROM block WHERE slot_no = $1 LIMIT 1@. Used by
-- 'DbSync.Checkpoint.SyncState.fetchBlockHashAtSlot' at boot.
selectBlockHashAtSlotStmt :: Stmt.Statement Word64 (Maybe ByteString)
selectBlockHashAtSlotStmt =
  Stmt.preparable
    "SELECT hash FROM \"block\" WHERE slot_no = $1 LIMIT 1"
    encoder
    (D.rowMaybe (D.column (D.nonNullable D.bytea)))
  where
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)
