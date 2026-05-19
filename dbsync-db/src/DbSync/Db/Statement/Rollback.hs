{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the 'FollowingChainTip' rollback
-- cascade.
--
-- Mirrors the original cardano-db-sync's @deleteBlocksBlockId@ shape
-- without the @reverse_index@ fast path: rolls back by resolving the
-- rollback point to a @block.id@, finding the smallest dependent id
-- past that block in each FK family (tx, tx_out, pool_update), then
-- issuing range deletes against the dependent tables.
module DbSync.Db.Statement.Rollback
  ( -- * Resolving the rollback point
    queryBlockAtPointStmt
  , queryBlockAtOrAfterSlotStmt
  , queryLastCommittedSlotStmt
  , queryTipBlockNoStmt

    -- * Min-id queries (cascade entry points)
  , queryMinTxIdAfterBlockStmt
  , queryMinTxOutIdAfterBlockStmt
  , queryMinPoolUpdateIdAfterTxStmt

    -- * Per-table deletes
  , deleteWhereGteStmt
  , deleteBlockAfterIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import qualified DbSync.Db.Schema.Core as Core
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , PoolUpdateId (..)
  , TxId (..)
  , TxOutId (..)
  , idDecoder
  , idEncoder
  )
import qualified DbSync.Db.Schema.Pool as Pool
import DbSync.Db.Schema.Types (TableDef (..))
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Sql (quoteIdent)

-- ---------------------------------------------------------------------------
-- * Resolving the rollback point
-- ---------------------------------------------------------------------------

-- | Look up the @(block_id, block_no)@ of the block at @(slot, hash)@.
-- 'Nothing' when the rollback point doesn't exist in PG — that's a
-- protocol violation (the node sent a point we never saw); the caller
-- panics.
queryBlockAtPointStmt :: Stmt.Statement (Word64, ByteString) (Maybe (BlockId, Word64))
queryBlockAtPointStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = T.concat
      [ "SELECT id, block_no FROM ", quoteIdent (tdName Core.blockTableDef)
      , " WHERE slot_no = $1 AND hash = $2"
      , " LIMIT 1"
      ]
    encoder =
         (fst >$< E.param (E.nonNullable (fromIntegral >$< E.int8)))
      <> (snd >$< E.param (E.nonNullable E.bytea))
    decoder = D.rowMaybe $ (,)
      <$> idDecoder BlockId
      <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

-- | Current @last_committed_slot@ in the sync-state singleton.
-- 'Nothing' when nothing has been committed yet (fresh DB).
queryLastCommittedSlotStmt :: Stmt.Statement () (Maybe Word64)
queryLastCommittedSlotStmt =
  Stmt.preparable
    "SELECT last_committed_slot FROM dbsync_sync_state WHERE id = 1"
    E.noParams
    (D.singleRow (D.column (D.nullable (fromIntegral <$> D.int8))))

-- | The smallest block at-or-after a given slot. The CLI rollback
-- target is a slot only; the cascade needs a point with the matching
-- hash, so we resolve the slot to a real on-chain block here. Returns
-- @(block_id, slot, block_no, hash)@. The @slot@ may exceed the
-- requested value when empty slots sit between the request and the
-- next block. 'Nothing' when no block lives at or after the requested
-- slot (database empty, or rollback target is past the current tip).
queryBlockAtOrAfterSlotStmt
  :: Stmt.Statement Word64 (Maybe (BlockId, Word64, Word64, ByteString))
queryBlockAtOrAfterSlotStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = T.concat
      [ "SELECT id, slot_no, block_no, hash FROM "
      , quoteIdent (tdName Core.blockTableDef)
      , " WHERE slot_no >= $1"
      , " ORDER BY slot_no ASC"
      , " LIMIT 1"
      ]
    encoder = E.param (E.nonNullable (fromIntegral >$< E.int8))
    decoder = D.rowMaybe $ (,,,)
      <$> idDecoder BlockId
      <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
      <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
      <*> D.column (D.nonNullable D.bytea)

-- | @MAX(block_no) FROM block@. Feeds the k-safety guard inside
-- 'rollbackToPoint'. 'Nothing' on an empty @block@ table.
queryTipBlockNoStmt :: Stmt.Statement () (Maybe Word64)
queryTipBlockNoStmt =
  Stmt.preparable
    ("SELECT MAX(block_no) FROM " <> quoteIdent (tdName Core.blockTableDef))
    E.noParams
    (D.singleRow (fmap fromIntegral <$> D.column (D.nullable D.int8)))

-- ---------------------------------------------------------------------------
-- * Min-id queries
-- ---------------------------------------------------------------------------

-- | @MIN(id) FROM tx WHERE block_id > $1@ — smallest tx in any block
-- strictly past the rollback target. 'Nothing' when no txs to delete.
queryMinTxIdAfterBlockStmt :: Stmt.Statement BlockId (Maybe TxId)
queryMinTxIdAfterBlockStmt =
  Stmt.preparable
    ("SELECT MIN(id) FROM " <> quoteIdent (tdName Core.txTableDef)
        <> " WHERE block_id > $1")
    (idEncoder getBlockId)
    (D.singleRow (D.column (D.nullable (TxId <$> D.int8))))

-- | @MIN(id) FROM tx_out WHERE tx_id >= $1@ — smallest tx_out
-- belonging to a tx that will be deleted. 'Nothing' when no tx_outs
-- to delete.
queryMinTxOutIdAfterBlockStmt :: Stmt.Statement TxId (Maybe TxOutId)
queryMinTxOutIdAfterBlockStmt =
  Stmt.preparable
    ("SELECT MIN(id) FROM " <> quoteIdent (tdName UTxO.txOutTableDef)
        <> " WHERE tx_id >= $1")
    (idEncoder getTxId)
    (D.singleRow (D.column (D.nullable (TxOutId <$> D.int8))))

-- | @MIN(id) FROM pool_update WHERE registered_tx_id >= $1@ — smallest
-- pool_update belonging to a tx that will be deleted. Used to drive
-- the pool_owner / pool_relay cascade. 'Nothing' when no pool updates
-- to delete.
queryMinPoolUpdateIdAfterTxStmt :: Stmt.Statement TxId (Maybe PoolUpdateId)
queryMinPoolUpdateIdAfterTxStmt =
  Stmt.preparable
    ("SELECT MIN(id) FROM " <> quoteIdent (tdName Pool.poolUpdateTableDef)
        <> " WHERE registered_tx_id >= $1")
    (idEncoder getTxId)
    (D.singleRow (D.column (D.nullable (PoolUpdateId <$> D.int8))))

-- ---------------------------------------------------------------------------
-- * Per-table deletes
-- ---------------------------------------------------------------------------

-- | @DELETE FROM \<table\> WHERE \<column\> >= $1@. Returns rows
-- affected. The column is supplied at call time so one helper covers
-- every cascading table.
deleteWhereGteStmt :: Text -> Text -> Stmt.Statement Int64 Int64
deleteWhereGteStmt tableName columnName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE ", quoteIdent columnName, " >= $1"
      ]
    encoder = E.param (E.nonNullable E.int8)

-- | @DELETE FROM block WHERE id > $1@. Strictly greater because the
-- rollback target itself is the new tip; only blocks above it are
-- deleted.
deleteBlockAfterIdStmt :: Stmt.Statement BlockId Int64
deleteBlockAfterIdStmt =
  Stmt.preparable
    ("DELETE FROM " <> quoteIdent (tdName Core.blockTableDef) <> " WHERE id > $1")
    (idEncoder getBlockId)
    D.rowsAffected
