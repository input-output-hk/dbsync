{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the 'FollowingChainTip' rollback
-- cascade.
--
-- Mirrors cardano-db-sync's @deleteBlocksBlockId@ shape without the
-- @reverse_index@ fast path: resolve the rollback point to a
-- @block.id@, find the smallest dependent id past that block in each
-- FK family (tx, tx_out, pool_update), then issue range deletes
-- against the dependent tables.
module DbSync.Db.Statement.Worker.Rollback
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
import DbSync.Db.Schema.Core (BlockCols (..), TxCols (..))
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , PoolUpdateId (..)
  , TxId (..)
  , TxOutId (..)
  , idDecoder
  , idEncoder
  )
import qualified DbSync.Db.Schema.Pool as Pool
import DbSync.Db.Schema.Pool (PoolUpdateCols (..))
import DbSync.Db.Schema.SyncState (SyncStateCols (..), syncStateCols, syncStateTableDef)
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Schema.UTxO (TxOutCols (..))
import DbSync.Db.Sql (quoteIdent)
import DbSync.Db.Sql.Refs (col, table)

-- ---------------------------------------------------------------------------
-- * Resolving the rollback point
-- ---------------------------------------------------------------------------

-- | @(block_id, block_no, epoch_no)@ for the block at
-- @(slot, hash)@. @epoch_no@ is 'Nothing' for Byron EBBs that
-- predate the field. Outer 'Nothing' means the rollback point
-- doesn't exist in PG — a protocol violation the caller turns into
-- a panic.
queryBlockAtPointStmt
  :: Stmt.Statement (Word64, ByteString) (Maybe (BlockId, Word64, Maybe Word64))
queryBlockAtPointStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "SELECT ", Core.blockCols.bcId.tcName
      , ", ", Core.blockCols.bcBlockNo.tcName
      , ", ", Core.blockCols.bcEpochNo.tcName
      , " FROM ", quoteIdent (tdName Core.blockTableDef)
      , " WHERE ", Core.blockCols.bcSlotNo.tcName, " = $1"
      , " AND ", Core.blockCols.bcHash.tcName, " = $2"
      , " LIMIT 1"
      ]
    encoder =
         (fst >$< E.param (E.nonNullable (fromIntegral >$< E.int8)))
      <> (snd >$< E.param (E.nonNullable E.bytea))
    decoder = D.rowMaybe $ (,,)
      <$> idDecoder BlockId
      <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
      <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))

-- | 'Nothing' on a fresh DB.
queryLastCommittedSlotStmt :: Stmt.Statement () (Maybe Word64)
queryLastCommittedSlotStmt =
  Stmt.preparable
    ( "SELECT " <> col syncStateCols.sscLastCommittedSlot
        <> " FROM " <> table syncStateTableDef
        <> " WHERE " <> col syncStateCols.sscId <> " = 1"
    )
    E.noParams
    (D.singleRow (D.column (D.nullable (fromIntegral <$> D.int8))))

-- | Resolve a CLI rollback slot to a real on-chain block. The
-- returned @slot@ may exceed the request when empty slots sit
-- between request and next block. 'Nothing' when no block at-or-after
-- the slot exists (empty DB, or target past the tip). Returns
-- @(block_id, slot, block_no, hash)@.
queryBlockAtOrAfterSlotStmt
  :: Stmt.Statement Word64 (Maybe (BlockId, Word64, Word64, ByteString))
queryBlockAtOrAfterSlotStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "SELECT ", Core.blockCols.bcId.tcName
      , ", ", Core.blockCols.bcSlotNo.tcName
      , ", ", Core.blockCols.bcBlockNo.tcName
      , ", ", Core.blockCols.bcHash.tcName
      , " FROM ", quoteIdent (tdName Core.blockTableDef)
      , " WHERE ", Core.blockCols.bcSlotNo.tcName, " >= $1"
      , " ORDER BY ", Core.blockCols.bcSlotNo.tcName, " ASC"
      , " LIMIT 1"
      ]
    encoder = E.param (E.nonNullable (fromIntegral >$< E.int8))
    decoder = D.rowMaybe $ (,,,)
      <$> idDecoder BlockId
      <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
      <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
      <*> D.column (D.nonNullable D.bytea)

-- | Feeds the k-safety guard in 'rollbackToPoint'. 'Nothing' on an
-- empty @block@ table.
queryTipBlockNoStmt :: Stmt.Statement () (Maybe Word64)
queryTipBlockNoStmt =
  Stmt.preparable
    ( "SELECT MAX(" <> Core.blockCols.bcBlockNo.tcName <> ") FROM "
      <> quoteIdent (tdName Core.blockTableDef)
    )
    E.noParams
    (D.singleRow (fmap fromIntegral <$> D.column (D.nullable D.int8)))

-- ---------------------------------------------------------------------------
-- * Min-id queries
-- ---------------------------------------------------------------------------

-- | Smallest tx in any block strictly past the rollback target.
queryMinTxIdAfterBlockStmt :: Stmt.Statement BlockId (Maybe TxId)
queryMinTxIdAfterBlockStmt =
  Stmt.preparable
    ( "SELECT MIN(" <> Core.txCols.tcId.tcName <> ") FROM "
      <> quoteIdent (tdName Core.txTableDef)
      <> " WHERE " <> Core.txCols.tcBlockId.tcName <> " > $1"
    )
    (idEncoder getBlockId)
    (D.singleRow (D.column (D.nullable (TxId <$> D.int8))))

-- | Smallest tx_out belonging to a tx that will be deleted.
queryMinTxOutIdAfterBlockStmt :: Stmt.Statement TxId (Maybe TxOutId)
queryMinTxOutIdAfterBlockStmt =
  Stmt.preparable
    ( "SELECT MIN(" <> UTxO.txOutCols.tocId.tcName <> ") FROM "
      <> quoteIdent (tdName UTxO.txOutTableDef)
      <> " WHERE " <> UTxO.txOutCols.tocTxId.tcName <> " >= $1"
    )
    (idEncoder getTxId)
    (D.singleRow (D.column (D.nullable (TxOutId <$> D.int8))))

-- | Smallest pool_update belonging to a tx that will be deleted.
-- Drives the pool_owner / pool_relay cascade.
queryMinPoolUpdateIdAfterTxStmt :: Stmt.Statement TxId (Maybe PoolUpdateId)
queryMinPoolUpdateIdAfterTxStmt =
  Stmt.preparable
    ( "SELECT MIN(" <> Pool.poolUpdateCols.pucId.tcName <> ") FROM "
        <> quoteIdent (tdName Pool.poolUpdateTableDef)
        <> " WHERE " <> Pool.poolUpdateCols.pucRegisteredTxId.tcName <> " >= $1"
    )
    (idEncoder getTxId)
    (D.singleRow (D.column (D.nullable (PoolUpdateId <$> D.int8))))

-- ---------------------------------------------------------------------------
-- * Per-table deletes
-- ---------------------------------------------------------------------------

-- | @DELETE FROM <table> WHERE <column> >= $1@. The column is
-- supplied at call time so one helper covers every cascading table.
deleteWhereGteStmt :: Text -> Text -> Stmt.Statement Int64 Int64
deleteWhereGteStmt tableName columnName =
  Stmt.unpreparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "DELETE FROM ", quoteIdent tableName
      , " WHERE ", quoteIdent columnName, " >= $1"
      ]
    encoder = E.param (E.nonNullable E.int8)

-- | Strictly @>@ because the rollback target itself is the new tip;
-- only blocks above it are deleted.
deleteBlockAfterIdStmt :: Stmt.Statement BlockId Int64
deleteBlockAfterIdStmt =
  Stmt.preparable
    ("DELETE FROM " <> quoteIdent (tdName Core.blockTableDef) <> " WHERE id > $1")
    (idEncoder getBlockId)
    D.rowsAffected
