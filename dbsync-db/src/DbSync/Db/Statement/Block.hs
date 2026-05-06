{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @block@ table.
--
-- Used during 'FollowingChainTip' (per-block INSERT \/ resolver
-- lookups) and at boot for resume + intersection. The
-- 'IngestChainHistory' phase writes via COPY instead.
module DbSync.Db.Statement.Block
  ( -- * Inserts
    insertBlockStmt
  , insertBlockRowStmt

    -- * ID allocation
  , nextBlockIdStmt

    -- * Lookups
  , queryBlockIdByHashStmt
  , queryBlockCountStmt
  , queryLatestBlockNoStmt
  , queryLatestSlotNoStmt
  , queryLatestBlockIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core (Block, blockEncoder, blockTableDef)
import DbSync.Db.Schema.Ids (BlockId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName blockTableDef

-- | Insert a 'Block', let the DB pick an id, return it.
insertBlockStmt :: Stmt.Statement Block BlockId
insertBlockStmt =
  Stmt.preparable sql blockEncoder (D.singleRow $ idDecoder BlockId)
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( hash, epoch_no, slot_no, epoch_slot_no, block_no"
      , " , previous_id, slot_leader_id, size, time, tx_count"
      , " , proto_major, proto_minor, vrf_key, op_cert, op_cert_counter)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)"
      , " RETURNING id"
      ]

-- | Insert a 'Block' with a caller-chosen id. Used by
-- 'FollowingChainTip' after the resolver allocates the id via
-- 'nextBlockIdStmt'.
insertBlockRowStmt :: Stmt.Statement (BlockId, Block) ()
insertBlockRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getBlockId)
           <> (snd >$< blockEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, hash, epoch_no, slot_no, epoch_slot_no, block_no"
      , " , previous_id, slot_leader_id, size, time, tx_count"
      , " , proto_major, proto_minor, vrf_key, op_cert, op_cert_counter)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)"
      ]

-- | Allocate a new id from the @block_id_seq@ sequence.
nextBlockIdStmt :: Stmt.Statement () BlockId
nextBlockIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder BlockId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up a block id by its hash.
queryBlockIdByHashStmt :: Stmt.Statement ByteString (Maybe BlockId)
queryBlockIdByHashStmt =
  Stmt.preparable sql
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder BlockId))
  where
    sql = "SELECT id FROM " <> table <> " WHERE hash = $1"

-- | Count rows in @block@.
queryBlockCountStmt :: Stmt.Statement () Int64
queryBlockCountStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow (D.column (D.nonNullable D.int8)))
  where
    sql = "SELECT COUNT(*) FROM " <> table

-- | The largest @block_no@ stored. 'Nothing' if the table is empty
-- or every row has a NULL @block_no@ (Byron EBBs).
queryLatestBlockNoStmt :: Stmt.Statement () (Maybe Word64)
queryLatestBlockNoStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow $ D.column (D.nullable $ fromIntegral <$> D.int8))
  where
    sql = "SELECT MAX(block_no) FROM " <> table <> " WHERE block_no IS NOT NULL"

-- | The largest @slot_no@ stored, or @0@ if no slot-bearing block
-- exists yet. Used at boot to find the ChainSync intersection point.
queryLatestSlotNoStmt :: Stmt.Statement () Word64
queryLatestSlotNoStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow $ fromIntegral <$> D.column (D.nonNullable D.int8))
  where
    sql = T.concat
      [ "SELECT COALESCE(MAX(slot_no), 0)::bigint FROM ", table
      , " WHERE slot_no IS NOT NULL"
      ]

-- | The id of the block with the largest @slot_no@. 'Nothing' on an
-- empty table.
queryLatestBlockIdStmt :: Stmt.Statement () (Maybe BlockId)
queryLatestBlockIdStmt =
  Stmt.preparable sql E.noParams (D.rowMaybe (idDecoder BlockId))
  where
    sql = T.concat
      [ "SELECT id FROM ", table
      , " WHERE slot_no IS NOT NULL ORDER BY slot_no DESC LIMIT 1"
      ]
