{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @slot_leader@ table.
--
-- Used during 'FollowingChainTip' and 'PreparingForChainTip'; the
-- 'IngestChainHistory' phase writes via COPY instead.
module DbSync.Db.Statement.SlotLeader
  ( -- * Inserts
    insertSlotLeaderStmt
  , insertSlotLeaderRowStmt

    -- * ID allocation
  , nextSlotLeaderIdStmt

    -- * Lookups
  , querySlotLeaderIdStmt
  , querySlotLeaderCountStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core (SlotLeader, slotLeaderEncoder, slotLeaderTableDef)
import DbSync.Db.Schema.Ids (SlotLeaderId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName slotLeaderTableDef

-- | Insert a 'SlotLeader', let the DB pick an id, return it.
insertSlotLeaderStmt :: Stmt.Statement SlotLeader SlotLeaderId
insertSlotLeaderStmt =
  Stmt.preparable sql slotLeaderEncoder (D.singleRow $ idDecoder SlotLeaderId)
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (hash, pool_hash_id, description) VALUES ($1, $2, $3) RETURNING id"
      ]

-- | Insert a 'SlotLeader' with a caller-chosen id. Used by
-- 'FollowingChainTip' after the resolver allocates the id via
-- 'nextSlotLeaderIdStmt'.
insertSlotLeaderRowStmt :: Stmt.Statement (SlotLeaderId, SlotLeader) ()
insertSlotLeaderRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getSlotLeaderId)
           <> (snd >$< slotLeaderEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, hash, pool_hash_id, description) VALUES ($1, $2, $3, $4)"
      ]

-- | Allocate a new id from the @slot_leader_id_seq@ sequence.
nextSlotLeaderIdStmt :: Stmt.Statement () SlotLeaderId
nextSlotLeaderIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder SlotLeaderId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up a slot_leader id by its 28-byte hash.
querySlotLeaderIdStmt :: Stmt.Statement ByteString (Maybe SlotLeaderId)
querySlotLeaderIdStmt =
  Stmt.preparable sql
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder SlotLeaderId))
  where
    sql = "SELECT id FROM " <> table <> " WHERE hash = $1"

-- | Count rows in the @slot_leader@ table.
querySlotLeaderCountStmt :: Stmt.Statement () Int64
querySlotLeaderCountStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow (D.column (D.nonNullable D.int8)))
  where
    sql = "SELECT COUNT(*) FROM " <> table
