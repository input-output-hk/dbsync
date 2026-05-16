{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_in@ table.
--
-- Used during 'FollowingChainTip' (per-input INSERT and id allocation).
-- The 'IngestChainHistory' phase writes via COPY instead.
--
-- @tx_out_id@ is left NULL during ingest; it is resolved post-load by
-- a SQL join in 'PreparingForVolatileTail'. The same convention applies
-- here in FollowingChainTip — the writer never sets it.
module DbSync.Db.Statement.TxIn
  ( -- * Inserts
    insertTxInRowStmt

    -- * ID allocation
  , nextTxInIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (TxInId (..), idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (TxIn, txInEncoder, txInTableDef)
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName txInTableDef

-- | Insert a 'TxIn' with a caller-chosen id.
insertTxInRowStmt :: Stmt.Statement (TxInId, TxIn) ()
insertTxInRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxInId)
           <> (snd >$< txInEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, tx_in_id, tx_out_id, tx_out_index, tx_out_hash, redeemer_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6)"
      ]

-- | Allocate a new id from the @tx_in_id_seq@ sequence.
nextTxInIdStmt :: Stmt.Statement () TxInId
nextTxInIdStmt = nextIdStmt txInTableDef TxInId
