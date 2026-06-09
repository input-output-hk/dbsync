{-# LANGUAGE OverloadedStrings #-}

-- | Per-epoch bulk UPDATE of @tx_out.consumed_by_tx_id@ driven by
-- 'DbSync.Worker.TxOut.Worker'.
module DbSync.Db.Statement.Worker.ConsumedBy
  ( bulkUpdateConsumedByTxIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.UTxO (TxOutCols (..), txOutCols, txOutTableDef)
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (arrayParam)

-- | Two parallel arrays: producer-output tx_out ids and the consumer
-- tx ids that spent them. One round-trip regardless of input size.
--
-- Matches by @tx_out.id@ (PK), so it runs without the
-- @(tx_id, index)@ index that doesn't exist during Ingest. The
-- @consumed_by_tx_id IS NULL@ guard makes replays idempotent and
-- leaves cross-epoch races alone.
bulkUpdateConsumedByTxIdStmt :: Stmt.Statement ([Int64], [Int64]) ()
bulkUpdateConsumedByTxIdStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< arrayParam E.int8)   -- producer tx_out ids
           <> (snd >$< arrayParam E.int8)   -- consumer tx ids
    sql = mconcat
      [ "UPDATE ", table txOutTableDef
      , " SET ", col txOutCols.tocConsumedByTxId, " = u.consumer"
      , " FROM unnest($1, $2) AS u(out_id, consumer)"
      , " WHERE ", qcol (table txOutTableDef) txOutCols.tocId, " = u.out_id"
      , " AND ", qcol (table txOutTableDef) txOutCols.tocConsumedByTxId, " IS NULL"
      ]
