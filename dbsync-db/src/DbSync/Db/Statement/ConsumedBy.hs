{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' for the per-epoch @tx_out.consumed_by_tx_id@
-- bulk UPDATE driven by 'DbSync.Worker.TxOut'.
module DbSync.Db.Statement.ConsumedBy
  ( bulkUpdateConsumedByTxIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (txOutTableDef)
import DbSync.Db.Statement.Common (arrayParam)

table :: Text
table = tdName txOutTableDef

-- | Two parallel arrays: producer-output tx_out ids and the consumer
-- tx ids that spent them. One round-trip regardless of input size.
--
-- Matches by @tx_out.id@ (PK) so it runs without the
-- @(tx_id, index)@ index that doesn't exist during Ingest. Lives on
-- the same connection as the AddressResolver UPDATEs, so no
-- cross-transaction lock contention on overlapping rows.
--
-- @consumed_by_tx_id IS NULL@ guards against re-applying the same
-- write if a job is replayed; matched rows whose value already
-- differs (cross-epoch races) are left alone.
bulkUpdateConsumedByTxIdStmt :: Stmt.Statement ([Int64], [Int64]) ()
bulkUpdateConsumedByTxIdStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< arrayParam E.int8)   -- producer tx_out ids
           <> (snd >$< arrayParam E.int8)   -- consumer tx ids
    sql = T.unwords
      [ "UPDATE", table
      , "SET consumed_by_tx_id = u.consumer"
      , "FROM unnest($1, $2) AS u(out_id, consumer)"
      , "WHERE", table <> ".id = u.out_id"
      , "  AND", table <> ".consumed_by_tx_id IS NULL"
      ]
