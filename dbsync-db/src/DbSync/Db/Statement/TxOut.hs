{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_out@ table.
--
-- Used during 'FollowingChainTip' (per-output INSERT and id allocation).
-- The 'IngestChainHistory' phase writes via COPY instead.
module DbSync.Db.Statement.TxOut
  ( -- * Inserts
    insertTxOutRowStmt

    -- * ID allocation
  , nextTxOutIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (TxOutId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (TxOut, txOutEncoder, txOutTableDef)

table :: Text
table = tdName txOutTableDef

-- | Insert a 'TxOut' with a caller-chosen id. Used by
-- 'FollowingChainTip' after the resolver allocates the id via
-- 'nextTxOutIdStmt'.
insertTxOutRowStmt :: Stmt.Statement (TxOutId, TxOut) ()
insertTxOutRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxOutId)
           <> (snd >$< txOutEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, tx_id, index, address, address_has_script, payment_cred"
      , " , stake_address_id, value, data_hash, inline_datum_id"
      , " , reference_script_id, consumed_by_tx_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)"
      ]

-- | Allocate a new id from the @tx_out_id_seq@ sequence.
nextTxOutIdStmt :: Stmt.Statement () TxOutId
nextTxOutIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder TxOutId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
