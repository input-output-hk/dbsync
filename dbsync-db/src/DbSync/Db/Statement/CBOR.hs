-- | Hasql 'Statement' bindings for the @cbor@ extractor table:
-- @tx_cbor@.
module DbSync.Db.Statement.CBOR
  ( insertTxCborRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.CBOR (TxCbor, txCborEncoder, txCborTableDef)
import DbSync.Db.Statement.Common (insertRowSql)

-- ---------------------------------------------------------------------------
-- * tx_cbor
-- ---------------------------------------------------------------------------

insertTxCborRowStmt :: Stmt.Statement TxCbor ()
insertTxCborRowStmt =
  Stmt.preparable (insertRowSql txCborTableDef) txCborEncoder D.noResult
