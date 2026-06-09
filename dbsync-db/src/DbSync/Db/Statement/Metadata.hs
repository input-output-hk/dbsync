-- | Hasql 'Statement' bindings for the @metadata@ extractor table:
-- @tx_metadata@.
module DbSync.Db.Statement.Metadata
  ( insertTxMetadataRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Metadata (TxMetadata, txMetadataEncoder, txMetadataTableDef)
import DbSync.Db.Statement.Common (insertRowSql)

-- ---------------------------------------------------------------------------
-- * tx_metadata
-- ---------------------------------------------------------------------------

insertTxMetadataRowStmt :: Stmt.Statement TxMetadata ()
insertTxMetadataRowStmt =
  Stmt.preparable (insertRowSql txMetadataTableDef) txMetadataEncoder D.noResult
