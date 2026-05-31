-- | hasql writer for the table owned by the @metadata@ extractor.
module DbSync.Phase.Following.Writer.Metadata
  ( writeTxMetadataConn
  , writeTxMetadataBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (TxMetadataId)
import DbSync.Db.Schema.Metadata (TxMetadata)
import DbSync.Db.Statement.TxMetadata (insertTxMetadataRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeTxMetadataConn :: Conn.Connection -> TxMetadataId -> TxMetadata -> IO ()
writeTxMetadataConn conn mid md = runConn conn (mid, md) insertTxMetadataRowStmt

writeTxMetadataBuf :: WriteBuffer -> TxMetadataId -> TxMetadata -> IO ()
writeTxMetadataBuf buf mid md = queueBuf buf (mid, md) insertTxMetadataRowStmt
