-- | hasql writer for the table owned by the @metadata@ extractor.
module DbSync.Phase.Following.Writer.Metadata
  ( writeTxMetadataConn
  , writeTxMetadataBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Metadata (TxMetadata)
import DbSync.Db.Statement.Metadata (insertTxMetadataRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeTxMetadataConn :: Conn.Connection -> TxMetadata -> IO ()
writeTxMetadataConn conn md = runConn conn md insertTxMetadataRowStmt

writeTxMetadataBuf :: WriteBuffer -> TxMetadata -> IO ()
writeTxMetadataBuf buf md = queueBuf buf md insertTxMetadataRowStmt
