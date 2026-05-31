-- | COPY writer for the table owned by the @metadata@ extractor.
module DbSync.Phase.Ingest.Writer.Metadata
  ( writeTxMetadataCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Ids (TxMetadataId)
import DbSync.Db.Schema.Metadata (TxMetadata, encodeTxMetadataCopy, txMetadataTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

writeTxMetadataCopy :: LoaderStream -> TxMetadataId -> TxMetadata -> IO ()
writeTxMetadataCopy ls mid md = lsWriteRow ls (tdName txMetadataTableDef) (encodeTxMetadataCopy mid md)
