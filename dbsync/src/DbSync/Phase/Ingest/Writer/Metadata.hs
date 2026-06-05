-- | COPY writer for the table owned by the @metadata@ extractor.
module DbSync.Phase.Ingest.Writer.Metadata
  ( writeTxMetadataCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Metadata (TxMetadata, encodeTxMetadataCopy, txMetadataTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

writeTxMetadataCopy :: LoaderStream -> TxMetadata -> IO ()
writeTxMetadataCopy ls md = lsWriteRow ls (tdName txMetadataTableDef) (encodeTxMetadataCopy md)
