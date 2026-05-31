-- | COPY writer for the table owned by the @cbor@ extractor.
module DbSync.Phase.Ingest.Writer.Cbor
  ( writeTxCborCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.CBOR (TxCbor, encodeTxCborCopy, txCborTableDef)
import DbSync.Db.Schema.Ids (TxCborId)
import DbSync.Db.Schema.Types (TableDef (..))

writeTxCborCopy :: LoaderStream -> TxCborId -> TxCbor -> IO ()
writeTxCborCopy ls tcid tc = lsWriteRow ls (tdName txCborTableDef) (encodeTxCborCopy tcid tc)
