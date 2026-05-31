-- | Follow 'IdResolver' fragment for the @metadata@ extractor.
module DbSync.Phase.Following.Resolver.Metadata
  ( assignTxMetadataIdConn
  , assignTxMetadataIdBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (TxMetadataId)
import DbSync.Db.Statement.TxMetadata (nextTxMetadataIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal (runStmt)

assignTxMetadataIdConn :: Conn.Connection -> IO TxMetadataId
assignTxMetadataIdConn conn = runStmt conn () nextTxMetadataIdStmt

assignTxMetadataIdBuf :: PreAllocatedIds -> IO TxMetadataId
assignTxMetadataIdBuf preAlloc = popHead "assignTxMetadataId" (paiTxMetadataIds preAlloc)
