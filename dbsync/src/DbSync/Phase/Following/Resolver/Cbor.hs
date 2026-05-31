-- | Follow 'IdResolver' fragment for the @cbor@ extractor.
module DbSync.Phase.Following.Resolver.Cbor
  ( assignTxCborIdConn
  , assignTxCborIdBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (TxCborId)
import DbSync.Db.Statement.TxCbor (nextTxCborIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal (runStmt)

assignTxCborIdConn :: Conn.Connection -> IO TxCborId
assignTxCborIdConn conn = runStmt conn () nextTxCborIdStmt

assignTxCborIdBuf :: PreAllocatedIds -> IO TxCborId
assignTxCborIdBuf preAlloc = popHead "assignTxCborId" (paiTxCborIds preAlloc)
