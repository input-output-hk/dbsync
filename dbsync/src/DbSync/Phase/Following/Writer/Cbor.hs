-- | hasql writer for the table owned by the @cbor@ extractor.
module DbSync.Phase.Following.Writer.Cbor
  ( writeTxCborConn
  , writeTxCborBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.CBOR (TxCbor)
import DbSync.Db.Schema.Ids (TxCborId)
import DbSync.Db.Statement.TxCbor (insertTxCborRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeTxCborConn :: Conn.Connection -> TxCborId -> TxCbor -> IO ()
writeTxCborConn conn tcid tc = runConn conn (tcid, tc) insertTxCborRowStmt

writeTxCborBuf :: WriteBuffer -> TxCborId -> TxCbor -> IO ()
writeTxCborBuf buf tcid tc = queueBuf buf (tcid, tc) insertTxCborRowStmt
