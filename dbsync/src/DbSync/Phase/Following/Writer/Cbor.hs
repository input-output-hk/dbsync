-- | hasql writer for the table owned by the @cbor@ extractor.
module DbSync.Phase.Following.Writer.Cbor
  ( writeTxCborConn
  , writeTxCborBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.CBOR (TxCbor)
import DbSync.Db.Statement.TxCbor (insertTxCborRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeTxCborConn :: Conn.Connection -> TxCbor -> IO ()
writeTxCborConn conn tc = runConn conn tc insertTxCborRowStmt

writeTxCborBuf :: WriteBuffer -> TxCbor -> IO ()
writeTxCborBuf buf tc = queueBuf buf tc insertTxCborRowStmt
