-- | hasql writers for tables owned by the @utxo@ extractor.
module DbSync.Phase.Following.Writer.UTxO
  ( writeAddressConn
  , writeAddressBuf
  , writeTxOutConn
  , writeTxOutBuf
  , writeTxInConn
  , writeTxInBuf
  , writeCollateralTxInConn
  , writeCollateralTxInBuf
  , writeCollateralTxOutConn
  , writeCollateralTxOutBuf
  , writeReferenceTxInConn
  , writeReferenceTxInBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Ids
  ( AddressId
  , CollateralTxInId
  , CollateralTxOutId
  , ReferenceTxInId
  , TxInId
  , TxOutId
  )
import DbSync.Db.Schema.UTxO
  ( CollateralTxIn
  , CollateralTxOut
  , ReferenceTxIn
  , TxIn
  , TxOut
  )
import DbSync.Db.Statement.Address (insertAddressRowStmt)
import DbSync.Db.Statement.CollateralTxIn (insertCollateralTxInRowStmt)
import DbSync.Db.Statement.CollateralTxOut (insertCollateralTxOutRowStmt)
import DbSync.Db.Statement.ReferenceTxIn (insertReferenceTxInRowStmt)
import DbSync.Db.Statement.TxIn (insertTxInRowStmt)
import DbSync.Db.Statement.TxOut (insertTxOutRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeAddressConn :: Conn.Connection -> AddressId -> Address -> IO ()
writeAddressConn conn aid addr = runConn conn (aid, addr) insertAddressRowStmt

writeAddressBuf :: WriteBuffer -> AddressId -> Address -> IO ()
writeAddressBuf buf aid addr = queueBuf buf (aid, addr) insertAddressRowStmt

writeTxOutConn :: Conn.Connection -> TxOutId -> TxOut -> IO ()
writeTxOutConn conn oid txo = runConn conn (oid, txo) insertTxOutRowStmt

writeTxOutBuf :: WriteBuffer -> TxOutId -> TxOut -> IO ()
writeTxOutBuf buf oid txo = queueBuf buf (oid, txo) insertTxOutRowStmt

writeTxInConn :: Conn.Connection -> TxInId -> TxIn -> IO ()
writeTxInConn conn iid ti = runConn conn (iid, ti) insertTxInRowStmt

writeTxInBuf :: WriteBuffer -> TxInId -> TxIn -> IO ()
writeTxInBuf buf iid ti = queueBuf buf (iid, ti) insertTxInRowStmt

writeCollateralTxInConn :: Conn.Connection -> CollateralTxInId -> CollateralTxIn -> IO ()
writeCollateralTxInConn conn iid ci = runConn conn (iid, ci) insertCollateralTxInRowStmt

writeCollateralTxInBuf :: WriteBuffer -> CollateralTxInId -> CollateralTxIn -> IO ()
writeCollateralTxInBuf buf iid ci = queueBuf buf (iid, ci) insertCollateralTxInRowStmt

writeCollateralTxOutConn :: Conn.Connection -> CollateralTxOutId -> CollateralTxOut -> IO ()
writeCollateralTxOutConn conn oid co = runConn conn (oid, co) insertCollateralTxOutRowStmt

writeCollateralTxOutBuf :: WriteBuffer -> CollateralTxOutId -> CollateralTxOut -> IO ()
writeCollateralTxOutBuf buf oid co = queueBuf buf (oid, co) insertCollateralTxOutRowStmt

writeReferenceTxInConn :: Conn.Connection -> ReferenceTxInId -> ReferenceTxIn -> IO ()
writeReferenceTxInConn conn iid ri = runConn conn (iid, ri) insertReferenceTxInRowStmt

writeReferenceTxInBuf :: WriteBuffer -> ReferenceTxInId -> ReferenceTxIn -> IO ()
writeReferenceTxInBuf buf iid ri = queueBuf buf (iid, ri) insertReferenceTxInRowStmt
