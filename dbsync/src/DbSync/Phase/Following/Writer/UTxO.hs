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
  , CollateralTxOutId
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

writeTxInConn :: Conn.Connection -> TxIn -> IO ()
writeTxInConn conn ti = runConn conn ti insertTxInRowStmt

writeTxInBuf :: WriteBuffer -> TxIn -> IO ()
writeTxInBuf buf ti = queueBuf buf ti insertTxInRowStmt

writeCollateralTxInConn :: Conn.Connection -> CollateralTxIn -> IO ()
writeCollateralTxInConn conn ci = runConn conn ci insertCollateralTxInRowStmt

writeCollateralTxInBuf :: WriteBuffer -> CollateralTxIn -> IO ()
writeCollateralTxInBuf buf ci = queueBuf buf ci insertCollateralTxInRowStmt

writeCollateralTxOutConn :: Conn.Connection -> CollateralTxOutId -> CollateralTxOut -> IO ()
writeCollateralTxOutConn conn oid co = runConn conn (oid, co) insertCollateralTxOutRowStmt

writeCollateralTxOutBuf :: WriteBuffer -> CollateralTxOutId -> CollateralTxOut -> IO ()
writeCollateralTxOutBuf buf oid co = queueBuf buf (oid, co) insertCollateralTxOutRowStmt

writeReferenceTxInConn :: Conn.Connection -> ReferenceTxIn -> IO ()
writeReferenceTxInConn conn ri = runConn conn ri insertReferenceTxInRowStmt

writeReferenceTxInBuf :: WriteBuffer -> ReferenceTxIn -> IO ()
writeReferenceTxInBuf buf ri = queueBuf buf ri insertReferenceTxInRowStmt
