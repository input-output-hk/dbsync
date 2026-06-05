-- | COPY writers for tables owned by the @utxo@ extractor.
module DbSync.Phase.Ingest.Writer.UTxO
  ( writeAddressCopy
  , writeTxOutCopy
  , writeTxInCopy
  , writeCollateralTxInCopy
  , writeCollateralTxOutCopy
  , writeReferenceTxInCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Address (Address, addressTableDef, encodeAddressCopy)
import DbSync.Db.Schema.Ids (AddressId, CollateralTxOutId, TxOutId)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxIn
  , CollateralTxOut
  , ReferenceTxIn
  , TxIn
  , TxOut
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , encodeCollateralTxInCopy
  , encodeCollateralTxOutCopy
  , encodeReferenceTxInCopy
  , encodeTxInCopy
  , encodeTxOutCopy
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )

writeAddressCopy :: LoaderStream -> AddressId -> Address -> IO ()
writeAddressCopy ls aid addr = lsWriteRow ls (tdName addressTableDef) (encodeAddressCopy aid addr)

writeTxOutCopy :: LoaderStream -> TxOutId -> TxOut -> IO ()
writeTxOutCopy ls oid txo = lsWriteRow ls (tdName txOutTableDef) (encodeTxOutCopy oid txo)

writeTxInCopy :: LoaderStream -> TxIn -> IO ()
writeTxInCopy ls ti = lsWriteRow ls (tdName txInTableDef) (encodeTxInCopy ti)

writeCollateralTxInCopy :: LoaderStream -> CollateralTxIn -> IO ()
writeCollateralTxInCopy ls ci = lsWriteRow ls (tdName collateralTxInTableDef) (encodeCollateralTxInCopy ci)

writeCollateralTxOutCopy :: LoaderStream -> CollateralTxOutId -> CollateralTxOut -> IO ()
writeCollateralTxOutCopy ls cid co = lsWriteRow ls (tdName collateralTxOutTableDef) (encodeCollateralTxOutCopy cid co)

writeReferenceTxInCopy :: LoaderStream -> ReferenceTxIn -> IO ()
writeReferenceTxInCopy ls ri = lsWriteRow ls (tdName referenceTxInTableDef) (encodeReferenceTxInCopy ri)
