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
import DbSync.Db.Schema.Ids
  ( AddressId
  , CollateralTxInId
  , CollateralTxOutId
  , ReferenceTxInId
  , TxInId
  , TxOutId
  )
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

writeTxInCopy :: LoaderStream -> TxInId -> TxIn -> IO ()
writeTxInCopy ls iid ti = lsWriteRow ls (tdName txInTableDef) (encodeTxInCopy iid ti)

writeCollateralTxInCopy :: LoaderStream -> CollateralTxInId -> CollateralTxIn -> IO ()
writeCollateralTxInCopy ls iid ci = lsWriteRow ls (tdName collateralTxInTableDef) (encodeCollateralTxInCopy iid ci)

writeCollateralTxOutCopy :: LoaderStream -> CollateralTxOutId -> CollateralTxOut -> IO ()
writeCollateralTxOutCopy ls oid co = lsWriteRow ls (tdName collateralTxOutTableDef) (encodeCollateralTxOutCopy oid co)

writeReferenceTxInCopy :: LoaderStream -> ReferenceTxInId -> ReferenceTxIn -> IO ()
writeReferenceTxInCopy ls iid ri = lsWriteRow ls (tdName referenceTxInTableDef) (encodeReferenceTxInCopy iid ri)
