{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @utxo@ extractor tables:
-- @address@, @tx_out@, @tx_in@, @collateral_tx_in@,
-- @collateral_tx_out@, @reference_tx_in@.
--
-- @address@ is dedup-keyed on @raw@ (probed via the @raw_hash@ index,
-- a btree on the fixed-width md5). @tx_out@ and @collateral_tx_out@
-- are counter-managed. The three input tables (@tx_in@,
-- @collateral_tx_in@, @reference_tx_in@) are IDENTITY leaves.
--
-- The @bulk…@ statements fold an epoch's worth of work into a single
-- round-trip; they are used by the IngestChainHistory AddressResolver
-- worker.
module DbSync.Db.Statement.UTxO
  ( -- * address
    insertAddressRowStmt
  , nextAddressIdStmt
  , queryAddressIdStmt
  , BulkAddressInsert (..)
  , bulkSelectAddressIdsStmt
  , bulkInsertAddressesStmt

    -- * tx_out
  , insertTxOutRowStmt
  , updateTxOutAddressIdStmt
  , bulkUpdateTxOutAddressIdsStmt
  , nextTxOutIdStmt
  , queryOutputValueStmt
  , queryInputUtxoStmt

    -- * tx_in
  , insertTxInRowStmt

    -- * collateral_tx_in
  , insertCollateralTxInRowStmt

    -- * collateral_tx_out
  , insertCollateralTxOutRowStmt
  , nextCollateralTxOutIdStmt
  , updateCollateralTxOutAddressIdStmt
  , bulkUpdateCollateralTxOutAddressIdsStmt

    -- * reference_tx_in
  , insertReferenceTxInRowStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address
  ( Address
  , AddressCols (..)
  , addressCols
  , addressEncoder
  , addressTableDef
  )
import DbSync.Db.Schema.Core (TxCols (..), txCols, txTableDef)
import DbSync.Db.Schema.Ids
  ( AddressId (..)
  , CollateralTxOutId (..)
  , TxId (..)
  , TxOutId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Schema.Types (TableColumn (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxIn
  , CollateralTxOut
  , CollateralTxOutCols (..)
  , ReferenceTxIn
  , TxIn
  , TxOut
  , TxOutCols (..)
  , collateralTxInEncoder
  , collateralTxInTableDef
  , collateralTxOutCols
  , collateralTxOutEncoder
  , collateralTxOutTableDef
  , referenceTxInEncoder
  , referenceTxInTableDef
  , txInEncoder
  , txInTableDef
  , txOutCols
  , txOutEncoder
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common
  ( arrayParam
  , insertRowSql
  , nextIdStmt
  , nullArrayParam
  )
import DbSync.Db.Types (DbLovelace, dbLovelaceValueDecoder)

-- ---------------------------------------------------------------------------
-- * address
-- ---------------------------------------------------------------------------

insertAddressRowStmt :: Stmt.Statement (AddressId, Address) ()
insertAddressRowStmt =
  Stmt.preparable (insertRowSql addressTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< addressEncoder)

nextAddressIdStmt :: Stmt.Statement () AddressId
nextAddressIdStmt = nextIdStmt addressTableDef AddressId

-- | Probes the @raw_hash@ unique index (fixed-width md5 of @raw@) and
-- verifies the full @raw@ match to guard against the theoretical
-- 128-bit collision.
queryAddressIdStmt :: Stmt.Statement ByteString (Maybe AddressId)
queryAddressIdStmt =
  Stmt.preparable sql encoder decoder
  where
    encoder = E.param (E.nonNullable E.bytea)
    decoder = D.rowMaybe (idDecoder AddressId)
    sql = mconcat
      [ "SELECT ", addressCols.acId.tcName, " FROM ", table addressTableDef
      , " WHERE ", addressCols.acRawHash.tcName, " = decode(md5($1), 'hex')"
      , " AND ", addressCols.acRaw.tcName, " = $1"
      ]

-- | Parallel-arrays payload for 'bulkInsertAddressesStmt'. Each list
-- holds one column's values and must be the same length as the others;
-- @baiIds[i]@, @baiAddresses[i]@, etc. together describe one row.
data BulkAddressInsert = BulkAddressInsert
  { baiIds            :: ![Int64]
  , baiAddresses      :: ![Text]
  , baiRaws           :: ![ByteString]
  , baiHasScript      :: ![Bool]
  , baiPaymentCreds   :: ![Maybe ByteString]
  , baiStakeAddressId :: ![Maybe Int64]
  }

-- | Returns @(raw, id)@ pairs for every input raw that already exists
-- in the @address@ table; missing raws are absent from the result.
-- Probes the @raw_hash@ index, verifies the full @raw@ per row.
bulkSelectAddressIdsStmt :: Stmt.Statement [ByteString] [(ByteString, AddressId)]
bulkSelectAddressIdsStmt =
  Stmt.preparable sql (arrayParam E.bytea) decoder
  where
    decoder = D.rowList $ (,)
      <$> D.column (D.nonNullable D.bytea)
      <*> idDecoder AddressId
    sql = mconcat
      [ "SELECT a.", addressCols.acRaw.tcName
      , ", a.", addressCols.acId.tcName
      , " FROM unnest($1) AS i(raw_in)"
      , " JOIN ", table addressTableDef, " a"
      , " ON a.", addressCols.acRawHash.tcName, " = decode(md5(i.raw_in), 'hex')"
      , " WHERE a.", addressCols.acRaw.tcName, " = i.raw_in"
      ]

-- | The caller must pre-check via 'bulkSelectAddressIdsStmt' to avoid
-- violating the @raw@ UNIQUE constraint; this statement does no
-- @ON CONFLICT@ handling.
bulkInsertAddressesStmt :: Stmt.Statement BulkAddressInsert ()
bulkInsertAddressesStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder =
         (baiIds            >$< arrayParam     E.int8)
      <> (baiAddresses      >$< arrayParam     E.text)
      <> (baiRaws           >$< arrayParam     E.bytea)
      <> (baiHasScript      >$< arrayParam     E.bool)
      <> (baiPaymentCreds   >$< nullArrayParam E.bytea)
      <> (baiStakeAddressId >$< nullArrayParam E.int8)
    sql = mconcat
      [ "INSERT INTO ", table addressTableDef
      , " (", addressCols.acId.tcName
      , ", ", addressCols.acAddress.tcName
      , ", ", addressCols.acRaw.tcName
      , ", ", addressCols.acHasScript.tcName
      , ", ", addressCols.acPaymentCred.tcName
      , ", ", addressCols.acStakeAddressId.tcName, ")"
      , " SELECT * FROM unnest($1, $2, $3, $4, $5, $6)"
      ]

-- ---------------------------------------------------------------------------
-- * tx_out
-- ---------------------------------------------------------------------------

insertTxOutRowStmt :: Stmt.Statement (TxOutId, TxOut) ()
insertTxOutRowStmt =
  Stmt.preparable (insertRowSql txOutTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxOutId)
           <> (snd >$< txOutEncoder)

updateTxOutAddressIdStmt :: Stmt.Statement (AddressId, TxOutId) ()
updateTxOutAddressIdStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< idEncoder getTxOutId)
    sql = mconcat
      [ "UPDATE ", table txOutTableDef
      , " SET ", col txOutCols.tocAddressId, " = $1"
      , " WHERE ", col txOutCols.tocId, " = $2"
      ]

-- | Two parallel arrays: tx_out ids and the address id to assign to
-- each. One round-trip regardless of input size; folds an epoch's
-- worth of FK fills into one statement.
bulkUpdateTxOutAddressIdsStmt :: Stmt.Statement ([Int64], [Int64]) ()
bulkUpdateTxOutAddressIdsStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< arrayParam E.int8)    -- tx_out ids
           <> (snd >$< arrayParam E.int8)    -- address ids
    sql = mconcat
      [ "UPDATE ", table txOutTableDef
      , " SET ", col txOutCols.tocAddressId, " = u.aid"
      , " FROM unnest($1, $2) AS u(tx_out_id, aid)"
      , " WHERE ", qcol (table txOutTableDef) txOutCols.tocId, " = u.tx_out_id"
      ]

nextTxOutIdStmt :: Stmt.Statement () TxOutId
nextTxOutIdStmt = nextIdStmt txOutTableDef TxOutId

-- | Looks up @tx_out.value@ by the producing tx's hash and the output
-- index. 'Nothing' when no such output exists in the DB.
queryOutputValueStmt :: Stmt.Statement (ByteString, Word16) (Maybe DbLovelace)
queryOutputValueStmt =
  Stmt.preparable sql encoder (D.rowMaybe valueDecoder)
  where
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable (fromIntegral >$< E.int8)))
    valueDecoder = D.column (D.nonNullable dbLovelaceValueDecoder)
    sql = mconcat
      [ "SELECT ", qcol (table txOutTableDef) txOutCols.tocValue
      , " FROM ", table txOutTableDef
      , " JOIN ", table txTableDef
      , " ON ", qcol (table txTableDef) txCols.tcId
      , " = ", qcol (table txOutTableDef) txOutCols.tocTxId
      , " WHERE ", qcol (table txTableDef) txCols.tcHash, " = $1"
      , " AND ", qcol (table txOutTableDef) txOutCols.tocIndex, " = $2"
      ]

-- | Resolves @(tx.id, tx_out.id, tx_out.value)@ for a spent output
-- identified by the producing tx's hash and the output index, in one
-- round-trip. 'Nothing' on miss.
queryInputUtxoStmt :: Stmt.Statement (ByteString, Word16) (Maybe (TxId, TxOutId, DbLovelace))
queryInputUtxoStmt =
  Stmt.preparable sql encoder (D.rowMaybe rowDecoder)
  where
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable (fromIntegral >$< E.int8)))
    rowDecoder = (,,)
      <$> D.column (D.nonNullable (TxId <$> D.int8))
      <*> D.column (D.nonNullable (TxOutId <$> D.int8))
      <*> D.column (D.nonNullable dbLovelaceValueDecoder)
    sql = mconcat
      [ "SELECT ", qcol (table txTableDef) txCols.tcId
      , ", ", qcol (table txOutTableDef) txOutCols.tocId
      , ", ", qcol (table txOutTableDef) txOutCols.tocValue
      , " FROM ", table txOutTableDef
      , " JOIN ", table txTableDef
      , " ON ", qcol (table txTableDef) txCols.tcId
      , " = ", qcol (table txOutTableDef) txOutCols.tocTxId
      , " WHERE ", qcol (table txTableDef) txCols.tcHash, " = $1"
      , " AND ", qcol (table txOutTableDef) txOutCols.tocIndex, " = $2"
      ]

-- ---------------------------------------------------------------------------
-- * tx_in
-- ---------------------------------------------------------------------------

-- | @tx_out_id@ is left NULL during Ingest and resolved post-load by a
-- SQL join in 'PreparingForVolatileTail'. The Follow writer also never
-- sets it.
insertTxInRowStmt :: Stmt.Statement TxIn ()
insertTxInRowStmt =
  Stmt.preparable (insertRowSql txInTableDef) txInEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * collateral_tx_in
-- ---------------------------------------------------------------------------

insertCollateralTxInRowStmt :: Stmt.Statement CollateralTxIn ()
insertCollateralTxInRowStmt =
  Stmt.preparable (insertRowSql collateralTxInTableDef) collateralTxInEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * collateral_tx_out
-- ---------------------------------------------------------------------------

insertCollateralTxOutRowStmt :: Stmt.Statement (CollateralTxOutId, CollateralTxOut) ()
insertCollateralTxOutRowStmt =
  Stmt.preparable (insertRowSql collateralTxOutTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCollateralTxOutId)
           <> (snd >$< collateralTxOutEncoder)

nextCollateralTxOutIdStmt :: Stmt.Statement () CollateralTxOutId
nextCollateralTxOutIdStmt = nextIdStmt collateralTxOutTableDef CollateralTxOutId

updateCollateralTxOutAddressIdStmt :: Stmt.Statement (AddressId, CollateralTxOutId) ()
updateCollateralTxOutAddressIdStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< idEncoder getCollateralTxOutId)
    sql = mconcat
      [ "UPDATE ", table collateralTxOutTableDef
      , " SET ", col collateralTxOutCols.ctocAddressId, " = $1"
      , " WHERE ", col collateralTxOutCols.ctocId, " = $2"
      ]

bulkUpdateCollateralTxOutAddressIdsStmt :: Stmt.Statement ([Int64], [Int64]) ()
bulkUpdateCollateralTxOutAddressIdsStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< arrayParam E.int8)    -- collateral_tx_out ids
           <> (snd >$< arrayParam E.int8)    -- address ids
    sql = mconcat
      [ "UPDATE ", table collateralTxOutTableDef
      , " SET ", col collateralTxOutCols.ctocAddressId, " = u.aid"
      , " FROM unnest($1, $2) AS u(out_id, aid)"
      , " WHERE ", qcol (table collateralTxOutTableDef) collateralTxOutCols.ctocId, " = u.out_id"
      ]

-- ---------------------------------------------------------------------------
-- * reference_tx_in
-- ---------------------------------------------------------------------------

insertReferenceTxInRowStmt :: Stmt.Statement ReferenceTxIn ()
insertReferenceTxInRowStmt =
  Stmt.preparable (insertRowSql referenceTxInTableDef) referenceTxInEncoder D.noResult
