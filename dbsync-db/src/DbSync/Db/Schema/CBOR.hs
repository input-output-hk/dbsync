{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the CBOR extractor table: tx_cbor.
--
-- Stores raw CBOR-encoded transaction bytes for debugging,
-- replay, and external tool integration.
module DbSync.Db.Schema.CBOR
  ( -- * Schema types
    TxCbor (..)

    -- * Table definitions
  , txCborTableDef

    -- * Column records (compile-time-safe column references)
  , TxCborCols (..)
  , txCborCols
  , txCborColsList

    -- * Per-module column-record registry
  , cborColumnRecords

    -- * COPY encoding
  , encodeTxCborCopy

    -- * Hasql encoders \/ decoders
  , txCborEncoder
  , txCborDecoder
  , entityTxCborDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Loader.Encoder (buildCopyRow, bHex, bInt64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key TxCbor = TxCborId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @tx_cbor@ table.
-- One row per transaction, storing the raw CBOR bytes.
data TxCbor = TxCbor
  { txCborTxId  :: !TxId
  , txCborBytes :: !ByteString  -- ^ Raw CBOR-encoded transaction
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

txCborTableDef :: TableDef
txCborTableDef = TableDef
  { tdName    = "tx_cbor"
  , tdColumns =
      [ ColumnDef "id"    PgBigInt False
      , ColumnDef "tx_id" PgBigInt False
      , ColumnDef "bytes" PgBytea  False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdParentRefs =
      [ ParentRef "tx_id" "tx" "id"
      ]
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data TxCborCols = TxCborCols
  { tcbId    :: !TableColumn
  , tcbTxId  :: !TableColumn
  , tcbBytes :: !TableColumn
  }

txCborCols :: TxCborCols
txCborCols =
  let c = TableColumn txCborTableDef
  in TxCborCols
       { tcbId    = c "id"
       , tcbTxId  = c "tx_id"
       , tcbBytes = c "bytes"
       }

txCborColsList :: [TableColumn]
txCborColsList =
  [ txCborCols.tcbId
  , txCborCols.tcbTxId
  , txCborCols.tcbBytes
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

cborColumnRecords :: [(TableDef, [TableColumn])]
cborColumnRecords =
  [ (txCborTableDef, txCborColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeTxCborCopy :: TxCbor -> ByteString
encodeTxCborCopy tc =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ txCborTxId tc)
    , Just $ bHex (txCborBytes tc)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

txCborEncoder :: E.Params TxCbor
txCborEncoder = mconcat
  [ txCborTxId  >$< idEncoder getTxId
  , txCborBytes >$< E.param (E.nonNullable E.bytea)
  ]

txCborDecoder :: D.Row TxCbor
txCborDecoder = TxCbor
  <$> idDecoder TxId
  <*> D.column (D.nonNullable D.bytea)

entityTxCborDecoder :: D.Row (TxCborId, TxCbor)
entityTxCborDecoder = (,)
  <$> idDecoder TxCborId
  <*> txCborDecoder
