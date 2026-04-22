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

    -- * COPY encoding
  , encodeTxCborCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Core (encodeHex, encodeInt64)
import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Writer.Copy.Encoder (encodeToCopyRow)

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
  , tdMode = TableUnlogged
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeTxCborCopy :: TxCborId -> TxCbor -> ByteString
encodeTxCborCopy (TxCborId tcid) tc =
  encodeToCopyRow
    [ Just $ encodeInt64 tcid
    , Just $ encodeInt64 (getTxId $ txCborTxId tc)
    , Just $ encodeHex (txCborBytes tc)
    ]
