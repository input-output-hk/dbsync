{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the Metadata extractor table: tx_metadata.
module DbSync.Db.Schema.Metadata
  ( -- * Schema types
    TxMetadata (..)

    -- * Table definitions
  , txMetadataTableDef

    -- * COPY encoding
  , encodeTxMetadataCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids (TxId (..), TxMetadataId (..))
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Db.Writer.Copy.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key TxMetadata = TxMetadataId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @tx_metadata@ table.
-- Ported from @Cardano.Db.Schema.Core.Base.TxMetadata@.
data TxMetadata = TxMetadata
  { txMetadataKey   :: !DbWord64         -- ^ Metadata key (integer)
  , txMetadataJson  :: !(Maybe Text)     -- ^ JSON representation (nullable)
  , txMetadataBytes :: !ByteString       -- ^ Raw CBOR bytes
  , txMetadataTxId  :: !TxId             -- ^ FK to tx
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

txMetadataTableDef :: TableDef
txMetadataTableDef = TableDef
  { tdName    = "tx_metadata"
  , tdColumns =
      [ ColumnDef "id"    PgBigInt  False
      , ColumnDef "key"   PgNumeric False
      , ColumnDef "json"  PgJsonb   True
      , ColumnDef "bytes" PgBytea   False
      , ColumnDef "tx_id" PgBigInt  False
      ]
  , tdMode = TableUnlogged
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeTxMetadataCopy :: TxMetadataId -> TxMetadata -> ByteString
encodeTxMetadataCopy (TxMetadataId mid) md =
  buildCopyRow
    [ Just $ bInt64 mid
    , Just $ bWord64 (unDbWord64 $ txMetadataKey md)
    , bText <$> txMetadataJson md
    , Just $ bHex (txMetadataBytes md)
    , Just $ bInt64 (getTxId $ txMetadataTxId md)
    ]
