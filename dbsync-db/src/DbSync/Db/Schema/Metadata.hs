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

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

import DbSync.Db.Schema.Core (encodeHex, encodeInt64, encodeWord64)
import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids (TxId (..), TxMetadataId (..))
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Db.Writer.Copy.Encoder (encodeToCopyRow)

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
  encodeToCopyRow
    [ Just $ encodeInt64 mid
    , Just $ encodeWord64 (unDbWord64 $ txMetadataKey md)
    , TE.encodeUtf8 <$> txMetadataJson md
    , Just $ encodeHex (txMetadataBytes md)
    , Just $ encodeInt64 (getTxId $ txMetadataTxId md)
    ]
