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

    -- * Hasql encoders \/ decoders
  , txMetadataEncoder
  , txMetadataDecoder
  , entityTxMetadataDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text.Encoding as TE
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
  ( TxId (..)
  , TxMetadataId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbWord64, dbWord64ValueDecoder, dbWord64ValueEncoder, unDbWord64)
import DbSync.Db.Loader.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key TxMetadata = TxMetadataId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @tx_metadata@ table.
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
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys =
      [ ForeignKey "tx_id" "tx" "id"
      ]
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

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | The @json@ column is @jsonb@. We round-trip via UTF-8 bytes through
-- 'E.jsonbBytes' / 'D.jsonbBytes'. The Haskell-side representation stays
-- 'Text' (the raw JSON text).
--
-- Asymmetry note: 'E.jsonbBytes' is a 'E.Value ByteString' (PG happily
-- accepts the raw JSON text we hand it). 'D.jsonbBytes' takes a parser
-- @ByteString -> Either Text a@ — we use 'TE.decodeUtf8'' and surface
-- any UTF-8 error as the parse failure. PG never stores invalid UTF-8 in
-- a @jsonb@ column, so the @Left@ branch is defensive.
jsonbTextEncoder :: E.Value Text
jsonbTextEncoder = TE.encodeUtf8 >$< E.jsonbBytes

jsonbTextDecoder :: D.Value Text
jsonbTextDecoder = D.jsonbBytes $ \bs ->
  case TE.decodeUtf8' bs of
    Right t -> Right t
    Left  e -> Left (show e)

-- | Encoder for a 'TxMetadata', excluding the auto-generated @id@.
-- Field order matches the column order in 'txMetadataTableDef'.
txMetadataEncoder :: E.Params TxMetadata
txMetadataEncoder = mconcat
  [ txMetadataKey      >$< E.param (E.nonNullable dbWord64ValueEncoder)
  , txMetadataJson     >$< E.param (E.nullable jsonbTextEncoder)
  , txMetadataBytes    >$< E.param (E.nonNullable E.bytea)
  , txMetadataTxId     >$< idEncoder getTxId
  ]

-- | Decoder for the data columns of a 'TxMetadata' (excluding @id@).
txMetadataDecoder :: D.Row TxMetadata
txMetadataDecoder = TxMetadata
  <$> D.column (D.nonNullable dbWord64ValueDecoder)
  <*> D.column (D.nullable jsonbTextDecoder)
  <*> D.column (D.nonNullable D.bytea)
  <*> idDecoder TxId

-- | Decoder for a full @tx_metadata@ row, including @id@.
entityTxMetadataDecoder :: D.Row (TxMetadataId, TxMetadata)
entityTxMetadataDecoder = (,)
  <$> idDecoder TxMetadataId
  <*> txMetadataDecoder
