{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the MultiAsset extractor tables: multi_asset,
-- ma_tx_mint, ma_tx_out.
module DbSync.Db.Schema.MultiAsset
  ( -- * Schema types
    MultiAsset (..)
  , MaTxMint (..)
  , MaTxOut (..)

    -- * Table definitions
  , multiAssetTableDef
  , maTxMintTableDef
  , maTxOutTableDef

    -- * COPY encoding
  , encodeMultiAssetCopy
  , encodeMaTxMintCopy
  , encodeMaTxOutCopy

    -- * Hasql encoders \/ decoders
  , multiAssetEncoder
  , multiAssetDecoder
  , entityMultiAssetDecoder
  , maTxMintEncoder
  , maTxMintDecoder
  , entityMaTxMintDecoder
  , maTxOutEncoder
  , maTxOutDecoder
  , entityMaTxOutDecoder
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbWord64, dbWord64ValueDecoder, dbWord64ValueEncoder, unDbWord64)
import DbSync.Db.Writer.Copy.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key MultiAsset = MultiAssetId
type instance Key MaTxMint = MaTxMintId
type instance Key MaTxOut = MaTxOutId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @multi_asset@ table.
-- One row per unique (policy, name) pair.
data MultiAsset = MultiAsset
  { multiAssetPolicy      :: !ByteString  -- ^ Policy ID (28 bytes)
  , multiAssetName        :: !ByteString  -- ^ Asset name (0-32 bytes)
  , multiAssetFingerprint :: !Text        -- ^ CIP-14 fingerprint
  }
  deriving stock (Eq, Show)

-- | The @ma_tx_mint@ table.
-- Tracks minting/burning events per transaction.
data MaTxMint = MaTxMint
  { maTxMintQuantity :: !Integer   -- ^ Signed quantity (positive=mint, negative=burn)
  , maTxMintTxId     :: !TxId     -- ^ FK to tx
  , maTxMintIdent    :: !MultiAssetId -- ^ FK to multi_asset
  }
  deriving stock (Eq, Show)

-- | The @ma_tx_out@ table.
-- Tracks multi-asset quantities attached to transaction outputs.
data MaTxOut = MaTxOut
  { maTxOutQuantity :: !DbWord64       -- ^ Unsigned quantity
  , maTxOutTxOutId  :: !TxOutId       -- ^ FK to tx_out
  , maTxOutIdent    :: !MultiAssetId  -- ^ FK to multi_asset
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

multiAssetTableDef :: TableDef
multiAssetTableDef = TableDef
  { tdName    = "multi_asset"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt  False
      , ColumnDef "policy"      PgBytea   False
      , ColumnDef "name"        PgBytea   False
      , ColumnDef "fingerprint" PgText    False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

maTxMintTableDef :: TableDef
maTxMintTableDef = TableDef
  { tdName    = "ma_tx_mint"
  , tdColumns =
      [ ColumnDef "id"       PgBigInt   False
      , ColumnDef "quantity" PgNumeric  False
      , ColumnDef "tx_id"   PgBigInt   False
      , ColumnDef "ident"   PgBigInt   False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

maTxOutTableDef :: TableDef
maTxOutTableDef = TableDef
  { tdName    = "ma_tx_out"
  , tdColumns =
      [ ColumnDef "id"        PgBigInt   False
      , ColumnDef "quantity"  PgNumeric  False
      , ColumnDef "tx_out_id" PgBigInt   False
      , ColumnDef "ident"     PgBigInt   False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeMultiAssetCopy :: MultiAssetId -> MultiAsset -> ByteString
encodeMultiAssetCopy (MultiAssetId mid) ma =
  buildCopyRow
    [ Just $ bInt64 mid
    , Just $ bHex (multiAssetPolicy ma)
    , Just $ bHex (multiAssetName ma)
    , Just $ bText (multiAssetFingerprint ma)
    ]

encodeMaTxMintCopy :: MaTxMintId -> MaTxMint -> ByteString
encodeMaTxMintCopy (MaTxMintId mid) m =
  buildCopyRow
    [ Just $ bInt64 mid
    , Just $ bInteger (maTxMintQuantity m)
    , Just $ bInt64 (getTxId $ maTxMintTxId m)
    , Just $ bInt64 (getMultiAssetId $ maTxMintIdent m)
    ]

encodeMaTxOutCopy :: MaTxOutId -> MaTxOut -> ByteString
encodeMaTxOutCopy (MaTxOutId mid) m =
  buildCopyRow
    [ Just $ bInt64 mid
    , Just $ bWord64 (unDbWord64 $ maTxOutQuantity m)
    , Just $ bInt64 (getTxOutId $ maTxOutTxOutId m)
    , Just $ bInt64 (getMultiAssetId $ maTxOutIdent m)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | Encoder/decoder for a signed 'Integer' over PostgreSQL @numeric@.
-- Mints / burns can in principle exceed @int8@ range, so we route through
-- 'Sci.Scientific'. Mint quantities are always whole numbers, so 'floor'
-- on the decode side is exact.
integerAsNumericEncoder :: E.Value Integer
integerAsNumericEncoder = fromInteger >$< E.numeric

integerAsNumericDecoder :: D.Value Integer
integerAsNumericDecoder = floor <$> D.numeric

-- | Encoder for a 'MultiAsset', excluding the auto-generated @id@.
multiAssetEncoder :: E.Params MultiAsset
multiAssetEncoder = mconcat
  [ multiAssetPolicy      >$< E.param (E.nonNullable E.bytea)
  , multiAssetName        >$< E.param (E.nonNullable E.bytea)
  , multiAssetFingerprint >$< E.param (E.nonNullable E.text)
  ]

-- | Decoder for the data columns of a 'MultiAsset' (excluding @id@).
multiAssetDecoder :: D.Row MultiAsset
multiAssetDecoder = MultiAsset
  <$> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.text)

entityMultiAssetDecoder :: D.Row (MultiAssetId, MultiAsset)
entityMultiAssetDecoder = (,)
  <$> idDecoder MultiAssetId
  <*> multiAssetDecoder

-- | Encoder for a 'MaTxMint', excluding the auto-generated @id@.
maTxMintEncoder :: E.Params MaTxMint
maTxMintEncoder = mconcat
  [ maTxMintQuantity >$< E.param (E.nonNullable integerAsNumericEncoder)
  , maTxMintTxId     >$< idEncoder getTxId
  , maTxMintIdent    >$< idEncoder getMultiAssetId
  ]

maTxMintDecoder :: D.Row MaTxMint
maTxMintDecoder = MaTxMint
  <$> D.column (D.nonNullable integerAsNumericDecoder)
  <*> idDecoder TxId
  <*> idDecoder MultiAssetId

entityMaTxMintDecoder :: D.Row (MaTxMintId, MaTxMint)
entityMaTxMintDecoder = (,)
  <$> idDecoder MaTxMintId
  <*> maTxMintDecoder

-- | Encoder for an 'MaTxOut', excluding the auto-generated @id@.
maTxOutEncoder :: E.Params MaTxOut
maTxOutEncoder = mconcat
  [ maTxOutQuantity    >$< E.param (E.nonNullable dbWord64ValueEncoder)
  , maTxOutTxOutId     >$< idEncoder getTxOutId
  , maTxOutIdent       >$< idEncoder getMultiAssetId
  ]

maTxOutDecoder :: D.Row MaTxOut
maTxOutDecoder = MaTxOut
  <$> D.column (D.nonNullable dbWord64ValueDecoder)
  <*> idDecoder TxOutId
  <*> idDecoder MultiAssetId

entityMaTxOutDecoder :: D.Row (MaTxOutId, MaTxOut)
entityMaTxOutDecoder = (,)
  <$> idDecoder MaTxOutId
  <*> maTxOutDecoder

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Encode a signed 'Integer' as decimal ASCII into a 'Builder'.
bInteger :: Integer -> Builder
bInteger = byteString . BS8.pack . show
