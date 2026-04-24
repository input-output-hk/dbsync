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
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (DbWord64 (..))
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
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Encode a signed 'Integer' as decimal ASCII into a 'Builder'.
bInteger :: Integer -> Builder
bInteger = byteString . BS8.pack . show
