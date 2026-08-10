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

    -- * Column records (compile-time-safe column references)
  , MultiAssetCols (..)
  , multiAssetCols
  , multiAssetColsList
  , MaTxMintCols (..)
  , maTxMintCols
  , maTxMintColsList
  , MaTxOutCols (..)
  , maTxOutCols
  , maTxOutColsList

    -- * Per-module column-record registry
  , multiAssetColumnRecords

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
import DbSync.Db.Loader.Encoder (buildCopyRow, bHex, bInt64, bText, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key MultiAsset = MultiAssetId
type instance Key MaTxMint = MaTxMintId
type instance Key MaTxOut = MaTxOutId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | One row per unique (policy, name) pair.
data MultiAsset = MultiAsset
  { multiAssetPolicy      :: !ByteString  -- ^ Policy ID (28 bytes)
  , multiAssetName        :: !ByteString  -- ^ Asset name (0-32 bytes)
  , multiAssetFingerprint :: !Text        -- ^ CIP-14 fingerprint
  }
  deriving stock (Eq, Show)

-- | Tracks minting/burning events per transaction.
data MaTxMint = MaTxMint
  { maTxMintQuantity :: !Integer   -- ^ Signed quantity (positive=mint, negative=burn)
  , maTxMintTxId     :: !TxId     -- ^ FK to tx
  , maTxMintIdent    :: !MultiAssetId -- ^ FK to multi_asset
  }
  deriving stock (Eq, Show)

-- | Tracks multi-asset quantities attached to transaction outputs.
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
    -- Unique by (policy, name). The Follow dedup resolver SELECTs on
    -- this pair for every unique multi-asset per block; without the
    -- index every resolve sequential-scans the whole table.
  , tdUniqueConstraints = ["policy" :| ["name"]]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdParentRefs = []
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdParentRefs =
      [ ParentRef "tx_id" "tx" "id"
      ]
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
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = ["id"]
  , tdParentRefs =
      [ ParentRef "tx_out_id" "tx_out" "id"
      ]
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data MultiAssetCols = MultiAssetCols
  { macId          :: !TableColumn
  , macPolicy      :: !TableColumn
  , macName        :: !TableColumn
  , macFingerprint :: !TableColumn
  }

multiAssetCols :: MultiAssetCols
multiAssetCols =
  let c = TableColumn multiAssetTableDef
  in MultiAssetCols
       { macId          = c "id"
       , macPolicy      = c "policy"
       , macName        = c "name"
       , macFingerprint = c "fingerprint"
       }

multiAssetColsList :: [TableColumn]
multiAssetColsList =
  [ multiAssetCols.macId
  , multiAssetCols.macPolicy
  , multiAssetCols.macName
  , multiAssetCols.macFingerprint
  ]

data MaTxMintCols = MaTxMintCols
  { mtmcId       :: !TableColumn
  , mtmcQuantity :: !TableColumn
  , mtmcTxId     :: !TableColumn
  , mtmcIdent    :: !TableColumn
  }

maTxMintCols :: MaTxMintCols
maTxMintCols =
  let c = TableColumn maTxMintTableDef
  in MaTxMintCols
       { mtmcId       = c "id"
       , mtmcQuantity = c "quantity"
       , mtmcTxId     = c "tx_id"
       , mtmcIdent    = c "ident"
       }

maTxMintColsList :: [TableColumn]
maTxMintColsList =
  [ maTxMintCols.mtmcId
  , maTxMintCols.mtmcQuantity
  , maTxMintCols.mtmcTxId
  , maTxMintCols.mtmcIdent
  ]

data MaTxOutCols = MaTxOutCols
  { mtocId       :: !TableColumn
  , mtocQuantity :: !TableColumn
  , mtocTxOutId  :: !TableColumn
  , mtocIdent    :: !TableColumn
  }

maTxOutCols :: MaTxOutCols
maTxOutCols =
  let c = TableColumn maTxOutTableDef
  in MaTxOutCols
       { mtocId       = c "id"
       , mtocQuantity = c "quantity"
       , mtocTxOutId  = c "tx_out_id"
       , mtocIdent    = c "ident"
       }

maTxOutColsList :: [TableColumn]
maTxOutColsList =
  [ maTxOutCols.mtocId
  , maTxOutCols.mtocQuantity
  , maTxOutCols.mtocTxOutId
  , maTxOutCols.mtocIdent
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

multiAssetColumnRecords :: [(TableDef, [TableColumn])]
multiAssetColumnRecords =
  [ (multiAssetTableDef, multiAssetColsList)
  , (maTxMintTableDef,   maTxMintColsList)
  , (maTxOutTableDef,    maTxOutColsList)
  ]

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

encodeMaTxMintCopy :: MaTxMint -> ByteString
encodeMaTxMintCopy m =
  buildCopyRow
    [ Just $ bInteger (maTxMintQuantity m)
    , Just $ bInt64 (getTxId $ maTxMintTxId m)
    , Just $ bInt64 (getMultiAssetId $ maTxMintIdent m)
    ]

encodeMaTxOutCopy :: MaTxOut -> ByteString
encodeMaTxOutCopy m =
  buildCopyRow
    [ Just $ bWord64 (unDbWord64 $ maTxOutQuantity m)
    , Just $ bInt64 (getTxOutId $ maTxOutTxOutId m)
    , Just $ bInt64 (getMultiAssetId $ maTxOutIdent m)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------
--
-- A @\<row>Encoder@ and @\<row>Decoder@ pair omits the @id@ column. An
-- @entity\<Row>Decoder@ reads @id@ first, so its column order matches
-- @SELECT *@ on the table.

-- | Encoder/decoder for a signed 'Integer' over PostgreSQL @numeric@.
-- Mints / burns can in principle exceed @int8@ range, so we route through
-- 'Sci.Scientific'. Mint quantities are always whole numbers, so 'floor'
-- on the decode side is exact.
integerAsNumericEncoder :: E.Value Integer
integerAsNumericEncoder = fromInteger >$< E.numeric

integerAsNumericDecoder :: D.Value Integer
integerAsNumericDecoder = floor <$> D.numeric

multiAssetEncoder :: E.Params MultiAsset
multiAssetEncoder = mconcat
  [ multiAssetPolicy      >$< E.param (E.nonNullable E.bytea)
  , multiAssetName        >$< E.param (E.nonNullable E.bytea)
  , multiAssetFingerprint >$< E.param (E.nonNullable E.text)
  ]

multiAssetDecoder :: D.Row MultiAsset
multiAssetDecoder = MultiAsset
  <$> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.text)

entityMultiAssetDecoder :: D.Row (MultiAssetId, MultiAsset)
entityMultiAssetDecoder = (,)
  <$> idDecoder MultiAssetId
  <*> multiAssetDecoder

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
