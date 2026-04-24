{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : DbSync.Db.Schema.Core
Description : Schema types for the Core extractor tables: block, tx, slot_leader.

Ported from @Cardano.Db.Schema.Core.Base@ in the original cardano-db-sync.
These are the __single canonical Haskell representation__ of each database
table — used for COPY encoding during 'IngestChainHistory' and (later)
for hasql INSERT\/SELECT during 'FollowingChainTip'.

COPY encoding functions ('encodeBlockCopy', 'encodeTxCopy',
'encodeSlotLeaderCopy') are new — the original project used UNNEST,
which we replace entirely with the PostgreSQL COPY protocol.
-}
module DbSync.Db.Schema.Core
  ( -- * Schema types
    Block (..)
  , Tx (..)
  , SlotLeader (..)

    -- * Table definitions (for DDL generation)
  , blockTableDef
  , txTableDef
  , slotLeaderTableDef

    -- * COPY encoding
  , encodeBlockCopy
  , encodeTxCopy
  , encodeSlotLeaderCopy

    -- * Internal encoding helpers (exported for testing)
  , encodeInt64
  , encodeWord64
  , encodeBool
  , encodeHex
  , encodeUTCTime
  ) where

import Cardano.Prelude

import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import qualified Data.ByteString.Char8 as BS8

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids (BlockId (..), PoolHashId (..), SlotLeaderId (..), TxId (..))
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..))
import DbSync.Db.Writer.Copy.Encoder
  ( buildCopyRow
  , bBool, bHex, bInt64, bText, bUTCTime, bWord16, bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key Block = BlockId
type instance Key Tx = TxId
type instance Key SlotLeader = SlotLeaderId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @block@ table.
-- Matches @Cardano.Db.Schema.Core.Base.Block@ from the original project.
--
-- Note: the @id@ column is NOT part of this record — it lives in
-- @'Key' Block = 'BlockId'@, paired via 'Entity'.
data Block = Block
  { blockHash           :: !ByteString       -- ^ hash32type
  , blockEpochNo        :: !(Maybe Word64)   -- ^ word31type
  , blockSlotNo         :: !(Maybe Word64)   -- ^ word63type
  , blockEpochSlotNo    :: !(Maybe Word64)   -- ^ word31type
  , blockBlockNo        :: !(Maybe Word64)   -- ^ word31type
  , blockPreviousId     :: !(Maybe BlockId)  -- ^ FK to block (noreference)
  , blockSlotLeaderId   :: !SlotLeaderId     -- ^ FK to slot_leader (noreference)
  , blockSize           :: !Word64           -- ^ word31type
  , blockTime           :: !UTCTime          -- ^ timestamp
  , blockTxCount        :: !Word64
  , blockProtoMajor     :: !Word16           -- ^ word31type
  , blockProtoMinor     :: !Word16           -- ^ word31type
  , blockVrfKey         :: !(Maybe Text)     -- ^ Shelley+
  , blockOpCert         :: !(Maybe ByteString) -- ^ hash32type, Shelley+
  , blockOpCertCounter  :: !(Maybe Word64)   -- ^ hash63type, Shelley+
  }
  deriving stock (Eq, Show)

-- | The @tx@ table.
-- Matches @Cardano.Db.Schema.Core.Base.Tx@ from the original project.
data Tx = Tx
  { txHash              :: !ByteString       -- ^ hash32type
  , txBlockId           :: !BlockId          -- ^ FK to block (noreference)
  , txBlockIndex        :: !Word64           -- ^ word31type — index within the block
  , txOutSum            :: !DbLovelace       -- ^ lovelace
  , txFee               :: !DbLovelace       -- ^ lovelace
  , txDeposit           :: !(Maybe Int64)    -- ^ allows negative values
  , txSize              :: !Word64           -- ^ word31type
  , txInvalidBefore     :: !(Maybe DbWord64) -- ^ word64type — Allegra+
  , txInvalidHereafter  :: !(Maybe DbWord64) -- ^ word64type — Allegra+
  , txValidContract     :: !Bool             -- ^ Alonzo+: False if script fails phase 2
  , txScriptSize        :: !Word64           -- ^ word31type — Alonzo+
  , txTreasuryDonation  :: !DbLovelace       -- ^ lovelace — Conway+, default 0
  }
  deriving stock (Eq, Show)

-- | The @slot_leader@ table.
-- Matches @Cardano.Db.Schema.Core.Base.SlotLeader@ from the original project.
data SlotLeader = SlotLeader
  { slotLeaderHash        :: !ByteString       -- ^ hash28type
  , slotLeaderPoolHashId  :: !(Maybe PoolHashId) -- ^ non-null when block mined by pool
  , slotLeaderDescription :: !Text             -- ^ description of the slot leader
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

-- | Table definition for the @block@ table.
-- Created as UNLOGGED during 'IngestChainHistory'.
blockTableDef :: TableDef
blockTableDef = TableDef
  { tdName    = "block"
  , tdColumns =
      [ ColumnDef "id"              PgBigInt    False
      , ColumnDef "hash"            PgBytea     False
      , ColumnDef "epoch_no"        PgBigInt    True
      , ColumnDef "slot_no"         PgBigInt    True
      , ColumnDef "epoch_slot_no"   PgBigInt    True
      , ColumnDef "block_no"        PgBigInt    True
      , ColumnDef "previous_id"     PgBigInt    True
      , ColumnDef "slot_leader_id"  PgBigInt    False
      , ColumnDef "size"            PgBigInt    False
      , ColumnDef "time"            PgTimestamp  False
      , ColumnDef "tx_count"        PgBigInt    False
      , ColumnDef "proto_major"     PgSmallInt  False
      , ColumnDef "proto_minor"     PgSmallInt  False
      , ColumnDef "vrf_key"         PgText      True
      , ColumnDef "op_cert"         PgBytea     True
      , ColumnDef "op_cert_counter" PgBigInt    True
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

-- | Table definition for the @tx@ table.
txTableDef :: TableDef
txTableDef = TableDef
  { tdName    = "tx"
  , tdColumns =
      [ ColumnDef "id"                PgBigInt    False
      , ColumnDef "hash"              PgBytea     False
      , ColumnDef "block_id"          PgBigInt    False
      , ColumnDef "block_index"       PgBigInt    False
      , ColumnDef "out_sum"           PgNumeric   False
      , ColumnDef "fee"               PgNumeric   False
      , ColumnDef "deposit"           PgBigInt    True
      , ColumnDef "size"              PgBigInt    False
      , ColumnDef "invalid_before"    PgNumeric   True
      , ColumnDef "invalid_hereafter" PgNumeric   True
      , ColumnDef "valid_contract"    PgBoolean   False
      , ColumnDef "script_size"       PgBigInt    False
      , ColumnDef "treasury_donation" PgNumeric   False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

-- | Table definition for the @slot_leader@ table.
slotLeaderTableDef :: TableDef
slotLeaderTableDef = TableDef
  { tdName    = "slot_leader"
  , tdColumns =
      [ ColumnDef "id"           PgBigInt  False
      , ColumnDef "hash"         PgBytea   False
      , ColumnDef "pool_hash_id" PgBigInt  True
      , ColumnDef "description"  PgText    False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

-- | Encode a 'Block' with its 'BlockId' into a COPY text row.
-- The ID is prepended as the first column.
encodeBlockCopy :: BlockId -> Block -> ByteString
encodeBlockCopy (BlockId bid) blk =
  buildCopyRow
    [ Just $ bInt64 bid
    , Just $ bHex (blockHash blk)
    , bWord64 <$> blockEpochNo blk
    , bWord64 <$> blockSlotNo blk
    , bWord64 <$> blockEpochSlotNo blk
    , bWord64 <$> blockBlockNo blk
    , bInt64 . getBlockId <$> blockPreviousId blk
    , Just $ bInt64 (getSlotLeaderId $ blockSlotLeaderId blk)
    , Just $ bWord64 (blockSize blk)
    , Just $ bUTCTime (blockTime blk)
    , Just $ bWord64 (blockTxCount blk)
    , Just $ bWord16 (blockProtoMajor blk)
    , Just $ bWord16 (blockProtoMinor blk)
    , bText <$> blockVrfKey blk
    , bHex <$> blockOpCert blk
    , bWord64 <$> blockOpCertCounter blk
    ]

-- | Encode a 'Tx' with its 'TxId' into a COPY text row.
encodeTxCopy :: TxId -> Tx -> ByteString
encodeTxCopy (TxId tid) tx =
  buildCopyRow
    [ Just $ bInt64 tid
    , Just $ bHex (txHash tx)
    , Just $ bInt64 (getBlockId $ txBlockId tx)
    , Just $ bWord64 (txBlockIndex tx)
    , Just $ bWord64 (unDbLovelace $ txOutSum tx)
    , Just $ bWord64 (unDbLovelace $ txFee tx)
    , bInt64 <$> txDeposit tx
    , Just $ bWord64 (txSize tx)
    , bWord64 . unDbWord64 <$> txInvalidBefore tx
    , bWord64 . unDbWord64 <$> txInvalidHereafter tx
    , Just $ bBool (txValidContract tx)
    , Just $ bWord64 (txScriptSize tx)
    , Just $ bWord64 (unDbLovelace $ txTreasuryDonation tx)
    ]

-- | Encode a 'SlotLeader' with its 'SlotLeaderId' into a COPY text row.
encodeSlotLeaderCopy :: SlotLeaderId -> SlotLeader -> ByteString
encodeSlotLeaderCopy (SlotLeaderId slid) sl =
  buildCopyRow
    [ Just $ bInt64 slid
    , Just $ bHex (slotLeaderHash sl)
    , bInt64 . getPoolHashId <$> slotLeaderPoolHashId sl
    , Just $ bText (slotLeaderDescription sl)
    ]

-- ---------------------------------------------------------------------------
-- * Internal encoding helpers
-- ---------------------------------------------------------------------------

-- | Encode an 'Int64' as a decimal ASCII string.
encodeInt64 :: Int64 -> ByteString
encodeInt64 = BS8.pack . show

-- | Encode a 'Word64' as a decimal ASCII string.
encodeWord64 :: Word64 -> ByteString
encodeWord64 = BS8.pack . show

-- | Encode a 'Word16' as a decimal ASCII string.
encodeWord16 :: Word16 -> ByteString
encodeWord16 = BS8.pack . show

-- | Encode a 'Bool' as @t@ or @f@ (PostgreSQL COPY boolean format).
encodeBool :: Bool -> ByteString
encodeBool True  = "t"
encodeBool False = "f"

-- | Encode a 'ByteString' as a hex string with @\\x@ prefix
-- (PostgreSQL bytea hex format for COPY).
encodeHex :: ByteString -> ByteString
encodeHex bs = "\\x" <> toHex bs
  where
    toHex :: ByteString -> ByteString
    toHex = BS8.concatMap (\w -> BS8.pack (hexByte w))

    hexByte :: Char -> [Char]
    hexByte c =
      let n = fromEnum c
          hi = n `div` 16
          lo = n `mod` 16
      in [hexDigit hi, hexDigit lo]

    hexDigit :: Int -> Char
    hexDigit n
      | n < 10    = toEnum (n + fromEnum '0')
      | otherwise = toEnum (n - 10 + fromEnum 'a')

-- | Encode a 'UTCTime' as a PostgreSQL timestamp string.
-- Format: @YYYY-MM-DD HH:MM:SS@ (no timezone — PostgreSQL timestamp
-- without time zone).
encodeUTCTime :: UTCTime -> ByteString
encodeUTCTime = BS8.pack . formatTime defaultTimeLocale "%F %T"
